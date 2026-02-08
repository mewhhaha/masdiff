{-# LANGUAGE BangPatterns #-}

module MSDF.MSDF
  ( MSDFConfig(..)
  , AtlasConfig(..)
  , OutlineConfig(..)
  , EdgeColoringConfig(..)
  , ColoringStrategy(..)
  , DistanceConfig(..)
  , FillRule(..)
  , SignMode(..)
  , CorrectionConfig(..)
  , GlyphSet(..)
  , GlyphCache(..)
  , GlyphCacheLazy(..)
  , defaultMSDFConfig
  , effectiveParallelism
  , prepareGlyphCache
  , prepareGlyphCacheLazy
  , renderGlyphMSDF
  , renderGlyphMSDFCached
  , renderGlyphMSDFCachedLazy
  , glyphMetricsOnly
  , glyphMetricsOnlyAt
  ) where

import Control.Monad (forM, forM_, foldM, when)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import Data.Array (Array, array, accumArray)
import Data.Array.IArray (bounds, (!))
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newArray, readArray, writeArray, freeze)
import Data.Array.ST (STUArray, runSTUArray, thaw)
import Data.Array.Unboxed (UArray, listArray)
import qualified Data.Array.Unboxed as UA
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List (sort, sortOn, mapAccumL)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Ord (Down(..))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8)
import MSDF.Config
import MSDF.EdgeColoring (colorEdges)
import MSDF.Distance (LineSeg, flattenEdges, windingNumber)
import MSDF.Geometry
  ( EdgeRef(..)
  , ColoredEdge(..)
  , EdgeColor(..)
  , Vec2
  , edgeStartPoint
  , edgeEndPoint
  , edgeBBox
  , edgeLengthApprox
  , vecAdd
  , vecScale
  , vecSub
  , dot
  , edgePointAt
  , edgeTangentAt
  )
import MSDF.Outline (Point(..), Edge(..), contourToEdges, edgeStartDir, edgeEndDir)
import MSDF.TTF.Parser
import MSDF.TTF.Variations (applyGvarToContours, componentDeltas, hvarDeltas, isDefaultLocation, vvarDeltas)
import MSDF.Types
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc (getNumCapabilities, getNumProcessors)

-- | Default configuration.
defaultMSDFConfig :: MSDFConfig
defaultMSDFConfig = MSDFConfig
  { pixelSize = 32
  , range = 4
  , glyphSet = GlyphSetAll
  , variations = []
  , outputFormat = BitmapMSDF
  , parallelism = 0
  , atlas = AtlasConfig
      { packAtlas = True
      , atlasPadding = 1
      , atlasMinSize = 256
      , atlasMaxSize = 4096
      , atlasPowerOfTwo = True
      , buildAtlasImage = True
      }
  , outline = OutlineConfig
      { windingFlatness = 0.02
      , contourEpsilon = 1e-6
      , normalizeOrientation = True
      , splitIntersections = True
      , preprocess = False
      }
  , coloring = EdgeColoringConfig
      { cornerAngleDeg = 3.0
      , minSegments = 3
      , conflictDistance = 1.0
      , coloringSeed = 0
      , strategy = ColoringSimple
      }
  , distance = DistanceConfig
      { pseudoDistance = True
      , gridCellSize = 0
      , signEpsilon = 1e-6
      , fillRule = FillNonZero
      , signMode = SignScanline
      , overlapSupport = True
      , overlapEpsilon = 1e-3
      }
  , correction = CorrectionConfig
      { enableCorrection = True
      , channelThreshold = 0.1
      , edgeThreshold = 1.0
      , hardThreshold = 0.05
      }
  }

msdfTraceEnabled :: Bool
msdfTraceEnabled = unsafePerformIO $ do
  v <- lookupEnv "MSDF_TRACE"
  let enabled = case fmap (map toLower) v of
        Just "1" -> True
        Just "true" -> True
        Just "yes" -> True
        Just "on" -> True
        _ -> False
  pure enabled
{-# NOINLINE msdfTraceEnabled #-}

msdfRenderTimingsEnabled :: Bool
msdfRenderTimingsEnabled = unsafePerformIO $ do
  v <- lookupEnv "MSDF_RENDER_TIMINGS"
  let enabled = case fmap (map toLower) v of
        Just "1" -> True
        Just "true" -> True
        Just "yes" -> True
        Just "on" -> True
        _ -> False
  pure enabled
{-# NOINLINE msdfRenderTimingsEnabled #-}

msdfAutoParallelism :: Int
msdfAutoParallelism = unsafePerformIO $ do
  v <- lookupEnv "MSDF_PARALLELISM"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
  case v >>= parseInt of
    Just n | n > 0 -> pure n
    _ -> do
      procs <- getNumProcessors
      caps <- getNumCapabilities
      let p = if procs > 0 then procs else 1
      let c = max 1 caps
      pure (max p c)
{-# NOINLINE msdfAutoParallelism #-}

effectiveParallelism :: Int -> Int
effectiveParallelism n
  | n > 0 = n
  | otherwise = msdfAutoParallelism

msdfSplitMaxSegs :: Int
msdfSplitMaxSegs = unsafePerformIO $ do
  v <- lookupEnv "MSDF_SPLIT_MAX_SEGS"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxSegs = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 20000
  pure maxSegs
{-# NOINLINE msdfSplitMaxSegs #-}

msdfParallelPixelsMin :: Int
msdfParallelPixelsMin = unsafePerformIO $ do
  v <- lookupEnv "MSDF_PARALLEL_PIXELS_MIN"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      threshold = case v >>= parseInt of
        Just n | n > 0 -> n
        _ -> 65536
  pure threshold
{-# NOINLINE msdfParallelPixelsMin #-}

msdfSplitGridSize :: Double
msdfSplitGridSize = unsafePerformIO $ do
  v <- lookupEnv "MSDF_SPLIT_GRID"
  let parseD s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      grid = case v >>= parseD of
        Just n | n > 0 -> n
        _ -> 0
  pure grid
{-# NOINLINE msdfSplitGridSize #-}

msdfSplitMaxPairs :: Int
msdfSplitMaxPairs = unsafePerformIO $ do
  v <- lookupEnv "MSDF_SPLIT_MAX_PAIRS"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxPairs = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 2000000
  pure maxPairs
{-# NOINLINE msdfSplitMaxPairs #-}

msdfSplitMaxBucket :: Int
msdfSplitMaxBucket = unsafePerformIO $ do
  v <- lookupEnv "MSDF_SPLIT_MAX_BUCKET"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxBucket = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 2000
  pure maxBucket
{-# NOINLINE msdfSplitMaxBucket #-}

msdfSplitGridMaxIters :: Int
msdfSplitGridMaxIters = unsafePerformIO $ do
  v <- lookupEnv "MSDF_SPLIT_GRID_MAX_ITERS"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      iters = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 6
  pure iters
{-# NOINLINE msdfSplitGridMaxIters #-}

msdfPseudoCornerDeg :: Double
msdfPseudoCornerDeg = unsafePerformIO $ do
  v <- lookupEnv "MSDF_PSEUDO_CORNER_DEG"
  let parseD s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      deg = case v >>= parseD of
        Just n | n >= 0 -> n
        _ -> 3.0
  pure deg
{-# NOINLINE msdfPseudoCornerDeg #-}

msdfOverlapMaxPairs :: Int
msdfOverlapMaxPairs = unsafePerformIO $ do
  v <- lookupEnv "MSDF_OVERLAP_MAX_PAIRS"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxPairs = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 2000000
  pure maxPairs
{-# NOINLINE msdfOverlapMaxPairs #-}

msdfOverlapMaxBucket :: Int
msdfOverlapMaxBucket = unsafePerformIO $ do
  v <- lookupEnv "MSDF_OVERLAP_MAX_BUCKET"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxBucket = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 2000
  pure maxBucket
{-# NOINLINE msdfOverlapMaxBucket #-}

msdfOverlapGridSize :: Double
msdfOverlapGridSize = unsafePerformIO $ do
  v <- lookupEnv "MSDF_OVERLAP_GRID"
  let parseD s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      grid = case v >>= parseD of
        Just n | n > 0 -> n
        _ -> 0
  pure grid
{-# NOINLINE msdfOverlapGridSize #-}

msdfOverlapMaxEdges :: Int
msdfOverlapMaxEdges = unsafePerformIO $ do
  v <- lookupEnv "MSDF_OVERLAP_MAX_EDGES"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
      maxEdges = case v >>= parseInt of
        Just n | n >= 0 -> n
        _ -> 5000
  pure maxEdges
{-# NOINLINE msdfOverlapMaxEdges #-}

msdfMaxBitmapPixels :: Int
msdfMaxBitmapPixels = unsafePerformIO $ do
  v <- lookupEnv "MSDF_MAX_BITMAP_PIXELS"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
  case v >>= parseInt of
    Just n | n >= 0 -> pure n
    _ -> pure 0
{-# NOINLINE msdfMaxBitmapPixels #-}

msdfPreprocessEnabled :: Bool
msdfPreprocessEnabled = unsafePerformIO $ do
  v <- lookupEnv "MSDF_PREPROCESS"
  let enabled = case fmap (map toLower) v of
        Just "1" -> True
        Just "true" -> True
        Just "yes" -> True
        Just "on" -> True
        _ -> False
  pure enabled
{-# NOINLINE msdfPreprocessEnabled #-}

data GlyphCache = GlyphCache
  { location :: !(Maybe VariationLocation)
  , contours :: !(Array Int [[Point]])
  } deriving (Eq, Show)

data GlyphCacheLazy = GlyphCacheLazy
  { location :: !(Maybe VariationLocation)
  , contoursRef :: !(IORef (IntMap.IntMap [[Point]]))
  }

variationLocation :: MSDFConfig -> TTF -> Maybe VariationLocation
variationLocation cfg ttf =
  case ttf.variations of
    Nothing -> Nothing
    Just vars ->
      let loc = normalizeLocation vars.fvar vars.avar cfg.variations
      in if isDefaultLocation loc then Nothing else Just loc

renderGlyphMSDF :: MSDFConfig -> TTF -> Int -> GlyphMSDF
renderGlyphMSDF cfg ttf glyphIndex =
  let loc = variationLocation cfg ttf
      (rawContours, phantom) = glyphContoursAtVar loc ttf glyphIndex 0
      contours = normalizeContours cfg.outline rawContours
  in renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours (Just phantom)

renderGlyphMSDFWithContours :: Maybe VariationLocation -> MSDFConfig -> TTF -> Int -> [[Point]] -> Maybe (Double, Double, Double, Double) -> GlyphMSDF
renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours phantomMaybe =
  let phantom = case phantomMaybe of
        Just v -> v
        Nothing -> phantomDeltas loc ttf glyphIndex
      base =
        if null contours
        then glyphMetricsOnlyAt loc cfg ttf glyphIndex
        else glyphMetricsFromContours loc cfg ttf glyphIndex contours phantom
      bbox = base.bbox
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      safeRange = max 1 cfg.range
      traceEnabled = msdfTraceEnabled
      traceStage msg v = if traceEnabled then trace msg v else v
  in if null contours
     then base
      else
        let padding0 = fromIntegral (safeRange + 1) :: Double
            width0 = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding0))
            height0 = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding0))
            area0 = width0 * height0
            maxPixels0 = msdfMaxBitmapPixels
        in if maxPixels0 > 0 && area0 > maxPixels0
           then traceStage
             ("msdf: bitmap size exceeds MSDF_MAX_BITMAP_PIXELS=" <> show maxPixels0 <> ", skipping render")
             base
           else
             let contoursScaled = traceStage "msdf: scale contours" (map (map (scalePoint scale)) contours)
                 eps = cfg.outline.contourEpsilon
                 edgesByContour0 = traceStage "msdf: contour->edges" (map (filterDegenerateEdges eps . contourToEdges) contoursScaled)
                 edgesByContour1 =
                   traceStage "msdf: split intersections" $
                     if cfg.outline.splitIntersections
                     then splitContoursIntersections cfg.outline edgesByContour0
                     else edgesByContour0
                 edgesByContour2 =
                   traceStage "msdf: preprocess contours" $
                     if cfg.outline.splitIntersections && (cfg.outline.preprocess || msdfPreprocessEnabled)
                     then preprocessContours cfg.outline cfg.distance.fillRule edgesByContour1
                     else edgesByContour1
                 edgesByContour = map (filterDegenerateEdges eps) edgesByContour2
                 edgeInfosByContour0 = traceStage "msdf: build edge infos" (buildEdgeInfos cfg.outline edgesByContour)
                 allInfos0 = concat edgeInfosByContour0
                 edgeInfosByContour =
                   if cfg.distance.overlapSupport
                   then
                     let overlapFlags = traceStage "msdf: compute overlaps" (computeEdgeOverlaps cfg.outline.windingFlatness cfg.outline.contourEpsilon allInfos0)
                     in traceStage "msdf: apply overlap flags" (applyOverlapFlagsByOrder edgeInfosByContour0 overlapFlags)
                   else edgeInfosByContour0
                 coloredEdges = traceStage "msdf: edge coloring" (colorEdges cfg.coloring edgesByContour)
                 coloredInfos0 = traceStage "msdf: attach edge info" (attachEdgeInfo edgeInfosByContour coloredEdges)
                 allEdges0 = [ info | (_, info) <- coloredInfos0 ]
                 segsAll = traceStage "msdf: flatten edges all" (flattenEdges cfg.outline.windingFlatness [ info.edge | info <- allEdges0 ])
                 segsByContour = traceStage "msdf: flatten edges by contour" (map (flattenEdges cfg.outline.windingFlatness) edgesByContour)
                 coloredInfos =
                  if cfg.distance.overlapSupport
                   then
                     let filtered =
                           filterBoundaryEdges
                             cfg.distance.fillRule
                             cfg.distance.overlapEpsilon
                             cfg.outline.windingFlatness
                             segsAll
                             segsByContour
                             False
                             coloredInfos0
                     in if null filtered then coloredInfos0 else filtered
                   else coloredInfos0
                 segsSignFinal = segsAll
             in if null coloredInfos
                then base
                else
                  let padding = fromIntegral (safeRange + 1) :: Double
                      width = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding))
                      height = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding))
                      offsetX = bbox.xMin - padding
                      offsetY = bbox.yMin - padding
                      !_ = traceStage
                        ("msdf: bitmap size " <> show width <> "x" <> show height
                          <> " pixels=" <> show (width * height) <> " bbox=" <> show bbox)
                        ()
                  in
                    let pixels = traceStage "msdf: render bitmap" (renderBitmap cfg width height offsetX offsetY coloredInfos segsSignFinal segsByContour)
                    in GlyphMSDF
                      { index = glyphIndex
                      , codepoints = []
                      , advance = base.advance
                      , bearingX = base.bearingX
                      , bearingY = base.bearingY
                      , bbox = bbox
                      , bitmap =
                          pixels `seq`
                          MSDFBitmap
                            { width = width
                            , height = height
                            , offsetX = offsetX
                            , offsetY = offsetY
                            , format = cfg.outputFormat
                            , pixels = pixels
                            }
                      , vertical = base.vertical
                      , placement = Nothing
                      }

prepareGlyphCache :: MSDFConfig -> TTF -> GlyphCache
prepareGlyphCache cfg ttf =
  let loc = variationLocation cfg ttf
      numGlyphs = ttf.maxp.numGlyphs
      outlines = array (0, numGlyphs - 1)
        [ (i, normalizeContours cfg.outline (glyphOutlineAt loc ttf i)) | i <- [0 .. numGlyphs - 1] ]
  in GlyphCache loc outlines

prepareGlyphCacheLazy :: MSDFConfig -> TTF -> IO GlyphCacheLazy
prepareGlyphCacheLazy cfg ttf = do
  ref <- newIORef IntMap.empty
  pure (GlyphCacheLazy (variationLocation cfg ttf) ref)

renderGlyphMSDFCached :: GlyphCache -> MSDFConfig -> TTF -> Int -> GlyphMSDF
renderGlyphMSDFCached cache cfg ttf glyphIndex =
  let loc = variationLocation cfg ttf
      (lo, hi) = bounds cache.contours
  in if loc == cache.location && glyphIndex >= lo && glyphIndex <= hi
     then renderGlyphMSDFWithContours loc cfg ttf glyphIndex (cache.contours ! glyphIndex) Nothing
     else renderGlyphMSDF cfg ttf glyphIndex

renderGlyphMSDFCachedLazy :: GlyphCacheLazy -> MSDFConfig -> TTF -> Int -> IO GlyphMSDF
renderGlyphMSDFCachedLazy cache cfg ttf glyphIndex = do
  let loc = variationLocation cfg ttf
  if loc /= cache.location
    then pure (renderGlyphMSDF cfg ttf glyphIndex)
    else do
      stored <- readIORef cache.contoursRef
      case IntMap.lookup glyphIndex stored of
        Just contours ->
          pure (renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours Nothing)
        Nothing -> do
          let (rawContours, phantom) = glyphContoursAtVar loc ttf glyphIndex 0
              contours = normalizeContours cfg.outline rawContours
          modifyIORef' cache.contoursRef (IntMap.insert glyphIndex contours)
          pure (renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours (Just phantom))

glyphMetricsOnly :: MSDFConfig -> TTF -> Int -> GlyphMSDF
glyphMetricsOnly cfg ttf glyphIndex =
  let loc = variationLocation cfg ttf
  in glyphMetricsOnlyAt loc cfg ttf glyphIndex

glyphMetricsFromContours :: Maybe VariationLocation -> MSDFConfig -> TTF -> Int -> [[Point]] -> (Double, Double, Double, Double) -> GlyphMSDF
glyphMetricsFromContours loc cfg ttf glyphIndex contours phantom =
  let (xMin, yMin, xMax, yMax) = bboxFromContoursRaw contours
      metricsGlyph = case compositeMetricsGlyph ttf glyphIndex of
                       Just g | inBounds (ttf.hmtx.advances) g -> g
                       _ -> glyphIndex
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      baseAdvance = fromIntegral (safeIndex ttf.hmtx.advances metricsGlyph)
      baseLsb = fromIntegral (safeIndex ttf.hmtx.lsb metricsGlyph)
      (deltaL, deltaR, deltaT, deltaB) = phantom
      (hAdv, hLsb, _) = hvarAdjust loc ttf metricsGlyph
      advance = (baseAdvance + hAdv + (deltaR - deltaL)) * scale
      lsb = (baseLsb + hLsb + deltaL) * scale
      bearingY = fromIntegral yMax * scale
      verticalMetrics =
        case ttf.vmtx of
          Nothing -> Nothing
          Just vmtx ->
            let baseAdvanceV = fromIntegral (safeIndex vmtx.advances metricsGlyph)
                baseTsb = fromIntegral (safeIndex vmtx.tsb metricsGlyph)
                (vAdv, vTsb, _) = vvarAdjust loc ttf metricsGlyph
                advV = (baseAdvanceV + vAdv + (deltaB - deltaT)) * scale
                tsbV = (baseTsb + vTsb + deltaT) * scale
            in Just (VerticalMetrics advV tsbV)
      bbox = BBox
        { xMin = fromIntegral xMin * scale
        , yMin = fromIntegral yMin * scale
        , xMax = fromIntegral xMax * scale
        , yMax = fromIntegral yMax * scale
        }
  in GlyphMSDF
      { index = glyphIndex
      , codepoints = []
      , advance = advance
      , bearingX = lsb
      , bearingY = bearingY
      , bbox = bbox
      , bitmap = emptyBitmap cfg.outputFormat
      , vertical = verticalMetrics
      , placement = Nothing
      }

glyphMetricsOnlyAt :: Maybe VariationLocation -> MSDFConfig -> TTF -> Int -> GlyphMSDF
glyphMetricsOnlyAt loc cfg ttf glyphIndex =
  let (xMin, yMin, xMax, yMax) = glyphBBoxRawAt loc ttf glyphIndex
      metricsGlyph = case compositeMetricsGlyph ttf glyphIndex of
                       Just g | inBounds (ttf.hmtx.advances) g -> g
                       _ -> glyphIndex
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      baseAdvance = fromIntegral (safeIndex ttf.hmtx.advances metricsGlyph)
      baseLsb = fromIntegral (safeIndex ttf.hmtx.lsb metricsGlyph)
      (deltaL, deltaR, deltaT, deltaB) = phantomDeltas loc ttf metricsGlyph
      (hAdv, hLsb, _) = hvarAdjust loc ttf metricsGlyph
      advance = (baseAdvance + hAdv + (deltaR - deltaL)) * scale
      lsb = (baseLsb + hLsb + deltaL) * scale
      bearingY = fromIntegral yMax * scale
      verticalMetrics =
        case ttf.vmtx of
          Nothing -> Nothing
          Just vmtx ->
            let baseAdvanceV = fromIntegral (safeIndex vmtx.advances metricsGlyph)
                baseTsb = fromIntegral (safeIndex vmtx.tsb metricsGlyph)
                (vAdv, vTsb, _) = vvarAdjust loc ttf metricsGlyph
                advV = (baseAdvanceV + vAdv + (deltaB - deltaT)) * scale
                tsbV = (baseTsb + vTsb + deltaT) * scale
            in Just (VerticalMetrics advV tsbV)
      bbox = BBox
        { xMin = fromIntegral xMin * scale
        , yMin = fromIntegral yMin * scale
        , xMax = fromIntegral xMax * scale
        , yMax = fromIntegral yMax * scale
        }
  in GlyphMSDF
      { index = glyphIndex
      , codepoints = []
      , advance = advance
      , bearingX = lsb
      , bearingY = bearingY
      , bbox = bbox
      , bitmap = emptyBitmap cfg.outputFormat
      , vertical = verticalMetrics
      , placement = Nothing
      }

phantomDeltas :: Maybe VariationLocation -> TTF -> Int -> (Double, Double, Double, Double)
phantomDeltas loc ttf glyphIndex =
  case (loc, ttf.variations) of
    (Just loc', Just vars) ->
      case vars.gvar of
        Just gv ->
          let compCount = compositeComponentCount ttf glyphIndex
          in if compCount > 0
             then
               let (_, _, (dLeft, dRight, dTop, dBottom)) = componentDeltas gv loc' glyphIndex compCount
               in (dLeft, dRight, dTop, dBottom)
             else
               let baseContours = glyphOutlineAt Nothing ttf glyphIndex
                   (_, (dLeft, dRight, dTop, dBottom)) = applyGvarToContours gv loc' glyphIndex baseContours
               in (dLeft, dRight, dTop, dBottom)
        Nothing -> (0, 0, 0, 0)
    _ -> (0, 0, 0, 0)

hvarAdjust :: Maybe VariationLocation -> TTF -> Int -> (Double, Double, Double)
hvarAdjust loc ttf glyphIndex =
  case (loc, ttf.variations) of
    (Just loc', Just vars) ->
      case vars.hvar of
        Just hv -> hvarDeltas hv loc' glyphIndex
        Nothing -> (0, 0, 0)
    _ -> (0, 0, 0)

vvarAdjust :: Maybe VariationLocation -> TTF -> Int -> (Double, Double, Double)
vvarAdjust loc ttf glyphIndex =
  case (loc, ttf.variations) of
    (Just loc', Just vars) ->
      case vars.vvar of
        Just vv -> vvarDeltas vv loc' glyphIndex
        Nothing -> (0, 0, 0)
    _ -> (0, 0, 0)

emptyBitmap :: BitmapFormat -> MSDFBitmap
emptyBitmap fmt = MSDFBitmap
  { width = 0
  , height = 0
  , offsetX = 0
  , offsetY = 0
  , format = fmt
  , pixels = listArray (0, -1) []
  }

bboxFromContoursRaw :: [[Point]] -> (Int, Int, Int, Int)
bboxFromContoursRaw contours =
  case [ (p.x, p.y) | c <- contours, p <- c ] of
    [] -> (0, 0, 0, 0)
    pts ->
      let xs = map fst pts
          ys = map snd pts
          xMin' = floor (minimum xs)
          yMin' = floor (minimum ys)
          xMax' = ceiling (maximum xs)
          yMax' = ceiling (maximum ys)
      in (xMin', yMin', xMax', yMax')

normalizeContours :: OutlineConfig -> [[Point]] -> [[Point]]
normalizeContours cfg contours =
  let cleaned = map (normalizeContour cfg) (filter (not . null) contours)
  in if cfg.normalizeOrientation
     then normalizeOrientation cfg cleaned
     else cleaned

normalizeContour :: OutlineConfig -> [Point] -> [Point]
normalizeContour cfg pts =
  let stripped = dropDuplicatePoints cfg.contourEpsilon pts
  in case stripped of
       [] -> []
       (first:rest) ->
         let last' = lastPoint first rest
             xs = first : rest
             xs' = if samePointPos cfg.contourEpsilon first last' then init xs else xs
         in xs' ++ [first]

data EdgeInfo = EdgeInfo
  { edgeRef :: !EdgeRef
  , edge :: !Edge
  , start :: !Vec2
  , end :: !Vec2
  , pseudoStart :: !Vec2
  , pseudoEnd :: !Vec2
  , overlaps :: !Bool
  , edgeData :: !EdgeDistanceData
  }

data EdgeDistanceData
  = EdgeLineData !Double !Double !Double
  | EdgeQuadData !Double !Double !Double !Double !Double !Double !Double

attachEdgeInfo :: [[EdgeInfo]] -> [ColoredEdge] -> [(EdgeColor, EdgeInfo)]
attachEdgeInfo infos colored =
  [ (c, (infos !! ref.contourId) !! ref.edgeId)
  | ColoredEdge c ref <- colored
  ]

buildEdgeInfos :: OutlineConfig -> [[Edge]] -> [[EdgeInfo]]
buildEdgeInfos cfg contours =
  [ buildContourInfo cfg contourId edges | (contourId, edges) <- zip [0..] contours ]

buildContourInfo :: OutlineConfig -> Int -> [Edge] -> [EdgeInfo]
buildContourInfo cfg contourId edges =
  case edges of
    [] -> []
    _ ->
      let n = length edges
          area = contourArea cfg.windingFlatness edges
          orient = signOf area
          startDirs = map (normalizeVec . edgeStartDir) edges
          endDirs = map (normalizeVec . edgeEndDir) edges
          startNormals = map (interiorNormal orient) startDirs
          endNormals = map (interiorNormal orient) endDirs
          pseudoCornerThreshold = max 0 (msdfPseudoCornerDeg * pi / 180)
          angleBetween a b =
            if nearZero a || nearZero b
            then 0
            else
              let cosang = max (-1) (min 1 (dot a b))
              in acos cosang
          cornerAtVertex i =
            let prevIdx = (i - 1 + n) `mod` n
                prevDir = endDirs !! prevIdx
                currDir = startDirs !! i
            in angleBetween prevDir currDir > pseudoCornerThreshold
          pseudoAtVertex i =
            let prevIdx = (i - 1 + n) `mod` n
                nPrev = endNormals !! prevIdx
                nCurr = startNormals !! i
                summed = vecAdd nPrev nCurr
                pn = normalizeVec summed
            in if not (cornerAtVertex i)
               then (0, 0)
               else if nearZero pn then nCurr else pn
          pseudoStarts = [ pseudoAtVertex i | i <- [0 .. n - 1] ]
          pseudoEnds = [ pseudoAtVertex ((i + 1) `mod` n) | i <- [0 .. n - 1] ]
      in
      let edgeDataFor e =
            case e of
              EdgeLine (x0, y0) (x1, y1) ->
                let dx = x1 - x0
                    dy = y1 - y0
                    len2 = dx * dx + dy * dy
                    invLen2 = if len2 == 0 then 0 else 1 / len2
                in EdgeLineData dx dy invLen2
              EdgeQuad (x0, y0) (x1, y1) (x2, y2) ->
                let ax = x0 - 2 * x1 + x2
                    ay = y0 - 2 * y1 + y2
                    bx = 2 * (x1 - x0)
                    by = 2 * (y1 - y0)
                    c3 = 2 * (ax * ax + ay * ay)
                    c2 = 3 * (ax * bx + ay * by)
                    c1base = (bx * bx + by * by)
                in EdgeQuadData ax ay bx by c3 c2 c1base
      in
      [ EdgeInfo
          { edgeRef = EdgeRef contourId i e
          , edge = e
          , start = edgeStartPoint e
          , end = edgeEndPoint e
          , pseudoStart = pseudoStarts !! i
          , pseudoEnd = pseudoEnds !! i
          , overlaps = False
          , edgeData = edgeDataFor e
          }
      | (i, e) <- zip [0..] edges
      ]

interiorNormal :: Int -> Vec2 -> Vec2
interiorNormal orient (dx, dy) =
  let left = (-dy, dx)
      right = (dy, -dx)
  in if orient >= 0 then left else right

normalizeVec :: Vec2 -> Vec2
normalizeVec (x0, y0) =
  let len = sqrt (x0 * x0 + y0 * y0)
  in if len < 1e-12 then (0, 0) else (x0 / len, y0 / len)

nearZero :: Vec2 -> Bool
nearZero (x0, y0) = abs x0 < 1e-9 && abs y0 < 1e-9

normalizeOrientation :: OutlineConfig -> [[Point]] -> [[Point]]
normalizeOrientation cfg contours =
  let contoursWithEdges = map contourToEdges contours
      areas = map (contourArea cfg.windingFlatness) contoursWithEdges
      baseSign =
        case largestAreaSign areas of
          0 -> 1
          s -> s
      segs = map (flattenEdges cfg.windingFlatness) contoursWithEdges
      segBBoxes = map bboxFromSegs segs
      intersects = contourIntersections cfg.contourEpsilon segs segBBoxes
      contourCount = length contoursWithEdges
      intersectsAny i =
        or
          [ intersects i j
          | j <- [0 .. contourCount - 1]
          , j /= i
          ]
      points = zipWith (contourSamplePoint cfg.contourEpsilon) areas contoursWithEdges
      depth i =
        length
          [ ()
          | (j, s) <- zip [0..] segs
          , j /= i
          , not (intersects i j)
          , windingNumber s (points !! i) /= 0
          ]
      desiredSign i = if even (depth i) then baseSign else -baseSign
      adjust i contour =
        let a = areas !! i
            s = signOf a
        in if intersectsAny i
           then contour
           else if s == 0 || s == desiredSign i
                then contour
                else reverseContour contour
  in [ adjust i c | (i, c) <- zip [0..] contours ]

bboxFromSegs :: [LineSeg] -> Maybe BBox
bboxFromSegs [] = Nothing
bboxFromSegs segs =
  let xs = [v | ((x0, _), (x1, _)) <- segs, v <- [x0, x1]]
      ys = [v | ((_, y0), (_, y1)) <- segs, v <- [y0, y1]]
  in Just BBox
       { xMin = minimum xs
       , yMin = minimum ys
       , xMax = maximum xs
       , yMax = maximum ys
       }

bboxOverlaps :: BBox -> BBox -> Bool
bboxOverlaps a b =
  not (a.xMax < b.xMin || b.xMax < a.xMin || a.yMax < b.yMin || b.yMax < a.yMin)

bboxIntersect :: BBox -> BBox -> Maybe BBox
bboxIntersect a b =
  let x0 = max a.xMin b.xMin
      y0 = max a.yMin b.yMin
      x1 = min a.xMax b.xMax
      y1 = min a.yMax b.yMax
  in if x1 < x0 || y1 < y0
     then Nothing
     else Just BBox { xMin = x0, yMin = y0, xMax = x1, yMax = y1 }

bboxExpand :: Double -> BBox -> BBox
bboxExpand pad bb = BBox
  { xMin = bb.xMin - pad
  , yMin = bb.yMin - pad
  , xMax = bb.xMax + pad
  , yMax = bb.yMax + pad
  }

contourIntersections
  :: Double
  -> [[LineSeg]]
  -> [Maybe BBox]
  -> Int -> Int -> Bool
contourIntersections eps segs bbs =
  let n = min (length segs) (length bbs)
      hi = n - 1
      segsN = take n segs
      segBoundArr =
        if n <= 0
        then array (0, -1) []
        else array (0, hi) [ (k, toBoundedSegs s) | (k, s) <- zip [0 .. hi] segsN ]
      bbArr =
        if n <= 0
        then array (0, -1) []
        else array (0, hi) (zip [0 .. hi] (take n bbs))
  in \i j ->
       if i < 0 || j < 0 || i > hi || j > hi
       then False
       else
         case (bbArr ! i, bbArr ! j) of
           (Just bbA, Just bbB) ->
             bboxOverlaps bbA bbB && segmentsIntersectAnyBounded eps (segBoundArr ! i) (segBoundArr ! j)
           _ -> False

segmentsIntersectCoreXY :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
segmentsIntersectCoreXY eps p1x p1y p2x p2y q1x q1y q2x q2y =
  let o1 = orient2Dxy p1x p1y p2x p2y q1x q1y
      o2 = orient2Dxy p1x p1y p2x p2y q2x q2y
      o3 = orient2Dxy q1x q1y q2x q2y p1x p1y
      o4 = orient2Dxy q1x q1y q2x q2y p2x p2y
      diff a b = (a > eps && b < -eps) || (a < -eps && b > eps)
  in (diff o1 o2 && diff o3 o4)
     || (abs o1 <= eps && onSegmentBoundsXY eps p1x p1y p2x p2y q1x q1y)
     || (abs o2 <= eps && onSegmentBoundsXY eps p1x p1y p2x p2y q2x q2y)
     || (abs o3 <= eps && onSegmentBoundsXY eps q1x q1y q2x q2y p1x p1y)
     || (abs o4 <= eps && onSegmentBoundsXY eps q1x q1y q2x q2y p2x p2y)
{-# INLINE segmentsIntersectCoreXY #-}

orient2Dxy :: Double -> Double -> Double -> Double -> Double -> Double -> Double
orient2Dxy x1 y1 x2 y2 x3 y3 =
  (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
{-# INLINE orient2Dxy #-}

onSegmentBoundsXY :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
onSegmentBoundsXY eps x1 y1 x2 y2 x y =
  x >= min x1 x2 - eps && x <= max x1 x2 + eps &&
  y >= min y1 y2 - eps && y <= max y1 y2 + eps
{-# INLINE onSegmentBoundsXY #-}

data BoundedSeg
  = BoundedSeg
      {-# UNPACK #-} !Double {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double {-# UNPACK #-} !Double

toBoundedSegs :: [LineSeg] -> [BoundedSeg]
toBoundedSegs =
  map (\((x1, y1), (x2, y2)) ->
    let mnx = min x1 x2
        mxx = max x1 x2
        mny = min y1 y2
        mxy = max y1 y2
    in BoundedSeg x1 y1 x2 y2 mnx mxx mny mxy)

bboxOverlapsEps
  :: Double
  -> Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> Bool
bboxOverlapsEps eps minAx maxAx minAy maxAy minBx maxBx minBy maxBy =
  not
    (maxAx < minBx - eps || maxBx < minAx - eps
      || maxAy < minBy - eps || maxBy < minAy - eps)
{-# INLINE bboxOverlapsEps #-}

contourArea :: Double -> [Edge] -> Double
contourArea flatness edges =
  let segs = flattenEdges flatness edges
  in 0.5 * sum [ cross a b | (a, b) <- segs ]
  where
    cross (x0, y0) (x1, y1) = x0 * y1 - x1 * y0

largestAreaSign :: [Double] -> Int
largestAreaSign areas =
  let pick (bestA, bestS) a =
        let aa = abs a
            s = signOf a
        in if aa > bestA then (aa, s) else (bestA, bestS)
  in snd (foldl pick (0, 0) areas)

signOf :: Double -> Int
signOf v
  | v > 0 = 1
  | v < 0 = -1
  | otherwise = 0

contourSamplePoint :: Double -> Double -> [Edge] -> (Double, Double)
contourSamplePoint eps area edges =
  case edges of
    (e:_) ->
      let p = edgePointAt e 0.5
          t = edgeTangentAt e 0.5
          n = interiorNormal (signOf area) (normalizeVec t)
          bb = contourBBox edges
          diag = sqrt ((bb.xMax - bb.xMin) ^ (2 :: Int) + (bb.yMax - bb.yMin) ^ (2 :: Int))
          step = max (eps * 10) (diag * 1e-4)
      in vecAdd p (vecScale step n)
    [] -> (0, 0)

contourBBox :: [Edge] -> BBox
contourBBox edges =
  case edges of
    [] -> BBox 0 0 0 0
    (e:es) ->
      let bb0 = edgeBBox e
      in foldl (\acc edge -> bboxUnionSimple acc (edgeBBox edge)) bb0 es

reverseContour :: [Point] -> [Point]
reverseContour pts =
  case pts of
    [] -> []
    [_] -> pts
    (p:_) ->
      let lastPt = lastPointSafe p pts
          closed = if samePointPos 0 p lastPt then init pts else pts
          rev = reverse closed
      in case rev of
           [] -> []
           (r0:_) -> rev ++ [r0]
  where
    lastPointSafe prev [] = prev
    lastPointSafe _ (x:xs) = lastPointSafe x xs

dropDuplicatePoints :: Double -> [Point] -> [Point]
dropDuplicatePoints _ [] = []
dropDuplicatePoints eps (p:ps) = p : go p ps
  where
    go _ [] = []
    go prev (x:xs)
      | samePointPos eps prev x = go prev xs
      | otherwise = x : go x xs

samePointPos :: Double -> Point -> Point -> Bool
samePointPos eps a b = abs (a.x - b.x) <= eps && abs (a.y - b.y) <= eps

lastPoint :: Point -> [Point] -> Point
lastPoint prev [] = prev
lastPoint _ (x:xs) = lastPoint x xs

applyOverlapFlagsByOrder :: [[EdgeInfo]] -> [Bool] -> [[EdgeInfo]]
applyOverlapFlagsByOrder infosByContour flags =
  snd (mapAccumL applyContour flags infosByContour)
  where
    applyContour fs infos =
      let (infos', fs') = applyInfos fs infos
      in (fs', infos')

    applyInfos fs [] = ([], fs)
    applyInfos [] (info:infos) =
      let (rest, fs') = applyInfos [] infos
      in (info : rest, fs')
    applyInfos (f:fs) (info:infos) =
      let info' = info { overlaps = f }
          (rest, fs') = applyInfos fs infos
      in (info' : rest, fs')

filterDegenerateEdges :: Double -> [Edge] -> [Edge]
filterDegenerateEdges eps = filter (\e -> edgeLength e > eps)
  where
    edgeLength (EdgeLine (x0, y0) (x1, y1)) = sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int))
    edgeLength (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
      let dx01 = x1 - x0
          dy01 = y1 - y0
          dx12 = x2 - x1
          dy12 = y2 - y1
      in sqrt (dx01 * dx01 + dy01 * dy01) + sqrt (dx12 * dx12 + dy12 * dy12)

scalePoint :: Double -> Point -> Point
scalePoint s p = Point (p.x * s) (p.y * s) p.on

data FlatSeg = FlatSeg !Int !Int !Double !Double !Vec2 !Vec2

splitContoursIntersections :: OutlineConfig -> [[Edge]] -> [[Edge]]
splitContoursIntersections cfg contours
  | not cfg.splitIntersections = contours
  | all ((< 2) . length) contours = contours
  | otherwise =
      let flatness = cfg.windingFlatness
          eps = max cfg.contourEpsilon (flatness * 0.1)
          edgesWithIds =
            [ (cid, eid, e)
            | (cid, edges) <- zip [0..] contours
            , (eid, e) <- zip [0..] edges
            ]
          flatSegs = concat
            [ flattenEdgeWithParams flatness cid eid e
            | (cid, eid, e) <- edgesWithIds
            ]
          segCount = length flatSegs
          skipSplit = msdfSplitMaxSegs > 0 && segCount > msdfSplitMaxSegs
          cellSize = chooseSplitGridSize flatSegs eps
          tMap = collectIntersectionsGrid contours eps cellSize flatSegs
          splitEdgeFor cid eid e =
            let ts = Map.findWithDefault [] (cid, eid) tMap
            in splitEdgeAt cfg.contourEpsilon e ts
          traceSkip =
            if msdfTraceEnabled && skipSplit
            then trace ("msdf: skip split intersections, flatSegs=" <> show segCount) True
            else skipSplit
      in if traceSkip
         then contours
         else
           [ concat [ splitEdgeFor cid eid e | (eid, e) <- zip [0..] edges ]
           | (cid, edges) <- zip [0..] contours
           ]

reverseEdge :: Edge -> Edge
reverseEdge (EdgeLine p0 p1) = EdgeLine p1 p0
reverseEdge (EdgeQuad p0 p1 p2) = EdgeQuad p2 p1 p0

preprocessContours :: OutlineConfig -> FillRule -> [[Edge]] -> [[Edge]]
preprocessContours cfg rule contours =
  let flatness = cfg.windingFlatness
      eps = max cfg.contourEpsilon (flatness * 0.1)
      edgesAll = concat contours
      mergeEps = max eps (flatness * 0.5)
      loops = faceWalkLoops mergeEps edgesAll
      segsAll = flattenEdges flatness edgesAll
      loops0 = mapMaybe (classifyLoop rule eps segsAll) loops
      loops' = dedupeLoops mergeEps loops0
      areaAbs es = abs (contourArea flatness es)
      origArea = sum (map areaAbs contours)
      newArea = sum (map areaAbs loops')
      areaOk =
        if origArea <= 1e-9
        then True
        else
          let ratio = newArea / origArea
          in ratio >= 0.85 && ratio <= 1.15
      fallback = null loops' || not areaOk
      fallbackMsg =
        "msdf: preprocess fallback"
          <> " origArea=" <> show origArea
          <> " newArea=" <> show newArea
          <> " loopsIn=" <> show (length contours)
          <> " loopsClassified=" <> show (length loops0)
          <> " loopsOut=" <> show (length loops')
  in if fallback
     then if msdfTraceEnabled then trace fallbackMsg contours else contours
     else loops'

dedupeLoops :: Double -> [[Edge]] -> [[Edge]]
dedupeLoops eps loops =
  let scale = 1 / max 1e-9 eps
      quant (x, y) = (round (x * scale) :: Int, round (y * scale) :: Int)
      orderPair a b = if a <= b then (a, b) else (b, a)
      edgeKey e =
        case e of
          EdgeLine p0 p1 ->
            let (a, b) = orderPair (quant p0) (quant p1)
            in (0 :: Int, a, b, (0, 0))
          EdgeQuad p0 p1 p2 ->
            let (a, b) = orderPair (quant p0) (quant p2)
            in (1 :: Int, a, b, quant p1)
      loopKey es = sort (map edgeKey es)
      step (!seen, acc) es =
        let k = loopKey es
        in if Set.member k seen
           then (seen, acc)
           else (Set.insert k seen, es : acc)
      (_, keptRev) = foldl' step (Set.empty, []) loops
  in reverse keptRev

data HalfEdge = HalfEdge !Edge !Int !Int !Vec2 !Vec2

faceWalkLoops :: Double -> [Edge] -> [[Edge]]
faceWalkLoops eps edges =
  let vEps = max eps 1e-9
      key (x, y) =
        let kx = round (x / vEps) :: Int
            ky = round (y / vEps) :: Int
        in (kx, ky)
      addVertex (m, next) p =
        let k = key p
        in if Map.member k m
           then (m, next)
           else (Map.insert k next m, next + 1)
      (vmap, _) =
        foldl'
          addVertex
          (Map.empty, 0 :: Int)
          ([ edgeStartPoint e | e <- edges ] ++ [ edgeEndPoint e | e <- edges ])
      vertexId p = Map.findWithDefault (-1) (key p) vmap
      mkHalfEdges e =
        let s = edgeStartPoint e
            t = edgeEndPoint e
            sd = normalizeVec (edgeStartDir e)
            ed = normalizeVec (edgeEndDir e)
            sid = vertexId s
            tid = vertexId t
            rev = reverseEdge e
            sdR = normalizeVec (vecScale (-1) ed)
            edR = normalizeVec (vecScale (-1) sd)
        in if sid < 0 || tid < 0
           then []
           else
             [ HalfEdge e sid tid sd ed
             , HalfEdge rev tid sid sdR edR
             ]
      hedges = concatMap mkHalfEdges edges
      n = length hedges
      edgeArr =
        if n == 0 then array (0, -1) [] else array (0, n - 1) (zip [0..] hedges)
      angleOf (dx, dy) =
        let a = atan2 dy dx
        in if a < 0 then a + 2 * pi else a
      outgoing =
        foldl'
          (\acc (i, e) ->
             let HalfEdge _ start _ startDir _ = e
                 ang = angleOf startDir
             in IntMap.insertWith (++) start [(ang, i)] acc
          )
          IntMap.empty
          (zip [0..] hedges)
      chooseNext v incomingDir used =
        case IntMap.lookup v outgoing of
          Nothing -> Nothing
          Just lst ->
            let anglePrev = angleOf incomingDir
                candidates = [ (ang, idx) | (ang, idx) <- lst, not (IntSet.member idx used) ]
                sorted = sortOn fst candidates
            in case sorted of
                 [] -> Nothing
                 _ ->
                   let before = takeWhile (\(ang, _) -> ang < anglePrev - 1e-12) sorted
                       pick = case reverse before of
                         (x:_) -> x
                         [] -> last sorted
                   in Just (snd pick)
      walk startIdx used0 =
        let startEdge = edgeArr ! startIdx
            startV =
              let HalfEdge _ start _ _ _ = startEdge
              in start
            go idx used acc =
              if IntSet.member idx used
              then (used, reverse acc)
              else
                let used' = IntSet.insert idx used
                    e = edgeArr ! idx
                    HalfEdge edge _ endV _ endDir = e
                    acc' = edge : acc
                in if endV == startV && not (null acc)
                   then (used', reverse acc')
                   else
                     case chooseNext endV (vecScale (-1) endDir) used' of
                       Nothing -> (used', reverse acc')
                       Just nextIdx -> go nextIdx used' acc'
        in go startIdx used0 []
      build i used acc =
        if i >= n
        then acc
        else if IntSet.member i used
          then build (i + 1) used acc
          else
            let (used', loopEdges) = walk i used
            in build (i + 1) used' (if length loopEdges < 2 then acc else loopEdges : acc)
      in reverse (build 0 IntSet.empty [])

chooseSplitGridSize :: [FlatSeg] -> Double -> Double
chooseSplitGridSize segs eps =
  let override = msdfSplitGridSize
  in if override > 0
     then override
     else
       let (minx, maxx, miny, maxy) = segsBounds segs
           area = max 1e-6 ((maxx - minx) * (maxy - miny))
           n = max 1 (length segs)
           base = sqrt (area / fromIntegral n)
           minCell = max (eps * 4) 1e-3
       in max minCell base

segsBounds :: [FlatSeg] -> (Double, Double, Double, Double)
segsBounds [] = (0, 0, 0, 0)
segsBounds (FlatSeg _ _ _ _ (x0, y0) (x1, y1):rest) =
  let (minx, maxx) = if x0 < x1 then (x0, x1) else (x1, x0)
      (miny, maxy) = if y0 < y1 then (y0, y1) else (y1, y0)
      go (mnx, mxx, mny, mxy) [] = (mnx, mxx, mny, mxy)
      go (mnx, mxx, mny, mxy) (FlatSeg _ _ _ _ (ax0, ay0) (ax1, ay1):xs) =
        let mnx' = min mnx (min ax0 ax1)
            mxx' = max mxx (max ax0 ax1)
            mny' = min mny (min ay0 ay1)
            mxy' = max mxy (max ay0 ay1)
        in go (mnx', mxx', mny', mxy') xs
  in go (minx, maxx, miny, maxy) rest

type CellBuckets = IntMap.IntMap (IntMap.IntMap [Int])

insertBucketMember :: Int -> Int -> Int -> CellBuckets -> CellBuckets
insertBucketMember cx cy idx =
  IntMap.alter updateRow cx
  where
    updateRow Nothing = Just (IntMap.singleton cy [idx])
    updateRow (Just row) = Just (IntMap.insertWith (++) cy [idx] row)

bucketGroups :: CellBuckets -> [[Int]]
bucketGroups buckets = concatMap IntMap.elems (IntMap.elems buckets)

bucketCellCount :: CellBuckets -> Int
bucketCellCount buckets = sum (map IntMap.size (IntMap.elems buckets))

collectIntersectionsGrid :: [[Edge]] -> Double -> Double -> [FlatSeg] -> Map.Map (Int, Int) [Double]
collectIntersectionsGrid contours eps cellSize segs
  | null segs = Map.empty
  | otherwise =
      let n = length segs
          segArr = array (0, n - 1) (zip [0..] segs)
          keyBase = n + 1
          minCell = max (eps * 4) 1e-3
          build size =
            let cellBuckets = foldl' (insertSegBuckets eps size) IntMap.empty (zip [0..] segs)
                bucketSizes = map length (bucketGroups cellBuckets)
                maxBucketSize = if null bucketSizes then 0 else maximum bucketSizes
                pairCount = sum [ s * (s - 1) `div` 2 | s <- bucketSizes ]
            in (cellBuckets, maxBucketSize, pairCount)
          refine size iter =
            let (cellBuckets, maxBucketSize, pairCount) = build size
                overLimit = (msdfSplitMaxPairs > 0 && pairCount > msdfSplitMaxPairs)
                         || (msdfSplitMaxBucket > 0 && maxBucketSize > msdfSplitMaxBucket)
            in if overLimit && iter < msdfSplitGridMaxIters && size > minCell
               then refine (max minCell (size * 0.5)) (iter + 1)
               else (size, cellBuckets, maxBucketSize, pairCount, overLimit)
          (size', buckets, maxBucket, totalPairs, tooMany) = refine cellSize 0
          step (acc, seen) idxs =
            let (acc', seen') = foldPairs keyBase acc seen idxs segArr
            in (acc', seen')
          tMap =
            if tooMany
            then Map.empty
            else fst (foldl' step (Map.empty, IntSet.empty) (bucketGroups buckets))
          traceMsg =
            "msdf: split grid cell=" <> show size'
              <> " buckets=" <> show (bucketCellCount buckets)
              <> " maxBucket=" <> show maxBucket
              <> " pairs=" <> show totalPairs
              <> if tooMany then " (skip split)" else ""
      in if msdfTraceEnabled
         then trace traceMsg tMap
         else tMap
  where
    insertSegBuckets pad size acc (idx, seg) =
      let (minx, maxx, miny, maxy) = segBBox seg
          minCx = cellCoord size (minx - pad)
          maxCx = cellCoord size (maxx + pad)
          minCy = cellCoord size (miny - pad)
          maxCy = cellCoord size (maxy + pad)
          addCell m cx cy = insertBucketMember cx cy idx m
      in foldGridCells minCx maxCx minCy maxCy acc addCell

    segBBox (FlatSeg _ _ _ _ (x0, y0) (x1, y1)) =
      let mnx = min x0 x1
          mxx = max x0 x1
          mny = min y0 y1
          mxy = max y0 y1
      in (mnx, mxx, mny, mxy)

    foldPairs keyBase acc0 seen0 idxs segArr =
      let go [] acc seen = (acc, seen)
          go (i:is) acc seen =
            let (acc', seen') = foldl' (handlePair i) (acc, seen) is
            in go is acc' seen'
          handlePair i (acc, seen) j =
            let (a, b) = if i < j then (i, j) else (j, i)
                key = a * keyBase + b
            in if IntSet.member key seen
               then (acc, seen)
               else
                 let acc' = collectIntersection contours eps acc (segArr ! a, segArr ! b)
                 in (acc', IntSet.insert key seen)
      in go idxs acc0 seen0

classifyLoop :: FillRule -> Double -> [LineSeg] -> [Edge] -> Maybe [Edge]
classifyLoop rule eps segs edges =
  case edges of
    [] -> Nothing
    (e:_) ->
      case loopSide rule eps segs e of
        Nothing -> Nothing
        Just (insideLeft, insideRight) ->
          if insideLeft == insideRight
          then Nothing
          else if insideLeft
            then Just edges
            else Just (map reverseEdge (reverse edges))

loopSide :: FillRule -> Double -> [LineSeg] -> Edge -> Maybe (Bool, Bool)
loopSide rule eps segs edge =
  let p = edgePointAt edge 0.5
      (tx, ty) = edgeTangentAt edge 0.5
      n = normalizeVec (-ty, tx)
      base = max eps (edgeLengthApprox edge * 1e-2)
      steps = [base, base * 2, base * 4]
      apply = applyFillRule rule
      insideAt pt = apply (windingNumber segs pt)
      go [] = Nothing
      go (step:rest) =
        let p1 = vecAdd p (vecScale step n)
            p2 = vecSub p (vecScale step n)
            i1 = insideAt p1
            i2 = insideAt p2
        in if i1 == i2 then go rest else Just (i1, i2)
  in if nearZero n then Nothing else go steps


collectIntersection :: [[Edge]] -> Double -> Map.Map (Int, Int) [Double] -> (FlatSeg, FlatSeg) -> Map.Map (Int, Int) [Double]
collectIntersection contours eps acc (FlatSeg ca ea t0a t1a p0a p1a, FlatSeg cb eb t0b t1b p0b p1b)
  | ca == cb && ea == eb = acc
  | edgesShareEndpointEdge eps (edgeAt ca ea) (edgeAt cb eb) = acc
  | otherwise =
      case segmentIntersectionTU eps p0a p1a p0b p1b of
        Just (u, v) ->
          let ta = t0a + u * (t1a - t0a)
              tb = t0b + v * (t1b - t0b)
              accA = if isInterior u then Map.insertWith (++) (ca, ea) [ta] acc else acc
              accB = if isInterior v then Map.insertWith (++) (cb, eb) [tb] accA else accA
          in accB
        Nothing ->
          let acc0 = addColinearOverlap acc
              acc1 = insertEndpointSplit acc0 (cb, eb) p0b p1b p0a
              acc2 = insertEndpointSplit acc1 (cb, eb) p0b p1b p1a
              acc3 = insertEndpointSplit acc2 (ca, ea) p0a p1a p0b
              acc4 = insertEndpointSplit acc3 (ca, ea) p0a p1a p1b
          in acc4
  where
    edgeAt cid eid = (contours !! cid) !! eid
    isInterior t = t > eps && t < 1 - eps
    addColinearOverlap acc' =
      case colinearOverlapParams eps p0a p1a p0b p1b of
        Nothing -> acc'
        Just (tmin, tmax, smin, smax) ->
          let accA = addSplitParams acc' (ca, ea) t0a t1a tmin tmax
              accB = addSplitParams accA (cb, eb) t0b t1b smin smax
          in accB
    insertEndpointSplit acc' key s0 s1 pt =
      case pointOnSegmentParam eps s0 s1 pt of
        Just t | isInterior t -> Map.insertWith (++) key [t] acc'
        _ -> acc'

addSplitParams :: Map.Map (Int, Int) [Double] -> (Int, Int) -> Double -> Double -> Double -> Double -> Map.Map (Int, Int) [Double]
addSplitParams acc key segT0 segT1 tmin tmax =
  let isInterior t = t > 1e-6 && t < 1 - 1e-6
      ta = segT0 + tmin * (segT1 - segT0)
      tb = segT0 + tmax * (segT1 - segT0)
      acc1 = if isInterior tmin then Map.insertWith (++) key [ta] acc else acc
      acc2 = if isInterior tmax then Map.insertWith (++) key [tb] acc1 else acc1
  in acc2

colinearOverlapParams :: Double -> Vec2 -> Vec2 -> Vec2 -> Vec2 -> Maybe (Double, Double, Double, Double)
colinearOverlapParams eps p0 p1 q0 q1 =
  let rx = fst p1 - fst p0
      ry = snd p1 - snd p0
      sx = fst q1 - fst q0
      sy = snd q1 - snd q0
      rlen2 = rx * rx + ry * ry
      slen2 = sx * sx + sy * sy
  in if rlen2 < eps * eps || slen2 < eps * eps
     then Nothing
     else
       let d0 = distancePointLine q0 p0 p1
           d1 = distancePointLine q1 p0 p1
       in if d0 > eps || d1 > eps
          then Nothing
          else
            let t0 = ((fst q0 - fst p0) * rx + (snd q0 - snd p0) * ry) / rlen2
                t1 = ((fst q1 - fst p0) * rx + (snd q1 - snd p0) * ry) / rlen2
                tmin = max 0 (min t0 t1)
                tmax = min 1 (max t0 t1)
                s0 = ((fst p0 - fst q0) * sx + (snd p0 - snd q0) * sy) / slen2
                s1 = ((fst p1 - fst q0) * sx + (snd p1 - snd q0) * sy) / slen2
                smin = max 0 (min s0 s1)
                smax = min 1 (max s0 s1)
            in if tmax - tmin <= eps || smax - smin <= eps
               then Nothing
               else Just (tmin, tmax, smin, smax)

flattenEdgeWithParams :: Double -> Int -> Int -> Edge -> [FlatSeg]
flattenEdgeWithParams _ cid eid (EdgeLine p0 p1) =
  [ FlatSeg cid eid 0 1 p0 p1 ]
flattenEdgeWithParams eps cid eid (EdgeQuad p0 p1 p2) =
  flattenQuadWithParams eps cid eid 0 1 p0 p1 p2

flattenQuadWithParams :: Double -> Int -> Int -> Double -> Double -> Vec2 -> Vec2 -> Vec2 -> [FlatSeg]
flattenQuadWithParams eps cid eid t0 t1 p0 p1 p2
  | quadFlatEnough eps p0 p1 p2 = [ FlatSeg cid eid t0 t1 p0 p2 ]
  | otherwise =
      let (l0, l1, l2, r0, r1, r2) = subdivideQuad p0 p1 p2
          tm = (t0 + t1) * 0.5
      in flattenQuadWithParams eps cid eid t0 tm l0 l1 l2
         ++ flattenQuadWithParams eps cid eid tm t1 r0 r1 r2

quadFlatEnough :: Double -> Vec2 -> Vec2 -> Vec2 -> Bool
quadFlatEnough eps p0 p1 p2 =
  distancePointLine p1 p0 p2 <= eps

distancePointLine :: Vec2 -> Vec2 -> Vec2 -> Double
distancePointLine (px, py) (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      num = abs ((px - x0) * dy - (py - y0) * dx)
      den = sqrt (dx * dx + dy * dy)
  in if den < 1e-12 then sqrt ((px - x0) ^ (2 :: Int) + (py - y0) ^ (2 :: Int)) else num / den

subdivideQuad :: Vec2 -> Vec2 -> Vec2 -> (Vec2, Vec2, Vec2, Vec2, Vec2, Vec2)
subdivideQuad p0 p1 p2 =
  let p01 = midpoint p0 p1
      p12 = midpoint p1 p2
      p012 = midpoint p01 p12
  in (p0, p01, p012, p012, p12, p2)

midpoint :: Vec2 -> Vec2 -> Vec2
midpoint (x0, y0) (x1, y1) = ((x0 + x1) * 0.5, (y0 + y1) * 0.5)

segmentIntersectionTU :: Double -> Vec2 -> Vec2 -> Vec2 -> Vec2 -> Maybe (Double, Double)
segmentIntersectionTU eps (p0x, p0y) (p1x, p1y) (q0x, q0y) (q1x, q1y) =
  let rx = p1x - p0x
      ry = p1y - p0y
      sx = q1x - q0x
      sy = q1y - q0y
      rxs = rx * sy - ry * sx
      qpx = q0x - p0x
      qpy = q0y - p0y
      qpxr = qpx * ry - qpy * rx
  in if abs rxs <= eps
     then Nothing
     else
       let t = (qpx * sy - qpy * sx) / rxs
           u = qpxr / rxs
       in if t >= -eps && t <= 1 + eps && u >= -eps && u <= 1 + eps
          then Just (t, u)
          else Nothing

pointOnSegmentParam :: Double -> Vec2 -> Vec2 -> Vec2 -> Maybe Double
pointOnSegmentParam eps (x0, y0) (x1, y1) (px, py) =
  let dx = x1 - x0
      dy = y1 - y0
      denom = dx * dx + dy * dy
  in if denom < 1e-12
     then Nothing
     else
       let t = ((px - x0) * dx + (py - y0) * dy) / denom
           tClamped = max 0 (min 1 t)
           cx = x0 + tClamped * dx
           cy = y0 + tClamped * dy
           distSq = (px - cx) * (px - cx) + (py - cy) * (py - cy)
       in if distSq <= eps * eps then Just tClamped else Nothing

splitEdgeAt :: Double -> Edge -> [Double] -> [Edge]
splitEdgeAt eps edge ts =
  let ts' = dedupTs eps (filter (\t -> t > eps && t < 1 - eps) ts)
  in case edge of
       EdgeLine p0 p1 -> splitLineAt p0 p1 ts'
       EdgeQuad p0 p1 p2 -> splitQuadAt p0 p1 p2 ts'

splitLineAt :: Vec2 -> Vec2 -> [Double] -> [Edge]
splitLineAt p0 p1 ts =
  let pts = map (lerp p0 p1) ts
      allPts = p0 : pts ++ [p1]
  in case allPts of
       (_:_) -> [ EdgeLine a b | (a, b) <- zip allPts (drop 1 allPts) ]
       [] -> []

splitQuadAt :: Vec2 -> Vec2 -> Vec2 -> [Double] -> [Edge]
splitQuadAt p0 p1 p2 ts =
  case ts of
    [] -> [EdgeQuad p0 p1 p2]
    (t:rest) ->
      let (l0, l1, l2, r0, r1, r2) = splitQuadAtParam t p0 p1 p2
          rest' = map (\u -> (u - t) / (1 - t)) rest
      in EdgeQuad l0 l1 l2 : splitQuadAt r0 r1 r2 rest'

splitQuadAtParam :: Double -> Vec2 -> Vec2 -> Vec2 -> (Vec2, Vec2, Vec2, Vec2, Vec2, Vec2)
splitQuadAtParam t p0 p1 p2 =
  let p01 = lerp p0 p1 t
      p12 = lerp p1 p2 t
      p012 = lerp p01 p12 t
  in (p0, p01, p012, p012, p12, p2)

lerp :: Vec2 -> Vec2 -> Double -> Vec2
lerp (x0, y0) (x1, y1) t = (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)

dedupTs :: Double -> [Double] -> [Double]
dedupTs eps ts =
  case sortOn id ts of
    [] -> []
    (x:xs) -> x : go x xs
  where
    go _ [] = []
    go prev (y:ys)
      | abs (y - prev) <= eps = go prev ys
      | otherwise = y : go y ys

edgesShareEndpointEdge :: Double -> Edge -> Edge -> Bool
edgesShareEndpointEdge eps a b =
  let a0 = edgeStartPoint a
      a1 = edgeEndPoint a
      b0 = edgeStartPoint b
      b1 = edgeEndPoint b
  in sameVec eps a0 b0 || sameVec eps a0 b1 || sameVec eps a1 b0 || sameVec eps a1 b1

computeEdgeOverlaps :: Double -> Double -> [EdgeInfo] -> [Bool]
computeEdgeOverlaps flatness eps infos =
  let n = length infos
      infoArr =
        if null infos
        then array (0, -1) []
        else array (0, n - 1) (zip [0..] infos)
      bboxArr =
        if null infos
        then array (0, -1) []
        else array (0, n - 1)
          [ (i, edgeBBox info.edge) | (i, info) <- zip [0..] infos ]
      (_, traceMsg, tooMany) = overlapGridStats bboxArr eps
      skip = tooMany || (msdfOverlapMaxEdges > 0 && n > msdfOverlapMaxEdges)
  in if n == 0
     then []
     else if skip
       then
         let res = replicate n False
             msg = traceMsg <> if msdfOverlapMaxEdges > 0 && n > msdfOverlapMaxEdges
                               then " (skip edges=" <> show n <> ")"
                               else ""
         in if msdfTraceEnabled then trace msg res else res
       else
        let segsArr =
              array (0, n - 1)
                [ (i, flattenEdges flatness [info.edge]) | (i, info) <- zip [0..] infos ]
            (overlapPairs, traceMsg') = overlapPairsGrid eps infoArr bboxArr segsArr
            overlapsArr = accumArray (||) False (0, n - 1) overlapPairs
            res = [ overlapsArr ! i | i <- [0 .. n - 1] ]
            msg = traceMsg <> " " <> traceMsg'
        in if msdfTraceEnabled then trace msg res else res

edgesShareEndpoint :: Double -> EdgeInfo -> EdgeInfo -> Bool
edgesShareEndpoint eps infoA infoB =
  let a0 = edgeStartPoint infoA.edge
      a1 = edgeEndPoint infoA.edge
      b0 = edgeStartPoint infoB.edge
      b1 = edgeEndPoint infoB.edge
  in sameVec eps a0 b0 || sameVec eps a0 b1 || sameVec eps a1 b0 || sameVec eps a1 b1

sameVec :: Double -> Vec2 -> Vec2 -> Bool
sameVec eps (x0, y0) (x1, y1) = abs (x0 - x1) <= eps && abs (y0 - y1) <= eps

segmentsIntersectAnyBounded :: Double -> [BoundedSeg] -> [BoundedSeg] -> Bool
segmentsIntersectAnyBounded eps segsA segsB = goA segsA
  where
    goA [] = False
    goA (BoundedSeg ax1 ay1 ax2 ay2 minAx maxAx minAy maxAy : restA)
      | goB ax1 ay1 ax2 ay2 minAx maxAx minAy maxAy segsB = True
      | otherwise = goA restA
    goB _ _ _ _ _ _ _ _ [] = False
    goB ax1 ay1 ax2 ay2 minAx maxAx minAy maxAy (BoundedSeg bx1 by1 bx2 by2 minBx maxBx minBy maxBy : restB) =
      if not (bboxOverlapsEps eps minAx maxAx minAy maxAy minBx maxBx minBy maxBy)
      then goB ax1 ay1 ax2 ay2 minAx maxAx minAy maxAy restB
      else if segmentsIntersectCoreXY eps ax1 ay1 ax2 ay2 bx1 by1 bx2 by2
           then True
           else goB ax1 ay1 ax2 ay2 minAx maxAx minAy maxAy restB

overlapPairsGrid
  :: Double
  -> Array Int EdgeInfo
  -> Array Int BBox
  -> Array Int [LineSeg]
  -> ([(Int, Bool)], String)
overlapPairsGrid eps infoArr bboxArr segsArr =
  let (buckets, traceMsg, tooMany) = overlapGridStats bboxArr eps
      (lo, hi) = bounds infoArr
      n = if lo > hi then 0 else hi - lo + 1
      keyBase = n + 1
      segsBoundArr =
        if n <= 0
        then array (0, -1) []
        else array (0, n - 1)
          [ (i, toBoundedSegs (segsArr ! i))
          | i <- [0 .. n - 1]
          ]
  in if tooMany || n <= 1
     then ([], traceMsg)
     else
       let step (accPairs, seen) idxs =
             let (pairs', seen') = foldOverlapPairs keyBase eps infoArr bboxArr segsBoundArr accPairs seen idxs
             in (pairs', seen')
           (pairsOut, _) = foldl' step ([], IntSet.empty) (bucketGroups buckets)
       in (pairsOut, traceMsg)

overlapGridStats :: Array Int BBox -> Double -> (CellBuckets, String, Bool)
overlapGridStats bboxArr eps =
  let (lo, hi) = bounds bboxArr
      n = if lo > hi then 0 else hi - lo + 1
      minCell = max (eps * 4) 1e-3
      cellSize = if msdfOverlapGridSize > 0 then msdfOverlapGridSize else chooseOverlapCell bboxArr
      size = max minCell cellSize
      buckets = buildOverlapBuckets bboxArr size
      sizes = map length (bucketGroups buckets)
      maxBucket = if null sizes then 0 else maximum sizes
      totalPairs = sum [ s * (s - 1) `div` 2 | s <- sizes ]
      tooMany = (msdfOverlapMaxPairs > 0 && totalPairs > msdfOverlapMaxPairs)
             || (msdfOverlapMaxBucket > 0 && maxBucket > msdfOverlapMaxBucket)
      traceMsg =
        "msdf: overlap grid cell=" <> show size
          <> " buckets=" <> show (bucketCellCount buckets)
          <> " maxBucket=" <> show maxBucket
          <> " pairs=" <> show totalPairs
          <> if tooMany || n <= 1 then " (skip overlap)" else ""
  in (buckets, traceMsg, tooMany || n <= 1)

chooseOverlapCell :: Array Int BBox -> Double
chooseOverlapCell bboxArr =
  let (lo, hi) = bounds bboxArr
      (minx, maxx, miny, maxy) =
        if lo > hi
        then (0, 0, 0, 0)
        else
          let b0 = bboxArr ! lo
              start = (b0.xMin, b0.xMax, b0.yMin, b0.yMax)
          in foldl' expand start [lo + 1 .. hi]
      area = max 1e-6 ((maxx - minx) * (maxy - miny))
      n = max 1 (hi - lo + 1)
  in sqrt (area / fromIntegral n)
  where
    expand (mnx, mxx, mny, mxy) i =
      let b = bboxArr ! i
      in (min mnx b.xMin, max mxx b.xMax, min mny b.yMin, max mxy b.yMax)

buildOverlapBuckets :: Array Int BBox -> Double -> CellBuckets
buildOverlapBuckets bboxArr size =
  let (lo, hi) = bounds bboxArr
      addBucket m idx =
        let b = bboxArr ! idx
            minx = b.xMin
            maxx = b.xMax
            miny = b.yMin
            maxy = b.yMax
            minCx = cellCoord size minx
            maxCx = cellCoord size maxx
            minCy = cellCoord size miny
            maxCy = cellCoord size maxy
            addCell acc cx cy = insertBucketMember cx cy idx acc
        in foldGridCells minCx maxCx minCy maxCy m addCell
  in if lo > hi then IntMap.empty else foldl' addBucket IntMap.empty [lo .. hi]

cellCoord :: Double -> Double -> Int
cellCoord size v = floor (v / size) :: Int

foldGridCells :: Int -> Int -> Int -> Int -> a -> (a -> Int -> Int -> a) -> a
foldGridCells minCx maxCx minCy maxCy acc0 step = goX minCx acc0
  where
    goX !cx !acc
      | cx > maxCx = acc
      | otherwise = goX (cx + 1) (goY cx minCy acc)
    goY !cx !cy !acc
      | cy > maxCy = acc
      | otherwise = goY cx (cy + 1) (step acc cx cy)

foldOverlapPairs
  :: Int
  -> Double
  -> Array Int EdgeInfo
  -> Array Int BBox
  -> Array Int [BoundedSeg]
  -> [(Int, Bool)]
  -> IntSet.IntSet
  -> [Int]
  -> ([(Int, Bool)], IntSet.IntSet)
foldOverlapPairs keyBase eps infoArr bboxArr segsBoundArr pairs0 seen0 idxs =
  let go [] pairs seen = (pairs, seen)
      go (i:is) pairs seen =
        let (pairs', seen') = foldl' (handlePair i) (pairs, seen) is
        in go is pairs' seen'
      handlePair i (pairs, seen) j =
        let (a, b) = if i < j then (i, j) else (j, i)
            key = a * keyBase + b
        in if IntSet.member key seen
           then (pairs, seen)
           else
             let infoA = infoArr ! a
                 infoB = infoArr ! b
                 overlaps =
                   not (edgesShareEndpoint eps infoA infoB)
                   && bboxOverlaps (bboxArr ! a) (bboxArr ! b)
                   && segmentsIntersectAnyBounded eps (segsBoundArr ! a) (segsBoundArr ! b)
                 pairs' =
                   if overlaps
                   then (a, True) : (b, True) : pairs
                   else pairs
             in (pairs', IntSet.insert key seen)
  in go idxs pairs0 seen0

renderBitmap :: MSDFConfig -> Int -> Int -> Double -> Double -> [(EdgeColor, EdgeInfo)] -> [LineSeg] -> [[LineSeg]] -> UArray Int Word8
renderBitmap cfg width height offsetX offsetY coloredInfos segs _segsByContour =
  unsafePerformIO $ do
    let fmt = cfg.outputFormat
        n = width * height
        channels = bitmapChannels fmt
        range = cfg.range
        edgeThreshF = realToFrac cfg.correction.edgeThreshold :: Float
        chanThreshF = realToFrac cfg.correction.channelThreshold :: Float
        hardThreshF = realToFrac cfg.correction.hardThreshold :: Float
        neighbors8 =
          [ (-1, -1), (0, -1), (1, -1)
          , (-1,  0),          (1,  0)
          , (-1,  1), (0,  1), (1,  1)
          ]
        baseWorkers = max 1 (min height (effectiveParallelism cfg.parallelism))
        pixelCount = max 0 (width * height)
        workers = if pixelCount < msdfParallelPixelsMin then 1 else baseWorkers
        chunk = max 1 ((height + workers - 1) `div` workers)
        parallelForRows action =
          if workers <= 1
          then action 0 (height - 1)
          else do
            mvars <- forM [0 .. workers - 1] $ \w -> do
              let y0 = w * chunk
                  y1 = min (height - 1) ((w + 1) * chunk - 1)
              mv <- newEmptyMVar
              if y0 > y1
                then putMVar mv ()
                else do
                  _ <- forkIO (action y0 y1 >> putMVar mv ())
                  pure ()
              pure mv
            mapM_ takeMVar mvars
        loopY y0 yMax f =
          let go y
                | y > yMax = pure ()
                | otherwise = f y >> go (y + 1)
          in go y0
        loopX x0 xMax f =
          let go x
                | x > xMax = pure ()
                | otherwise = f x >> go (x + 1)
          in go x0
        timingEnabled = msdfRenderTimingsEnabled
        timeMs = (/ 1.0e6) . fromIntegral
        timeIO :: IO a -> IO (a, Double)
        timeIO action =
          if not timingEnabled
          then do
            r <- action
            pure (r, 0 :: Double)
          else do
            t0 <- getMonotonicTimeNSec
            r <- action
            t1 <- getMonotonicTimeNSec
            pure (r, timeMs (t1 - t0))
        timePure :: a -> IO (a, Double)
        timePure val =
          if not timingEnabled
          then pure (val, 0 :: Double)
          else do
            t0 <- getMonotonicTimeNSec
            val `seq` pure ()
            t1 <- getMonotonicTimeNSec
            pure (val, timeMs (t1 - t0))
    dRArr <- newArray (0, n - 1) 0.0 :: IO (IOUArray Int Float)
    dGArr <- newArray (0, n - 1) 0.0 :: IO (IOUArray Int Float)
    dBArr <- newArray (0, n - 1) 0.0 :: IO (IOUArray Int Float)
    dAllArr <- newArray (0, n - 1) 0.0 :: IO (IOUArray Int Float)
    (insideArr, tInside) <- timeIO $
      case cfg.distance.signMode of
        SignScanline -> Just <$> buildScanlineInside cfg.distance.fillRule width height offsetX offsetY segs
        SignWinding -> pure Nothing
    let coloredInfosFiltered = coloredInfos
        bbFallback = BBox offsetX offsetY (offsetX + fromIntegral width) (offsetY + fromIntegral height)
        bb = case bboxFromEdgeInfos (map snd coloredInfosFiltered) of
          Just b -> b
          Nothing -> bbFallback
        cellSz =
          let base = if cfg.distance.gridCellSize > 0
                     then cfg.distance.gridCellSize
                     else fromIntegral (max 2 cfg.range)
              edgeCount = length coloredInfosFiltered
              areaPix = fromIntegral (max 1 (width * height)) :: Double
              targetEdges = 8.0 :: Double
              heuristic =
                if edgeCount <= 0
                then base
                else sqrt (areaPix * targetEdges / fromIntegral edgeCount)
              maxCell = fromIntegral (max width height)
              chosen = max base (min maxCell heuristic)
          in max 1 chosen
        flatness = cfg.outline.windingFlatness
        pixelBB = BBox offsetX offsetY (offsetX + fromIntegral width) (offsetY + fromIntegral height)
        clipBB = bboxExpand (fromIntegral (max 1 cfg.range)) pixelBB
        useOverlap =
          cfg.distance.overlapSupport
          && case insideArr of
               Just _ -> True
               Nothing -> False
        edgesCount = length coloredInfosFiltered
        coarseFactor = 4 :: Double
        coarseCellSz = cellSz * coarseFactor
        coarseThresholdPixels = 128 * 128
        coarseEdgeThreshold = 200
        useCoarse =
          coarseFactor > 1
          && width * height >= coarseThresholdPixels
          && edgesCount >= coarseEdgeThreshold
          && (fromIntegral (max width height) > coarseCellSz * 2)
    (idxAllInfo, tIndex) <- timePure (buildEdgeIndexInfo flatness cellSz bb clipBB useOverlap coloredInfosFiltered)
    (idxCoarse, tCoarse) <- timePure $
      if useCoarse
      then Just (buildEdgeIndexInfo flatness coarseCellSz bb clipBB useOverlap coloredInfosFiltered)
      else Nothing
    let insideAtIdx ix iy =
          case insideArr of
            Nothing -> pure False
            Just arr ->
              if ix < 0 || ix >= width || iy < 0 || iy >= height
              then pure False
              else do
                insideMaskAt arr (iy * width + ix)
        insideAtPosXY px py =
          let ix = floor (px - offsetX)
              iy = floor (py - offsetY)
          in insideAtIdx ix iy
        boundaryOk idx' i t =
          case insideArr of
            Nothing -> pure True
            Just _ ->
              if not cfg.distance.overlapSupport
              then pure True
              else
                let (nx, ny) = edgeUnitNormalAtIdx idx' i t
                    (qx, qy) = edgePointAtIdx idx' i t
                    base = max 0.5 cfg.distance.overlapEpsilon
                    step1 = base
                    step2 = base * 2
                    applyRule = applyFillRule cfg.distance.fillRule
                    off = idx'.segOffsets ! i
                    cnt = idx'.segCounts ! i
                    check step = do
                      let !dx = step * nx
                          !dy = step * ny
                          !q1x = qx + dx
                          !q1y = qy + dy
                          !q2x = qx - dx
                          !q2y = qy - dy
                      i1 <- insideAtPosXY q1x q1y
                      i2 <- insideAtPosXY q2x q2y
                      if i1 == i2
                        then pure False
                        else if idx'.edgeOverlaps ! i == 0
                          then pure True
                          else do
                            let q1 = (q1x, q1y)
                                q2 = (q2x, q2y)
                                w1 = windingNumber segs q1 - windingNumberSlice idx' off cnt q1
                                w2 = windingNumber segs q2 - windingNumberSlice idx' off cnt q2
                            pure (applyRule w1 /= applyRule w2)
                in if nearZero (nx, ny)
                   then pure True
                   else do
                     ok1 <- check step1
                     if ok1 then pure True else check step2
        minDistancesM usePseudo idx p =
          let edgeCount = idx.edgeCount
              max4 a b c d = max a (max b (max c d))
          in if edgeCount <= 0
             then pure (1e9, 1e9, 1e9, 1e9)
             else do
               let (cx, cy) = pointCell idx p
                   maxR = max idx.gridW idx.gridH
                   scanCellM idx' p' usePseudo' (!bestR, !bestG, !bestB, !bestAll) (x, y) =
                     let cellIdx = cellIndex idx'.gridW x y
                         start = idx'.cellStarts ! cellIdx
                         count = idx'.cellCounts ! cellIdx
                         !bestMax = max4 bestR bestG bestB bestAll
                         !bestMaxSq = bestMax * bestMax
                         cellMinSq = cellDistanceSq idx' p' (x, y)
                         scanCellEdges !curR !curG !curB !curA !curMax !curMaxSq !j
                           | j >= count = pure (curR, curG, curB, curA)
                           | otherwise =
                               let i = idx'.cellEdges ! (start + j)
                                   dSqBB = bboxDistanceSqIdx idx' i p'
                               in
                               if dSqBB >= curMaxSq
                               then scanCellEdges curR curG curB curA curMax curMaxSq (j + 1)
                               else do
                                 let mask = idx'.edgeMasks ! i
                                     (dSq, t) = edgeDistanceSqWithParamFastIdx idx' i p'
                                     dTrue = sqrt dSq
                                     dPseudo = if usePseudo' then edgeDistancePseudoFromIdx idx' i p' dTrue t else dTrue
                                     wants =
                                       (mask .&. 1 /= 0 && dPseudo < curR)
                                       || (mask .&. 2 /= 0 && dPseudo < curG)
                                       || (mask .&. 4 /= 0 && dPseudo < curB)
                                       || dTrue < curA
                                 if not wants
                                   then scanCellEdges curR curG curB curA curMax curMaxSq (j + 1)
                                   else do
                                     ok <- if useOverlap
                                       then boundaryOk idx' i t
                                       else pure True
                                     if not ok
                                       then scanCellEdges curR curG curB curA curMax curMaxSq (j + 1)
                                       else do
                                         let !nextR = if mask .&. 1 /= 0 && dPseudo < curR then dPseudo else curR
                                             !nextG = if mask .&. 2 /= 0 && dPseudo < curG then dPseudo else curG
                                             !nextB = if mask .&. 4 /= 0 && dPseudo < curB then dPseudo else curB
                                             !nextA = if dTrue < curA then dTrue else curA
                                             !nextMax = max4 nextR nextG nextB nextA
                                             !nextMaxSq = nextMax * nextMax
                                         scanCellEdges nextR nextG nextB nextA nextMax nextMaxSq (j + 1)
                     in
                     if cellMinSq >= bestMaxSq
                     then pure (bestR, bestG, bestB, bestAll)
                     else scanCellEdges bestR bestG bestB bestAll bestMax bestMaxSq 0
                   scanRing minX maxX minY maxY r best0 = do
                     let ringBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) p
                         (r0, g0, b0, a0) = best0
                         bestMax0 = max4 r0 g0 b0 a0
                     if bestMax0 <= ringBoundary
                       then pure best0
                       else do
                         let goY y bestY
                               | y > maxY = pure bestY
                               | otherwise = do
                                   let goX x bestX
                                         | x > maxX = pure bestX
                                         | r == 0 || x == minX || x == maxX || y == minY || y == maxY = do
                                             bestX' <- scanCellM idx p usePseudo bestX (x, y)
                                             goX (x + 1) bestX'
                                         | otherwise = goX (x + 1) bestX
                                   bestY' <- goX minX bestY
                                   goY (y + 1) bestY'
                         goY minY best0
                   expandRings r best =
                     let minX = max 0 (cx - r)
                         maxX = min (idx.gridW - 1) (cx + r)
                         minY = max 0 (cy - r)
                         maxY = min (idx.gridH - 1) (cy + r)
                     in do
                       best' <- scanRing minX maxX minY maxY r best
                       let (rBest, gBest, bBest, aBest) = best'
                           bestMax = max4 rBest gBest bBest aBest
                           minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) p
                           fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
                       if bestMax <= minBoundary
                         then pure best'
                         else if fullGrid || r >= maxR
                              then pure best'
                              else expandRings (r + 1) best'
               bestInit <-
                 case idxCoarse of
                   Nothing -> pure (1e18, 1e18, 1e18, 1e18)
                   Just idxC -> do
                     let (ccx, ccy) = pointCell idxC p
                     scanCellM idxC p usePseudo (1e18, 1e18, 1e18, 1e18) (ccx, ccy)
               expandRings 0 bestInit
        computeDistances y0 y1 = do
          loopY y0 y1 $ \y -> do
            let !py' = offsetY + fromIntegral y + 0.5 + cfg.distance.signEpsilon
                !px0 = offsetX + 0.5
                !base = y * width
                go !x !px
                  | x >= width = pure ()
                  | otherwise = do
                      let i = base + x
                          p = (px, py')
                      inside <- case insideArr of
                         Just arr -> do
                           insideMaskAt arr i
                         Nothing -> pure (windingNumber segs p /= 0)
                      let sign = if inside then 1.0 else -1.0
                      (dR, dG, dB, dAll) <-
                        minDistancesM cfg.distance.pseudoDistance idxAllInfo p
                      let dR0 = dR * sign
                          dG0 = dG * sign
                          dB0 = dB * sign
                          dAll0 = dAll * sign
                      writeArray dRArr i (realToFrac dR0)
                      writeArray dGArr i (realToFrac dG0)
                      writeArray dBArr i (realToFrac dB0)
                      writeArray dAllArr i (realToFrac dAll0)
                      go (x + 1) (px + 1)
            go 0 px0
    (_, tDistances) <- timeIO $
      parallelForRows computeDistances
    (dAllArrCorr, tCorr) <-
      if cfg.correction.enableCorrection
      then timeIO $ do
        dst <- newArray (0, n - 1) 0.0 :: IO (IOUArray Int Float)
        let computeCorr y0 y1 = do
              loopY y0 y1 $ \y -> do
                loopX 0 (width - 1) $ \x -> do
                  let i = y * width + x
                  d <- readArray dAllArr i
                  let signD :: Int
                      signD = if d >= 0 then 1 else -1
                  if abs d < edgeThreshF
                    then do
                      (negCount, posCount, total) <-
                        foldM
                          (\(nC, pC, tC) (dx, dy) ->
                            let nx = x + dx
                                ny = y + dy
                            in if nx < 0 || nx >= width || ny < 0 || ny >= height
                               then pure (nC, pC, tC)
                               else do
                                 let ni = ny * width + nx
                                 nd <- readArray dAllArr ni
                                 let s :: Int
                                     s = if nd >= 0 then 1 else -1
                                 pure (if s < 0 then (nC + 1, pC, tC + 1) else (nC, pC + 1, tC + 1))
                          ) (0 :: Int, 0 :: Int, 0 :: Int) neighbors8
                      let majoritySign
                            | negCount >= 5 = -1
                            | posCount >= 5 = 1
                            | otherwise = signD
                          d' = if total >= 5 && majoritySign /= signD
                               then fromIntegral majoritySign * abs d
                               else d
                      writeArray dst i d'
                    else writeArray dst i d
        parallelForRows computeCorr
        pure dst
      else pure (dAllArr, 0)
    (badArr, tBad) <-
      if cfg.correction.enableCorrection && width > 1 && height > 1
      then timeIO $ do
        bad <- newArray (0, n - 1) False :: IO (IOUArray Int Bool)
        let computeBad y0 y1 = do
              let yMax = min (height - 2) y1
              loopY y0 yMax $ \y -> do
                loopX 0 (width - 2) $ \x -> do
                  let i00 = y * width + x
                      i10 = i00 + 1
                      i01 = i00 + width
                      i11 = i01 + 1
                  dR00 <- readArray dRArr i00
                  dR10 <- readArray dRArr i10
                  dR01 <- readArray dRArr i01
                  dR11 <- readArray dRArr i11
                  dG00 <- readArray dGArr i00
                  dG10 <- readArray dGArr i10
                  dG01 <- readArray dGArr i01
                  dG11 <- readArray dGArr i11
                  dB00 <- readArray dBArr i00
                  dB10 <- readArray dBArr i10
                  dB01 <- readArray dBArr i01
                  dB11 <- readArray dBArr i11
                  dAll00 <- readArray dAllArrCorr i00
                  dAll10 <- readArray dAllArrCorr i10
                  dAll01 <- readArray dAllArrCorr i01
                  dAll11 <- readArray dAllArrCorr i11
                  let dR = 0.25 * (dR00 + dR10 + dR01 + dR11)
                      dG = 0.25 * (dG00 + dG10 + dG01 + dG11)
                      dB = 0.25 * (dB00 + dB10 + dB01 + dB11)
                      dAll = 0.25 * (dAll00 + dAll10 + dAll01 + dAll11)
                      sd = median3F dR dG dB
                      mismatch = sd * dAll < 0 || abs (sd - dAll) > chanThreshF
                      hard = abs (dR - dAll) > hardThreshF || abs (dG - dAll) > hardThreshF || abs (dB - dAll) > hardThreshF
                  if abs dAll < edgeThreshF && (mismatch || hard)
                    then do
                      writeArray bad i00 True
                      writeArray bad i10 True
                      writeArray bad i01 True
                      writeArray bad i11 True
                    else pure ()
        parallelForRows computeBad
        pure (Just bad)
      else pure (Nothing, 0)
    out <- newArray (0, n * channels - 1) 0 :: IO (IOUArray Int Word8)
    let computeOutNoCorrection y0 y1 = do
          loopY y0 y1 $ \y -> do
            loopX 0 (width - 1) $ \x -> do
              let i = y * width + x
                  base = i * channels
              dR0 <- readArray dRArr i
              dG0 <- readArray dGArr i
              dB0 <- readArray dBArr i
              let r = distanceToByteF range dR0
                  g = distanceToByteF range dG0
                  b = distanceToByteF range dB0
              case fmt of
                BitmapMSDF -> do
                  writeArray out base r
                  writeArray out (base + 1) g
                  writeArray out (base + 2) b
                BitmapMTSDF -> do
                  dAllRaw <- readArray dAllArr i
                  writeArray out base r
                  writeArray out (base + 1) g
                  writeArray out (base + 2) b
                  writeArray out (base + 3) (distanceToByteF range dAllRaw)
        computeOutCorrection y0 y1 = do
          loopY y0 y1 $ \y -> do
            loopX 0 (width - 1) $ \x -> do
              let i = y * width + x
                  base = i * channels
              dR0 <- readArray dRArr i
              dG0 <- readArray dGArr i
              dB0 <- readArray dBArr i
              dAllCorr <- readArray dAllArrCorr i
              dAllRaw <- readArray dAllArr i
              let nearEdge = abs dAllCorr < edgeThreshF
                  (dR1, dG1, dB1) =
                    if nearEdge
                    then ( clampChannelF dR0 dAllCorr chanThreshF
                         , clampChannelF dG0 dAllCorr chanThreshF
                         , clampChannelF dB0 dAllCorr chanThreshF
                         )
                    else (dR0, dG0, dB0)
                  sd = median3F dR1 dG1 dB1
                  mismatch = sd * dAllCorr < 0 || abs (sd - dAllCorr) > chanThreshF
                  hard = abs (dR1 - dAllCorr) > hardThreshF || abs (dG1 - dAllCorr) > hardThreshF || abs (dB1 - dAllCorr) > hardThreshF
              clash <- if nearEdge
                then do
                  let center = (dR0, dG0, dB0)
                      threshold = hardThreshF
                  foldM
                    (\acc (dx, dy) ->
                      if acc
                      then pure True
                      else do
                        let nx = x + dx
                            ny = y + dy
                        if nx < 0 || nx >= width || ny < 0 || ny >= height
                          then pure False
                          else do
                            let ni = ny * width + nx
                            nr <- readArray dRArr ni
                            ng <- readArray dGArr ni
                            nb <- readArray dBArr ni
                            pure (detectClashF center (nr, ng, nb) threshold)
                    ) False neighbors8
                else pure False
              bad <- case badArr of
                Just arr -> readArray arr i
                Nothing -> pure False
              let (dR, dG, dB) =
                    if nearEdge && (mismatch || hard || clash || bad)
                    then (dAllCorr, dAllCorr, dAllCorr)
                    else (dR1, dG1, dB1)
                  r = distanceToByteF range dR
                  g = distanceToByteF range dG
                  b = distanceToByteF range dB
              case fmt of
                BitmapMSDF -> do
                  writeArray out base r
                  writeArray out (base + 1) g
                  writeArray out (base + 2) b
                BitmapMTSDF -> do
                  writeArray out base r
                  writeArray out (base + 1) g
                  writeArray out (base + 2) b
                  writeArray out (base + 3) (distanceToByteF range dAllRaw)
    (_, tOut) <- timeIO $
      parallelForRows (if cfg.correction.enableCorrection then computeOutCorrection else computeOutNoCorrection)
    when timingEnabled $
      printf "msdf: renderBitmap %dx%d edges=%d inside=%.2f idx=%.2f coarse=%.2f dist=%.2f corr=%.2f bad=%.2f out=%.2f\n"
        width height edgesCount tInside tIndex tCoarse tDistances tCorr tBad tOut
    freeze out

{-# NOINLINE renderBitmap #-}

bboxFromEdgeInfos :: [EdgeInfo] -> Maybe BBox
bboxFromEdgeInfos [] = Nothing
bboxFromEdgeInfos (e:es) =
  let bb0 = edgeBBox e.edge
  in Just (foldl mergeBBox bb0 es)
  where
    mergeBBox acc info = bboxUnionSimple acc (edgeBBox info.edge)

bboxUnionSimple :: BBox -> BBox -> BBox
bboxUnionSimple a b = BBox
  { xMin = min a.xMin b.xMin
  , yMin = min a.yMin b.yMin
  , xMax = max a.xMax b.xMax
  , yMax = max a.yMax b.yMax
  }

bboxDistanceSqIdx :: EdgeIndexInfo -> Int -> Vec2 -> Double
bboxDistanceSqIdx idx i (px, py) =
  let minX = idx.edgeBBoxMinX ! i
      maxX = idx.edgeBBoxMaxX ! i
      minY = idx.edgeBBoxMinY ! i
      maxY = idx.edgeBBoxMaxY ! i
      dx
        | px < minX = minX - px
        | px > maxX = px - maxX
        | otherwise = 0
      dy
        | py < minY = minY - py
        | py > maxY = py - maxY
        | otherwise = 0
  in dx * dx + dy * dy
{-# INLINE bboxDistanceSqIdx #-}

data EdgeIndexInfo = EdgeIndexInfo
  { cellSize :: !Double
  , originX :: !Double
  , originY :: !Double
  , gridW :: !Int
  , gridH :: !Int
  , edgeCount :: !Int
  , cellStarts :: !(UA.UArray Int Int)
  , cellCounts :: !(UA.UArray Int Int)
  , cellEdges :: !(UA.UArray Int Int)
  , edgeBBoxMinX :: !(UA.UArray Int Double)
  , edgeBBoxMinY :: !(UA.UArray Int Double)
  , edgeBBoxMaxX :: !(UA.UArray Int Double)
  , edgeBBoxMaxY :: !(UA.UArray Int Double)
  , segOffsets :: !(UA.UArray Int Int)
  , segCounts :: !(UA.UArray Int Int)
  , segX0 :: !(UA.UArray Int Double)
  , segY0 :: !(UA.UArray Int Double)
  , segX1 :: !(UA.UArray Int Double)
  , segY1 :: !(UA.UArray Int Double)
  , edgeOverlaps :: !(UA.UArray Int Word8)
  , edgeMasks :: !(UA.UArray Int Word8)
  , edgeKind :: !(UA.UArray Int Word8)
  , edgeStartX :: !(UA.UArray Int Double)
  , edgeStartY :: !(UA.UArray Int Double)
  , edgeEndX :: !(UA.UArray Int Double)
  , edgeEndY :: !(UA.UArray Int Double)
  , edgePseudoStartX :: !(UA.UArray Int Double)
  , edgePseudoStartY :: !(UA.UArray Int Double)
  , edgePseudoEndX :: !(UA.UArray Int Double)
  , edgePseudoEndY :: !(UA.UArray Int Double)
  , lineDx :: !(UA.UArray Int Double)
  , lineDy :: !(UA.UArray Int Double)
  , lineInvLen2 :: !(UA.UArray Int Double)
  , quadAx :: !(UA.UArray Int Double)
  , quadAy :: !(UA.UArray Int Double)
  , quadBx :: !(UA.UArray Int Double)
  , quadBy :: !(UA.UArray Int Double)
  , quadC3 :: !(UA.UArray Int Double)
  , quadC2 :: !(UA.UArray Int Double)
  , quadC1base :: !(UA.UArray Int Double)
  }


buildEdgeIndexInfo :: Double -> Double -> BBox -> BBox -> Bool -> [(EdgeColor, EdgeInfo)] -> EdgeIndexInfo
buildEdgeIndexInfo flatness cellSize' bb clipBB0 buildSegs coloredEdges =
  let cellSize'' = max 1 cellSize'
      clipBB = bboxExpand 0 clipBB0
      gridBB = case bboxIntersect bb clipBB of
        Just b -> b
        Nothing -> bb
      width = (max 1 (ceiling (gridBB.xMax - gridBB.xMin) + 1) :: Int)
      height = (max 1 (ceiling (gridBB.yMax - gridBB.yMin) + 1) :: Int)
      filteredEdges =
        [ (c, e)
        | (c, e) <- coloredEdges
        , bboxOverlaps (edgeBBox e.edge) clipBB
        ]
      edgesList = map snd filteredEdges
      gridW' = max 1 (ceiling (fromIntegral width / cellSize''))
      gridH' = max 1 (ceiling (fromIntegral height / cellSize''))
      edgeCount = length edgesList
      edgeBBoxArr =
        if null edgesList
        then array (0, -1) []
        else array (0, edgeCount - 1)
          [ (i, edgeBBox info.edge) | (i, info) <- zip [0 ..] edgesList ]
      edgeBBoxMinXArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1)
          [ (edgeBBoxArr ! i).xMin | i <- [0 .. edgeCount - 1] ]
      edgeBBoxMinYArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1)
          [ (edgeBBoxArr ! i).yMin | i <- [0 .. edgeCount - 1] ]
      edgeBBoxMaxXArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1)
          [ (edgeBBoxArr ! i).xMax | i <- [0 .. edgeCount - 1] ]
      edgeBBoxMaxYArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1)
          [ (edgeBBoxArr ! i).yMax | i <- [0 .. edgeCount - 1] ]
      edgeClipArr =
        if null edgesList
        then array (0, -1) []
        else array (0, edgeCount - 1)
          [ (i, bboxIntersect (edgeBBoxArr ! i) clipBB) | i <- [0 .. edgeCount - 1] ]
      maskArr =
        if null filteredEdges
        then UA.listArray (0, -1) []
        else UA.listArray (0, length filteredEdges - 1)
          [ colorMask c | (c, _) <- filteredEdges ]
      segsPerEdge =
        if not buildSegs
        then replicate edgeCount []
        else [ if info.overlaps then flattenEdges flatness [info.edge] else []
             | info <- edgesList
             ]
      segCountsList =
        if null edgesList
        then []
        else map length segsPerEdge
      segOffsetsList =
        if null edgesList
        then []
        else scanl (+) 0 segCountsList
      totalSegs =
        if null segOffsetsList
        then 0
        else last segOffsetsList
      segOffsetsArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1) (take edgeCount segOffsetsList)
      segCountsArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1) segCountsList
      (segX0Arr, segY0Arr, segX1Arr, segY1Arr) =
        if totalSegs <= 0
        then (UA.listArray (0, -1) [], UA.listArray (0, -1) [], UA.listArray (0, -1) [], UA.listArray (0, -1) [])
        else runST $ do
          x0Arr <- (newArray (0, totalSegs - 1) 0.0 :: ST s (STUArray s Int Double))
          y0Arr <- (newArray (0, totalSegs - 1) 0.0 :: ST s (STUArray s Int Double))
          x1Arr <- (newArray (0, totalSegs - 1) 0.0 :: ST s (STUArray s Int Double))
          y1Arr <- (newArray (0, totalSegs - 1) 0.0 :: ST s (STUArray s Int Double))
          forM_ (zip [0 ..] segsPerEdge) $ \(i, segs) -> do
            let off = segOffsetsArr ! i
            forM_ (zip [0 ..] segs) $ \(j, ((sx, sy), (ex, ey))) -> do
              let k = off + j
              writeArray x0Arr k sx
              writeArray y0Arr k sy
              writeArray x1Arr k ex
              writeArray y1Arr k ey
          x0' <- freeze x0Arr
          y0' <- freeze y0Arr
          x1' <- freeze x1Arr
          y1' <- freeze y1Arr
          pure (x0', y0', x1', y1')
      edgeOverlapsArr =
        if null edgesList
        then UA.listArray (0, -1) []
        else UA.listArray (0, edgeCount - 1)
          [ if info.overlaps then 1 else 0 | info <- edgesList ]
      (edgeKindArr, edgeStartXArr, edgeStartYArr, edgeEndXArr, edgeEndYArr
        , edgePseudoStartXArr, edgePseudoStartYArr, edgePseudoEndXArr, edgePseudoEndYArr
        , lineDxArr, lineDyArr, lineInvLen2Arr
        , quadAxArr, quadAyArr, quadBxArr, quadByArr, quadC3Arr, quadC2Arr, quadC1baseArr) =
          buildEdgeParamArrays edgesList
      cellCountArr =
        let cellCount = gridW' * gridH'
        in runSTUArray $ do
             arr <- (newArray (0, cellCount - 1) 0 :: ST s (STUArray s Int Int))
             forM_ [0 .. edgeCount - 1] $ \i -> do
               case edgeClipArr ! i of
                 Nothing -> pure ()
                 Just bbE -> do
                   let x0 = clamp 0 (gridW' - 1) (toCell gridBB.xMin cellSize'' bbE.xMin)
                       x1 = clamp 0 (gridW' - 1) (toCell gridBB.xMin cellSize'' bbE.xMax)
                       y0 = clamp 0 (gridH' - 1) (toCell gridBB.yMin cellSize'' bbE.yMin)
                       y1 = clamp 0 (gridH' - 1) (toCell gridBB.yMin cellSize'' bbE.yMax)
                   forM_ [y0 .. y1] $ \y -> do
                     forM_ [x0 .. x1] $ \x -> do
                       let idx = cellIndex gridW' x y
                       cur <- readArray arr idx
                       writeArray arr idx (cur + 1)
             pure arr
      (cellStartsArr, totalEdges) = buildCellStarts cellCountArr
      cellEdgesArr =
        if totalEdges <= 0
        then UA.listArray (0, -1) []
        else runSTUArray $ do
          arr <- (newArray (0, totalEdges - 1) 0 :: ST s (STUArray s Int Int))
          posArr <- (thaw cellStartsArr :: ST s (STUArray s Int Int))
          forM_ [0 .. edgeCount - 1] $ \i -> do
            case edgeClipArr ! i of
              Nothing -> pure ()
              Just bbE -> do
                let x0 = clamp 0 (gridW' - 1) (toCell gridBB.xMin cellSize'' bbE.xMin)
                    x1 = clamp 0 (gridW' - 1) (toCell gridBB.xMin cellSize'' bbE.xMax)
                    y0 = clamp 0 (gridH' - 1) (toCell gridBB.yMin cellSize'' bbE.yMin)
                    y1 = clamp 0 (gridH' - 1) (toCell gridBB.yMin cellSize'' bbE.yMax)
                forM_ [y0 .. y1] $ \y -> do
                  forM_ [x0 .. x1] $ \x -> do
                    let idx = cellIndex gridW' x y
                    pos <- readArray posArr idx
                    writeArray arr pos i
                    writeArray posArr idx (pos + 1)
          pure arr
  in EdgeIndexInfo
       { cellSize = cellSize''
       , originX = gridBB.xMin
       , originY = gridBB.yMin
       , gridW = gridW'
       , gridH = gridH'
       , edgeCount = edgeCount
       , cellStarts = cellStartsArr
       , cellCounts = cellCountArr
       , cellEdges = cellEdgesArr
       , edgeBBoxMinX = edgeBBoxMinXArr
       , edgeBBoxMinY = edgeBBoxMinYArr
       , edgeBBoxMaxX = edgeBBoxMaxXArr
       , edgeBBoxMaxY = edgeBBoxMaxYArr
       , segOffsets = segOffsetsArr
       , segCounts = segCountsArr
       , segX0 = segX0Arr
       , segY0 = segY0Arr
       , segX1 = segX1Arr
       , segY1 = segY1Arr
       , edgeOverlaps = edgeOverlapsArr
       , edgeMasks = maskArr
       , edgeKind = edgeKindArr
       , edgeStartX = edgeStartXArr
       , edgeStartY = edgeStartYArr
       , edgeEndX = edgeEndXArr
       , edgeEndY = edgeEndYArr
       , edgePseudoStartX = edgePseudoStartXArr
       , edgePseudoStartY = edgePseudoStartYArr
       , edgePseudoEndX = edgePseudoEndXArr
       , edgePseudoEndY = edgePseudoEndYArr
       , lineDx = lineDxArr
       , lineDy = lineDyArr
       , lineInvLen2 = lineInvLen2Arr
       , quadAx = quadAxArr
       , quadAy = quadAyArr
       , quadBx = quadBxArr
       , quadBy = quadByArr
       , quadC3 = quadC3Arr
       , quadC2 = quadC2Arr
       , quadC1base = quadC1baseArr
       }

buildEdgeParamArrays
  :: [EdgeInfo]
  -> ( UA.UArray Int Word8
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     , UA.UArray Int Double
     )
buildEdgeParamArrays edgesList =
  if null edgesList
  then
    ( UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    , UA.listArray (0, -1) []
    )
  else
    let n = length edgesList
        bounds' = (0, n - 1)
    in runST $ do
      kindArr <- newArray bounds' 0 :: ST s (STUArray s Int Word8)
      sxArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      syArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      exArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      eyArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      psxArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      psyArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      pexArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      peyArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      ldxArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      ldyArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      linvArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qaxArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qayArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qbxArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qbyArr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qc3Arr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qc2Arr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      qc1Arr <- newArray bounds' 0 :: ST s (STUArray s Int Double)
      forM_ (zip [0 ..] edgesList) $ \(i, e) -> do
        let (sx, sy) = e.start
            (ex, ey) = e.end
            (psx, psy) = e.pseudoStart
            (pex, pey) = e.pseudoEnd
        writeArray kindArr i $
          case e.edgeData of
            EdgeLineData {} -> 0
            EdgeQuadData {} -> 1
        writeArray sxArr i sx
        writeArray syArr i sy
        writeArray exArr i ex
        writeArray eyArr i ey
        writeArray psxArr i psx
        writeArray psyArr i psy
        writeArray pexArr i pex
        writeArray peyArr i pey
        case e.edgeData of
          EdgeLineData dx dy invLen2 -> do
            writeArray ldxArr i dx
            writeArray ldyArr i dy
            writeArray linvArr i invLen2
          _ -> pure ()
        case e.edgeData of
          EdgeQuadData ax ay bx by c3 c2 c1base -> do
            writeArray qaxArr i ax
            writeArray qayArr i ay
            writeArray qbxArr i bx
            writeArray qbyArr i by
            writeArray qc3Arr i c3
            writeArray qc2Arr i c2
            writeArray qc1Arr i c1base
          _ -> pure ()
      kind <- freeze kindArr
      sx <- freeze sxArr
      sy <- freeze syArr
      ex <- freeze exArr
      ey <- freeze eyArr
      psx <- freeze psxArr
      psy <- freeze psyArr
      pex <- freeze pexArr
      pey <- freeze peyArr
      ldx <- freeze ldxArr
      ldy <- freeze ldyArr
      linv <- freeze linvArr
      qax <- freeze qaxArr
      qay <- freeze qayArr
      qbx <- freeze qbxArr
      qby <- freeze qbyArr
      qc3 <- freeze qc3Arr
      qc2 <- freeze qc2Arr
      qc1 <- freeze qc1Arr
      pure
        ( kind
        , sx
        , sy
        , ex
        , ey
        , psx
        , psy
        , pex
        , pey
        , ldx
        , ldy
        , linv
        , qax
        , qay
        , qbx
        , qby
        , qc3
        , qc2
        , qc1
        )

buildCellStarts :: UA.UArray Int Int -> (UA.UArray Int Int, Int)
buildCellStarts counts =
  runST $ do
    let (lo, hi) = bounds counts
    starts <- (newArray (lo, hi) 0 :: ST s (STUArray s Int Int))
    let go i acc
          | i > hi = pure acc
          | otherwise = do
              let c = counts ! i
              writeArray starts i acc
              go (i + 1) (acc + c)
    total <- go lo 0
    starts' <- freeze starts
    pure (starts', total)

edgeDistanceSqLineIdx :: EdgeIndexInfo -> Int -> Vec2 -> (Double, Double)
edgeDistanceSqLineIdx idx i (px, py) =
  let x0 = idx.edgeStartX ! i
      y0 = idx.edgeStartY ! i
      dx = idx.lineDx ! i
      dy = idx.lineDy ! i
      invLen2 = idx.lineInvLen2 ! i
      t = if invLen2 == 0
          then 0
          else ((px - x0) * dx + (py - y0) * dy) * invLen2
      t' = max 0 (min 1 t)
      cx = x0 + t' * dx
      cy = y0 + t' * dy
      ex = px - cx
      ey = py - cy
  in (ex * ex + ey * ey, t')
{-# INLINE edgeDistanceSqLineIdx #-}

edgeDistanceSqQuadIdx :: EdgeIndexInfo -> Int -> Vec2 -> (Double, Double)
edgeDistanceSqQuadIdx idx i (px, py) =
  let x0 = idx.edgeStartX ! i
      y0 = idx.edgeStartY ! i
      ax = idx.quadAx ! i
      ay = idx.quadAy ! i
      bx = idx.quadBx ! i
      by = idx.quadBy ! i
      c3 = idx.quadC3 ! i
      c2 = idx.quadC2 ! i
      c1base = idx.quadC1base ! i
      cx = x0 - px
      cy = y0 - py
      c1 = c1base + 2 * (ax * cx + ay * cy)
      c0 = bx * cx + by * cy
      d0 = cx * cx + cy * cy
      bx2 = bx * 0.5
      by2 = by * 0.5
      p1x = x0 + bx2
      p1y = y0 + by2
      x2 = x0 + bx + ax
      y2 = y0 + by + ay
      dx2 = x2 - px
      dy2 = y2 - py
      d1 = dx2 * dx2 + dy2 * dy2
      start = if d0 < d1 then (d0, 0) else (d1, 1)
      pick t (bestD, bestT) =
        if t >= 0 && t <= 1
        then
          let d = distanceSqBezier2 (x0, y0) (p1x, p1y) (x2, y2) (px, py) t
          in if d < bestD then (d, t) else (bestD, bestT)
        else (bestD, bestT)
  in foldCubicRoots c3 c2 c1 c0 start pick
{-# INLINE edgeDistanceSqQuadIdx #-}

edgeDistanceSqWithParamFastIdx :: EdgeIndexInfo -> Int -> Vec2 -> (Double, Double)
edgeDistanceSqWithParamFastIdx idx i p =
  if idx.edgeKind ! i == 0
  then edgeDistanceSqLineIdx idx i p
  else edgeDistanceSqQuadIdx idx i p
{-# INLINE edgeDistanceSqWithParamFastIdx #-}

edgeDistancePseudoFromIdx :: EdgeIndexInfo -> Int -> Vec2 -> Double -> Double -> Double
edgeDistancePseudoFromIdx idx i p d t =
  let eps = 1e-6
  in if t <= 0 + eps
     then pseudoAt (idx.edgeStartX ! i, idx.edgeStartY ! i)
                   (idx.edgePseudoStartX ! i, idx.edgePseudoStartY ! i)
                   p d
     else if t >= 1 - eps
          then pseudoAt (idx.edgeEndX ! i, idx.edgeEndY ! i)
                        (idx.edgePseudoEndX ! i, idx.edgePseudoEndY ! i)
                        p d
          else d
{-# INLINE edgeDistancePseudoFromIdx #-}

edgePointAtIdx :: EdgeIndexInfo -> Int -> Double -> Vec2
edgePointAtIdx idx i t =
  if idx.edgeKind ! i == 0
  then
    let x0 = idx.edgeStartX ! i
        y0 = idx.edgeStartY ! i
        dx = idx.lineDx ! i
        dy = idx.lineDy ! i
    in (x0 + t * dx, y0 + t * dy)
  else
    let x0 = idx.edgeStartX ! i
        y0 = idx.edgeStartY ! i
        ax = idx.quadAx ! i
        ay = idx.quadAy ! i
        bx = idx.quadBx ! i
        by = idx.quadBy ! i
        tt = t * t
    in (x0 + bx * t + ax * tt, y0 + by * t + ay * tt)
{-# INLINE edgePointAtIdx #-}

edgeTangentAtIdx :: EdgeIndexInfo -> Int -> Double -> Vec2
edgeTangentAtIdx idx i t =
  if idx.edgeKind ! i == 0
  then (idx.lineDx ! i, idx.lineDy ! i)
  else
    let ax = idx.quadAx ! i
        ay = idx.quadAy ! i
        bx = idx.quadBx ! i
        by = idx.quadBy ! i
    in (bx + 2 * ax * t, by + 2 * ay * t)
{-# INLINE edgeTangentAtIdx #-}

edgeUnitNormalAtIdx :: EdgeIndexInfo -> Int -> Double -> Vec2
edgeUnitNormalAtIdx idx i t =
  let (dx, dy) = edgeTangentAtIdx idx i t
      n = normalizeVec (-dy, dx)
  in if nearZero n
     then
       let ps = (idx.edgePseudoStartX ! i, idx.edgePseudoStartY ! i)
           pe = (idx.edgePseudoEndX ! i, idx.edgePseudoEndY ! i)
           blend = vecAdd (vecScale (1 - t) ps) (vecScale t pe)
       in normalizeVec blend
     else n
{-# INLINE edgeUnitNormalAtIdx #-}

distanceSqBezier2 :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double -> Double
distanceSqBezier2 (x0, y0) (x1, y1) (x2, y2) (px, py) t =
  let u = 1 - t
      uu = u * u
      tt = t * t
      x = uu * x0 + 2 * u * t * x1 + tt * x2
      y = uu * y0 + 2 * u * t * y1 + tt * y2
      dx = x - px
      dy = y - py
  in dx * dx + dy * dy

foldCubicRoots :: Double -> Double -> Double -> Double -> r -> (Double -> r -> r) -> r
foldCubicRoots a b c d z f
  | abs a < 1e-12 = foldQuadraticRoots b c d z f
  | otherwise =
      let a' = b / a
          b' = c / a
          c' = d / a
          p = b' - a' * a' / 3
          q = 2 * a' * a' * a' / 27 - a' * b' / 3 + c'
          disc = (q * q) / 4 + (p * p * p) / 27
      in if disc > 0
         then
           let sqrtDisc = sqrt disc
               u = cbrt (-q / 2 + sqrtDisc)
               v = cbrt (-q / 2 - sqrtDisc)
               t = u + v - a' / 3
           in f t z
         else if abs disc < 1e-12
              then
                let u = cbrt (-q / 2)
                    t1 = 2 * u - a' / 3
                    t2 = -u - a' / 3
                in f t2 (f t1 z)
              else
                let r = sqrt (-(p * p * p) / 27)
                    phi = acos (-q / (2 * r))
                    t = 2 * cbrt r
                    t1 = t * cos (phi / 3) - a' / 3
                    t2 = t * cos ((phi + 2 * pi) / 3) - a' / 3
                    t3 = t * cos ((phi + 4 * pi) / 3) - a' / 3
                in f t3 (f t2 (f t1 z))

foldQuadraticRoots :: Double -> Double -> Double -> r -> (Double -> r -> r) -> r
foldQuadraticRoots a b c z f
  | abs a < 1e-12 = foldLinearRoots b c z f
  | otherwise =
      let disc = b * b - 4 * a * c
      in if disc < 0
         then z
         else
           let sqrtDisc = sqrt disc
               t1 = (-b + sqrtDisc) / (2 * a)
               t2 = (-b - sqrtDisc) / (2 * a)
           in f t2 (f t1 z)

foldLinearRoots :: Double -> Double -> r -> (Double -> r -> r) -> r
foldLinearRoots a b z f
  | abs a < 1e-12 = z
  | otherwise = f (-b / a) z

cbrt :: Double -> Double
cbrt x = if x < 0 then -((abs x) ** (1 / 3)) else x ** (1 / 3)

pseudoAt :: Vec2 -> Vec2 -> Vec2 -> Double -> Double
pseudoAt origin n p fallback =
  if nearZero n
  then fallback
  else
    let v = vecSub p origin
        proj = abs (dot v n)
    in min proj fallback

distanceToBoundary :: EdgeIndexInfo -> (Int, Int) -> (Int, Int) -> Vec2 -> Double
distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py') =
  let x0 = idx.originX + fromIntegral minX * idx.cellSize
      x1 = idx.originX + fromIntegral (maxX + 1) * idx.cellSize
      y0 = idx.originY + fromIntegral minY * idx.cellSize
      y1 = idx.originY + fromIntegral (maxY + 1) * idx.cellSize
      dx = min (px' - x0) (x1 - px')
      dy = min (py' - y0) (y1 - py')
  in max 0 (min dx dy)
{-# INLINE distanceToBoundary #-}

cellDistanceSq :: EdgeIndexInfo -> Vec2 -> (Int, Int) -> Double
cellDistanceSq idx (px', py') (cx, cy) =
  let x0 = idx.originX + fromIntegral cx * idx.cellSize
      x1 = x0 + idx.cellSize
      y0 = idx.originY + fromIntegral cy * idx.cellSize
      y1 = y0 + idx.cellSize
      dx
        | px' < x0 = x0 - px'
        | px' > x1 = px' - x1
        | otherwise = 0
      dy
        | py' < y0 = y0 - py'
        | py' > y1 = py' - y1
        | otherwise = 0
  in dx * dx + dy * dy
{-# INLINE cellDistanceSq #-}

pointCell :: EdgeIndexInfo -> Vec2 -> (Int, Int)
pointCell idx (px', py') =
  let cx = clamp 0 (idx.gridW - 1) (toCell idx.originX idx.cellSize px')
      cy = clamp 0 (idx.gridH - 1) (toCell idx.originY idx.cellSize py')
  in (cx, cy)

toCell :: Double -> Double -> Double -> Int
toCell origin size v =
  floor ((v - origin) / size)

cellIndex :: Int -> Int -> Int -> Int
cellIndex gridW' x y = y * gridW' + x

clamp :: Int -> Int -> Int -> Int
clamp lo hi v = max lo (min hi v)

buildScanlineInside
  :: FillRule
  -> Int
  -> Int
  -> Double
  -> Double
  -> [LineSeg]
  -> IO (IOUArray Int Word8)
buildScanlineInside rule width height offsetX offsetY segs = do
  let n = width * height
      bytes = insideMaskBytes n
  arr <-
    if bytes <= 0
    then newArray (0, -1) 0
    else newArray (0, bytes - 1) 0 :: IO (IOUArray Int Word8)
  let halfOpen y0 y1 py = (y0 <= py && y1 > py) || (y1 <= py && y0 > py)
      rowInside py =
        let hits =
              [ let t = (py - y0) / (y1 - y0)
                    x = x0 + t * (x1 - x0)
                    delta :: Int
                    delta = if y1 > y0 then 1 else -1
                in (x, delta)
              | ((x0, y0), (x1, y1)) <- segs
              , abs (y1 - y0) > 1e-12
              , halfOpen y0 y1 py
              ]
            sorted = sortOn fst hits
        in sorted
      applyRule = applyFillRule rule
  forM_ [0 .. height - 1] $ \y -> do
    let py = offsetY + fromIntegral y + 0.5
        hits = rowInside py
    let go _ [] (_w :: Int) = pure ()
        go xIdx rest (w :: Int)
          | xIdx >= width = pure ()
          | otherwise = do
              let px = offsetX + fromIntegral xIdx + 0.5
                  (passed, remaining) = span (\(xHit, _) -> xHit <= px) rest
                  w' = w + sum (map snd passed)
                  inside = applyRule w'
              when inside $
                setInsideBit arr (y * width + xIdx)
              go (xIdx + 1) remaining w'
    go 0 hits 0
  pure arr

{-# INLINE insideMaskBytes #-}
insideMaskBytes :: Int -> Int
insideMaskBytes n =
  if n <= 0 then 0 else (n + 7) `div` 8

{-# INLINE insideMaskAt #-}
insideMaskAt :: IOUArray Int Word8 -> Int -> IO Bool
insideMaskAt arr idx = do
  let byte = idx `shiftR` 3
      bit = idx .&. 7
      mask = (1 `shiftL` bit) :: Int
  v <- readArray arr byte
  pure ((v .&. fromIntegral mask) /= 0)

{-# INLINE setInsideBit #-}
setInsideBit :: IOUArray Int Word8 -> Int -> IO ()
setInsideBit arr idx = do
  let byte = idx `shiftR` 3
      bit = idx .&. 7
      mask = (1 `shiftL` bit) :: Int
  cur <- readArray arr byte
  writeArray arr byte (cur .|. fromIntegral mask)

{-# INLINE windingNumberSlice #-}
windingNumberSlice :: EdgeIndexInfo -> Int -> Int -> Vec2 -> Int
windingNumberSlice idx off count (px', py') =
  let end = off + count
      go i wn
        | i >= end = wn
        | otherwise =
            let x0 = idx.segX0 ! i
                y0 = idx.segY0 ! i
                x1 = idx.segX1 ! i
                y1 = idx.segY1 ! i
                wn' =
                  if y0 <= py'
                  then if y1 > py' && isLeft (x0, y0) (x1, y1) (px', py') > 0
                       then wn + 1
                       else wn
                  else if y1 <= py' && isLeft (x0, y0) (x1, y1) (px', py') < 0
                       then wn - 1
                       else wn
            in go (i + 1) wn'
  in go off 0

{-# INLINE isLeft #-}
isLeft :: Vec2 -> Vec2 -> Vec2 -> Double
isLeft (x0, y0) (x1, y1) (px', py') =
  (x1 - x0) * (py' - y0) - (px' - x0) * (y1 - y0)

applyFillRule :: FillRule -> Int -> Bool
applyFillRule rule w =
  case rule of
    FillNonZero -> w /= 0
    FillOdd -> odd w
    FillPositive -> w > 0
    FillNegative -> w < 0

edgeIsBoundary :: FillRule -> Double -> Double -> [LineSeg] -> [[LineSeg]] -> Bool -> EdgeInfo -> Bool
edgeIsBoundary rule eps _flatness segsAll _segsByContour useContour info =
  let edge = info.edge
      len = edgeLengthApprox edge
      -- Use a minimum pixel step so tiny contours (dots) don't get filtered out.
      base = max eps (max 0.5 (len * 1e-2))
      steps = [base, base * 2]
      samples = case edge of
        EdgeLine _ _ -> [0.5]
        EdgeQuad _ _ _ -> [0.25, 0.5, 0.75]
      applyRule = applyFillRule rule
      windingAt p = windingNumber segsAll p
      check t =
        let p = edgePointAt edge t
            n = edgeUnitNormalAt info t
        in if nearZero n
           then False
           else
             any
               (\step ->
                 let p1 = vecAdd p (vecScale step n)
                     p2 = vecSub p (vecScale step n)
                     w1 = windingAt p1
                     w2 = windingAt p2
                 in applyRule w1 /= applyRule w2
               )
               steps
  in if useContour
     then any check samples
     else if not info.overlaps
       then True
       else any check samples

filterBoundaryEdges
  :: FillRule
  -> Double
  -> Double
  -> [LineSeg]
  -> [[LineSeg]]
  -> Bool
  -> [(EdgeColor, EdgeInfo)]
  -> [(EdgeColor, EdgeInfo)]
filterBoundaryEdges rule eps flatness segsAll segsByContour useContour colored =
  let keep info = edgeIsBoundary rule eps flatness segsAll segsByContour useContour info
  in [ (c, info) | (c, info) <- colored, keep info ]


edgeUnitNormalAt :: EdgeInfo -> Double -> Vec2
edgeUnitNormalAt info t =
  let (dx, dy) = edgeTangentAt info.edge t
      n = normalizeVec (-dy, dx)
  in if nearZero n
     then
       let blend = vecAdd (vecScale (1 - t) info.pseudoStart) (vecScale t info.pseudoEnd)
       in normalizeVec blend
     else n

{-# INLINE clampChannelF #-}
clampChannelF :: Float -> Float -> Float -> Float
clampChannelF d dAll threshold =
  if abs (d - dAll) > threshold then dAll else d

colorMask :: EdgeColor -> Word8
colorMask c =
  case c of
    ColorRed -> 1
    ColorGreen -> 2
    ColorBlue -> 4
    ColorYellow -> 3
    ColorMagenta -> 5
    ColorCyan -> 6
    ColorWhite -> 7

{-# INLINE median3F #-}
median3F :: Float -> Float -> Float -> Float
median3F a b c = max (min a b) (min (max a b) c)

detectClashF :: (Float, Float, Float) -> (Float, Float, Float) -> Float -> Bool
detectClashF (ar, ag, ab) (br, bg, bb) threshold =
  let am = median3F ar ag ab
      bm = median3F br bg bb
      devsA :: [(Int, Float)]
      devsA = sortOn (Down . snd)
        [ (0, abs (ar - am))
        , (1, abs (ag - am))
        , (2, abs (ab - am))
        ]
      devsB :: [(Int, Float)]
      devsB = sortOn (Down . snd)
        [ (0, abs (br - bm))
        , (1, abs (bg - bm))
        , (2, abs (bb - bm))
        ]
      (_, aDist0) = devsA !! 0
      (aCh1, aDist1) = devsA !! 1
      (_, bDist0) = devsB !! 0
      (bCh1, bDist1) = devsB !! 1
  in aCh1 /= bCh1
     && abs ((aDist0 - aDist1) - (bDist0 - bDist1)) >= threshold
     && abs am >= abs bm

{-# INLINE distanceToByteF #-}
distanceToByteF :: Int -> Float -> Word8
distanceToByteF range dist =
  let r = fromIntegral (max 1 range) :: Float
      v = 0.5 + dist / (2 * r)
      v' = max 0 (min 1 v)
  in fromIntegral (round (v' * 255) :: Int)

safeIndex :: Array Int Int -> Int -> Int
safeIndex arr i =
  let (lo, hi) = bounds arr
  in if i < lo || i > hi then 0 else arr ! i

inBounds :: Array Int Int -> Int -> Bool
inBounds arr i =
  let (lo, hi) = bounds arr
  in i >= lo && i <= hi
