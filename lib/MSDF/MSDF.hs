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
  , prepareGlyphCache
  , prepareGlyphCacheLazy
  , renderGlyphMSDF
  , renderGlyphMSDFCached
  , renderGlyphMSDFCachedLazy
  , glyphMetricsOnly
  , glyphMetricsOnlyAt
  ) where

import Control.Monad (forM_, foldM)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, array, accumArray)
import Data.Array.IArray (bounds, (!))
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray, listArray)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List (sortOn, foldl')
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import qualified Data.IntSet as IntSet
import Data.Ord (Down(..))
import Data.Bits ((.&.))
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
import MSDF.TTF.Variations (applyGvarToContours, componentDeltas, hvarDeltas, vvarDeltas)
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
  , parallelism = msdfDefaultParallelism
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

msdfDefaultParallelism :: Int
msdfDefaultParallelism = unsafePerformIO $ do
  v <- lookupEnv "MSDF_PARALLELISM"
  let parseInt s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
  case v >>= parseInt of
    Just n | n >= 0 -> pure n
    _ -> do
      procs <- getNumProcessors
      if procs > 0 then pure procs else getNumCapabilities
{-# NOINLINE msdfDefaultParallelism #-}

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

data GlyphCache = GlyphCache
  { location :: Maybe VariationLocation
  , contours :: Array Int [[Point]]
  } deriving (Eq, Show)

data GlyphCacheLazy = GlyphCacheLazy
  { location :: Maybe VariationLocation
  , contoursRef :: IORef (IntMap.IntMap [[Point]])
  }

variationLocation :: MSDFConfig -> TTF -> Maybe VariationLocation
variationLocation cfg ttf =
  case ttf.variations of
    Nothing -> Nothing
    Just vars -> Just (normalizeLocation vars.fvar vars.avar cfg.variations)

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
                     if cfg.outline.splitIntersections
                     then preprocessContours cfg.outline cfg.distance.fillRule edgesByContour1
                     else edgesByContour1
                 edgesByContour = map (filterDegenerateEdges eps) edgesByContour2
                 edgeInfosByContour0 = traceStage "msdf: build edge infos" (buildEdgeInfos cfg.outline edgesByContour)
                 allInfos0 = concat edgeInfosByContour0
                 edgeInfosByContour =
                   if cfg.distance.overlapSupport
                   then
                     let overlapFlags = traceStage "msdf: compute overlaps" (computeEdgeOverlaps cfg.outline.windingFlatness cfg.outline.contourEpsilon allInfos0)
                         overlapMap = Map.fromList
                           [ (edgeKey info, flag)
                           | (info, flag) <- zip allInfos0 overlapFlags
                           ]
                     in traceStage "msdf: apply overlap flags" (map (map (applyOverlapFlag overlapMap)) edgeInfosByContour0)
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
                             cfg.outline.splitIntersections
                             coloredInfos0
                     in if null filtered then coloredInfos0 else filtered
                   else coloredInfos0
             in if null coloredInfos
                then base
                else
                  let padding = fromIntegral (safeRange + 1) :: Double
                      width = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding))
                      height = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding))
                      area = width * height
                      offsetX = bbox.xMin - padding
                      offsetY = bbox.yMin - padding
                      !_ = traceStage
                        ("msdf: bitmap size " <> show width <> "x" <> show height
                          <> " pixels=" <> show area <> " bbox=" <> show bbox)
                        ()
                  in
                    let pixels = traceStage "msdf: render bitmap" (renderBitmap cfg width height offsetX offsetY coloredInfos segsAll segsByContour)
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
  { edgeRef :: EdgeRef
  , edge :: Edge
  , start :: Vec2
  , end :: Vec2
  , pseudoStart :: Vec2
  , pseudoEnd :: Vec2
  , overlaps :: Bool
  , edgeData :: EdgeDistanceData
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
          pseudoAtVertex i =
            let prevIdx = (i - 1 + n) `mod` n
                nPrev = endNormals !! prevIdx
                nCurr = startNormals !! i
                summed = vecAdd nPrev nCurr
                pn = normalizeVec summed
            in if nearZero pn then nCurr else pn
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
        in if s == 0 || s == desiredSign i then contour else reverseContour contour
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

contourIntersections
  :: Double
  -> [[LineSeg]]
  -> [Maybe BBox]
  -> Int -> Int -> Bool
contourIntersections eps segs bbs i j =
  let segsA = segs !! i
      segsB = segs !! j
  in case (bbs !! i, bbs !! j) of
      (Just bbA, Just bbB) ->
        if not (bboxOverlaps bbA bbB)
        then False
        else any (uncurry (segmentsIntersect eps)) [ (sa, sb) | sa <- segsA, sb <- segsB ]
      _ -> False

segmentsIntersect :: Double -> LineSeg -> LineSeg -> Bool
segmentsIntersect eps (p1, p2) (q1, q2) =
  let o1 = orient2D p1 p2 q1
      o2 = orient2D p1 p2 q2
      o3 = orient2D q1 q2 p1
      o4 = orient2D q1 q2 p2
      diff a b = (a > eps && b < -eps) || (a < -eps && b > eps)
  in (diff o1 o2 && diff o3 o4)
     || (abs o1 <= eps && onSegment eps p1 p2 q1)
     || (abs o2 <= eps && onSegment eps p1 p2 q2)
     || (abs o3 <= eps && onSegment eps q1 q2 p1)
     || (abs o4 <= eps && onSegment eps q1 q2 p2)

orient2D :: Vec2 -> Vec2 -> Vec2 -> Double
orient2D (x1, y1) (x2, y2) (x3, y3) =
  (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

onSegment :: Double -> Vec2 -> Vec2 -> Vec2 -> Bool
onSegment eps (x1, y1) (x2, y2) (x, y) =
  x >= min x1 x2 - eps && x <= max x1 x2 + eps &&
  y >= min y1 y2 - eps && y <= max y1 y2 + eps &&
  abs (orient2D (x1, y1) (x2, y2) (x, y)) <= eps

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

edgeKey :: EdgeInfo -> (Int, Int)
edgeKey info = (info.edgeRef.contourId, info.edgeRef.edgeId)

applyOverlapFlag :: Map.Map (Int, Int) Bool -> EdgeInfo -> EdgeInfo
applyOverlapFlag overlapMap info =
  let key = edgeKey info
  in info { overlaps = Map.findWithDefault False key overlapMap }

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

data FlatSeg = FlatSeg Int Int Double Double Vec2 Vec2

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
      loops' = mapMaybe (classifyLoop rule eps segsAll) loops
  in if null loops' then contours else loops'

data HalfEdge = HalfEdge Edge Int Int Vec2 Vec2

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

collectIntersectionsGrid :: [[Edge]] -> Double -> Double -> [FlatSeg] -> Map.Map (Int, Int) [Double]
collectIntersectionsGrid contours eps cellSize segs
  | null segs = Map.empty
  | otherwise =
      let n = length segs
          segArr = array (0, n - 1) (zip [0..] segs)
          minCell = max (eps * 4) 1e-3
          keyBase = n + 1
          build size =
            let buckets = foldl' (insertSegBuckets eps size) Map.empty (zip [0..] segs)
                sizes = map length (Map.elems buckets)
                maxBucket = if null sizes then 0 else maximum sizes
                totalPairs = sum [ s * (s - 1) `div` 2 | s <- sizes ]
            in (buckets, maxBucket, totalPairs)
          refine size iter =
            let (buckets, maxBucket, totalPairs) = build size
                tooMany = (msdfSplitMaxPairs > 0 && totalPairs > msdfSplitMaxPairs)
                       || (msdfSplitMaxBucket > 0 && maxBucket > msdfSplitMaxBucket)
            in if tooMany && iter < msdfSplitGridMaxIters && size > minCell
               then refine (max minCell (size * 0.5)) (iter + 1)
               else (size, buckets, maxBucket, totalPairs, tooMany)
          (size', buckets, maxBucket, totalPairs, tooMany) = refine cellSize 0
          step (acc, seen) idxs =
            let (acc', seen') = foldPairs keyBase acc seen idxs segArr
            in (acc', seen')
          tMap =
            if tooMany
            then Map.empty
            else fst (foldl' step (Map.empty, IntSet.empty) (Map.elems buckets))
          traceMsg =
            "msdf: split grid cell=" <> show size'
              <> " buckets=" <> show (Map.size buckets)
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
          addCell m c = Map.insertWith (++) c [idx] m
      in foldl' addCell acc [ (cx, cy) | cx <- [minCx .. maxCx], cy <- [minCy .. maxCy] ]

    cellCoord size v = floor (v / size) :: Int

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
      (buckets, traceMsg, tooMany) = overlapGridStats bboxArr eps
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

segmentsIntersectAny :: Double -> [LineSeg] -> [LineSeg] -> Bool
segmentsIntersectAny eps segsA segsB =
  any (\sa -> any (segmentsIntersect eps sa) segsB) segsA

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
  in if tooMany || n <= 1
     then ([], traceMsg)
     else
       let keyBase = n + 1
           step (pairs, seen) idxs =
             let (pairs', seen') = foldOverlapPairs keyBase eps infoArr bboxArr segsArr pairs seen idxs
             in (pairs', seen')
           (pairs, _) = foldl' step ([], IntSet.empty) (Map.elems buckets)
       in (pairs, traceMsg)

overlapGridStats :: Array Int BBox -> Double -> (Map.Map (Int, Int) [Int], String, Bool)
overlapGridStats bboxArr eps =
  let (lo, hi) = bounds bboxArr
      n = if lo > hi then 0 else hi - lo + 1
      minCell = max (eps * 4) 1e-3
      cellSize = if msdfOverlapGridSize > 0 then msdfOverlapGridSize else chooseOverlapCell bboxArr
      size = max minCell cellSize
      buckets = buildOverlapBuckets bboxArr size
      sizes = map length (Map.elems buckets)
      maxBucket = if null sizes then 0 else maximum sizes
      totalPairs = sum [ s * (s - 1) `div` 2 | s <- sizes ]
      tooMany = (msdfOverlapMaxPairs > 0 && totalPairs > msdfOverlapMaxPairs)
             || (msdfOverlapMaxBucket > 0 && maxBucket > msdfOverlapMaxBucket)
      traceMsg =
        "msdf: overlap grid cell=" <> show size
          <> " buckets=" <> show (Map.size buckets)
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

buildOverlapBuckets :: Array Int BBox -> Double -> Map.Map (Int, Int) [Int]
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
            addCell acc c = Map.insertWith (++) c [idx] acc
        in foldl' addCell m [ (cx, cy) | cx <- [minCx .. maxCx], cy <- [minCy .. maxCy] ]
  in if lo > hi then Map.empty else foldl' addBucket Map.empty [lo .. hi]

cellCoord :: Double -> Double -> Int
cellCoord size v = floor (v / size) :: Int

foldOverlapPairs
  :: Int
  -> Double
  -> Array Int EdgeInfo
  -> Array Int BBox
  -> Array Int [LineSeg]
  -> [(Int, Bool)]
  -> IntSet.IntSet
  -> [Int]
  -> ([(Int, Bool)], IntSet.IntSet)
foldOverlapPairs keyBase eps infoArr bboxArr segsArr pairs0 seen0 idxs =
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
                   && segmentsIntersectAny eps (segsArr ! a) (segsArr ! b)
                 pairs' =
                   if overlaps
                   then (a, True) : (b, True) : pairs
                   else pairs
             in (pairs', IntSet.insert key seen)
  in go idxs pairs0 seen0

renderBitmap :: MSDFConfig -> Int -> Int -> Double -> Double -> [(EdgeColor, EdgeInfo)] -> [LineSeg] -> [[LineSeg]] -> UArray Int Word8
renderBitmap cfg width height offsetX offsetY coloredInfos segs _segsByContour =
  let fmt = cfg.outputFormat
      n = width * height
      channels = bitmapChannels fmt
  in runST (do
       dRArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dGArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dBArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dAllArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       insideArr <- case cfg.distance.signMode of
         SignScanline -> buildScanlineInside cfg.distance.fillRule width height offsetX offsetY segs
         SignWinding -> pure Nothing
       let coloredInfosFiltered = coloredInfos
           (edgesR, edgesG, edgesB) = splitColoredEdges coloredInfosFiltered
           allEdges = edgesR ++ edgesG ++ edgesB
           bbFallback = BBox offsetX offsetY (offsetX + fromIntegral width) (offsetY + fromIntegral height)
           bb = case bboxFromEdgeInfos allEdges of
             Just b -> b
             Nothing -> bbFallback
           cellSz =
             let base = if cfg.distance.gridCellSize > 0
                        then cfg.distance.gridCellSize
                        else fromIntegral (max 2 cfg.range)
             in max 1 base
           flatness = cfg.outline.windingFlatness
           idxR = buildEdgeIndexInfo flatness cellSz bb edgesR
           idxG = buildEdgeIndexInfo flatness cellSz bb edgesG
           idxB = buildEdgeIndexInfo flatness cellSz bb edgesB
           idxAllInfo = buildEdgeIndexInfo flatness cellSz bb allEdges
           insideAtIdx ix iy =
             case insideArr of
               Nothing -> pure False
               Just arr ->
                 if ix < 0 || ix >= width || iy < 0 || iy >= height
                 then pure False
                 else readArray arr (iy * width + ix)
           insideAtPos (px, py) =
             let ix = floor (px - offsetX)
                 iy = floor (py - offsetY)
             in insideAtIdx ix iy
           boundaryOk info t edgeSegs =
             case insideArr of
               Nothing -> pure True
               Just _ ->
                 if not cfg.distance.overlapSupport
                 then pure True
                 else if cfg.outline.splitIntersections
                   then pure True
                 else
                   let normal = edgeUnitNormalAt info t
                       q = edgePointAt info.edge t
                       base = max 0.5 cfg.distance.overlapEpsilon
                       steps = [base, base * 2]
                       applyRule = applyFillRule cfg.distance.fillRule
                       check step = do
                         let q1 = vecAdd q (vecScale step normal)
                             q2 = vecSub q (vecScale step normal)
                         i1 <- insideAtPos q1
                         i2 <- insideAtPos q2
                         if i1 == i2
                           then pure False
                           else if not info.overlaps
                             then pure True
                             else do
                               let contourSegs = edgeSegs
                                   w1 = windingNumber segs q1 - windingNumber contourSegs q1
                                   w2 = windingNumber segs q2 - windingNumber contourSegs q2
                               pure (applyRule w1 /= applyRule w2)
                   in if nearZero normal
                      then pure True
                      else
                        foldM
                          (\acc step -> if acc then pure True else check step)
                          False
                          steps
           minDistanceInfoM usePseudo idx p =
             let (elo, ehi) = bounds idx.edges
             in if elo > ehi
                then pure 1e9
                else
                  let (cx, cy) = pointCell idx p
                      maxR = max idx.gridW idx.gridH
                      scanCellM idx' p' usePseudo' best (x, y) =
                        let cellIdx = cellIndex idx'.gridW x y
                            edgeIdxs = idx'.cells ! cellIdx
                            bestSq0 = best * best
                            go acc _ [] = pure acc
                            go acc accSq (i:is) =
                              let bb = idx'.edgeBBoxes ! i
                                  dSqBB = bboxDistanceSq bb p'
                              in if dSqBB >= accSq
                                 then go acc accSq is
                                 else do
                                   let info = idx'.edges ! i
                                       edgeSegs = idx'.edgeSegs ! i
                                       (dist, t) = edgeDistanceWithParam usePseudo' p' info
                                   if dist >= acc
                                     then go acc accSq is
                                     else do
                                       ok <- boundaryOk info t edgeSegs
                                       let acc' = if ok then dist else acc
                                           accSq' = if ok then dist * dist else accSq
                                       go acc' accSq' is
                        in go best bestSq0 edgeIdxs
                      scanRing minX maxX minY maxY r best0 = do
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
                      go r best =
                        let minX = max 0 (cx - r)
                            maxX = min (idx.gridW - 1) (cx + r)
                            minY = max 0 (cy - r)
                            maxY = min (idx.gridH - 1) (cy + r)
                        in do
                          best' <- scanRing minX maxX minY maxY r best
                          let minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) p
                              fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
                          if best' <= minBoundary
                            then pure best'
                            else if fullGrid || r >= maxR
                                 then pure best'
                                 else go (r + 1) best'
                  in do
                    best <- go 0 1e18
                    if best > 9e17
                      then pure (minDistanceInfo usePseudo idx p)
                      else pure best
           useOverlap =
             cfg.distance.overlapSupport
             && not cfg.outline.splitIntersections
             && case insideArr of
                  Just _ -> True
                  Nothing -> False
           distanceFor usePseudo idx p =
             if useOverlap
             then minDistanceInfoM usePseudo idx p
             else pure (minDistanceInfo usePseudo idx p)
       forM_ [0 .. height - 1] $ \y -> do
         forM_ [0 .. width - 1] $ \x -> do
           let i = y * width + x
               px' = offsetX + fromIntegral x + 0.5
               py' = offsetY + fromIntegral y + 0.5 + cfg.distance.signEpsilon
               p = (px', py')
           inside <- case insideArr of
              Just arr -> readArray arr i
              Nothing -> pure (windingNumber segs p /= 0)
           let sign = if inside then 1.0 else -1.0
           dR0 <- (* sign) <$> distanceFor cfg.distance.pseudoDistance idxR p
           dG0 <- (* sign) <$> distanceFor cfg.distance.pseudoDistance idxG p
           dB0 <- (* sign) <$> distanceFor cfg.distance.pseudoDistance idxB p
           dAll0 <- (* sign) <$> distanceFor False idxAllInfo p
           writeArray dRArr i dR0
           writeArray dGArr i dG0
           writeArray dBArr i dB0
           writeArray dAllArr i dAll0
       dAllArrCorr <- if cfg.correction.enableCorrection
         then do
           let edgeThresh = cfg.correction.edgeThreshold
               neighbors8 =
                 [ (-1, -1), (0, -1), (1, -1)
                 , (-1,  0),          (1,  0)
                 , (-1,  1), (0,  1), (1,  1)
                 ]
           dst <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
           forM_ [0 .. height - 1] $ \y -> do
             forM_ [0 .. width - 1] $ \x -> do
               let i = y * width + x
               d <- readArray dAllArr i
               let signD :: Int
                   signD = if d >= 0 then 1 else -1
               if abs d < edgeThresh
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
           pure dst
         else pure dAllArr
       badArr <- if cfg.correction.enableCorrection && width > 1 && height > 1
         then do
           bad <- newArray (0, n - 1) False :: ST s (STUArray s Int Bool)
           let edgeThresh = cfg.correction.edgeThreshold
               chanThresh = cfg.correction.channelThreshold
               hardThresh = cfg.correction.hardThreshold
           forM_ [0 .. height - 2] $ \y -> do
             forM_ [0 .. width - 2] $ \x -> do
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
                   sd = median3 dR dG dB
                   mismatch = sd * dAll < 0 || abs (sd - dAll) > chanThresh
                   hard = abs (dR - dAll) > hardThresh || abs (dG - dAll) > hardThresh || abs (dB - dAll) > hardThresh
               if abs dAll < edgeThresh && (mismatch || hard)
                 then do
                   writeArray bad i00 True
                   writeArray bad i10 True
                   writeArray bad i01 True
                   writeArray bad i11 True
                 else pure ()
           pure (Just bad)
         else pure Nothing
       out <- newArray (0, n * channels - 1) 0 :: ST s (STUArray s Int Word8)
       forM_ [0 .. height - 1] $ \y -> do
         forM_ [0 .. width - 1] $ \x -> do
           let i = y * width + x
               base = i * channels
           dR0 <- readArray dRArr i
           dG0 <- readArray dGArr i
           dB0 <- readArray dBArr i
           dAllCorr <- readArray dAllArrCorr i
           dAllRaw <- readArray dAllArr i
           let edgeThresh = cfg.correction.edgeThreshold
               chanThresh = cfg.correction.channelThreshold
               hardThresh = cfg.correction.hardThreshold
               nearEdge = abs dAllCorr < edgeThresh
               (dR1, dG1, dB1) =
                 if cfg.correction.enableCorrection && nearEdge
                 then ( clampChannel dR0 dAllCorr chanThresh
                      , clampChannel dG0 dAllCorr chanThresh
                      , clampChannel dB0 dAllCorr chanThresh
                      )
                 else (dR0, dG0, dB0)
               sd = median3 dR1 dG1 dB1
               mismatch = sd * dAllCorr < 0 || abs (sd - dAllCorr) > chanThresh
               hard = abs (dR1 - dAllCorr) > hardThresh || abs (dG1 - dAllCorr) > hardThresh || abs (dB1 - dAllCorr) > hardThresh
           clash <- if cfg.correction.enableCorrection && nearEdge
             then do
               let center = (dR0, dG0, dB0)
                   threshold = hardThresh
                   neighbors =
                     [ (-1, 0), (1, 0), (0, -1), (0, 1)
                     , (-1, -1), (1, -1), (-1, 1), (1, 1)
                     ]
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
                         pure (detectClash center (nr, ng, nb) threshold)
                 ) False neighbors
             else pure False
           bad <- case badArr of
             Just arr -> readArray arr i
             Nothing -> pure False
           let (dR, dG, dB) =
                 if cfg.correction.enableCorrection && nearEdge && (mismatch || hard || clash || bad)
                 then (dAllCorr, dAllCorr, dAllCorr)
                 else (dR1, dG1, dB1)
               r = distanceToByte cfg.range dR
               g = distanceToByte cfg.range dG
               b = distanceToByte cfg.range dB
           case fmt of
             BitmapMSDF -> do
               writeArray out base r
               writeArray out (base + 1) g
               writeArray out (base + 2) b
             BitmapMTSDF -> do
               writeArray out base r
               writeArray out (base + 1) g
               writeArray out (base + 2) b
               writeArray out (base + 3) (distanceToByte cfg.range dAllRaw)
       freeze out)

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

bboxDistanceSq :: BBox -> Vec2 -> Double
bboxDistanceSq bb (px, py) =
  let dx
        | px < bb.xMin = bb.xMin - px
        | px > bb.xMax = px - bb.xMax
        | otherwise = 0
      dy
        | py < bb.yMin = bb.yMin - py
        | py > bb.yMax = py - bb.yMax
        | otherwise = 0
  in dx * dx + dy * dy

data EdgeIndexInfo = EdgeIndexInfo
  { cellSize :: Double
  , originX :: Double
  , originY :: Double
  , gridW :: Int
  , gridH :: Int
  , cells :: Array Int [Int]
  , edges :: Array Int EdgeInfo
  , edgeBBoxes :: Array Int BBox
  , edgeSegs :: Array Int [LineSeg]
  }


buildEdgeIndexInfo :: Double -> Double -> BBox -> [EdgeInfo] -> EdgeIndexInfo
buildEdgeIndexInfo flatness cellSize' bb edgesList =
  let cellSize'' = max 1 cellSize'
      width = (max 1 (ceiling (bb.xMax - bb.xMin) + 1) :: Int)
      height = (max 1 (ceiling (bb.yMax - bb.yMin) + 1) :: Int)
      gridW' = max 1 (ceiling (fromIntegral width / cellSize''))
      gridH' = max 1 (ceiling (fromIntegral height / cellSize''))
      edgesArr =
        if null edgesList
        then array (0, -1) []
        else array (0, length edgesList - 1) (zip [0..] edgesList)
      edgeSegsArr =
        if null edgesList
        then array (0, -1) []
        else array (0, length edgesList - 1)
          [ (i, flattenEdges flatness [info.edge]) | (i, info) <- zip [0..] edgesList ]
      edgeBBoxArr =
        if null edgesList
        then array (0, -1) []
        else array (0, length edgesList - 1)
          [ (i, edgeBBox info.edge) | (i, info) <- zip [0..] edgesList ]
      pairs = concat
        [ [ (cellIndex gridW' x y, i) | x <- [x0 .. x1], y <- [y0 .. y1] ]
        | (i, e) <- zip [0..] edgesList
        , let bbE = edgeBBox e.edge
              x0 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMin)
              x1 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMax)
              y0 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMin)
              y1 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMax)
        ]
      cellsArr = accumArray (flip (:)) [] (0, gridW' * gridH' - 1) pairs
  in EdgeIndexInfo
       { cellSize = cellSize''
       , originX = bb.xMin
       , originY = bb.yMin
       , gridW = gridW'
       , gridH = gridH'
       , cells = cellsArr
       , edges = edgesArr
       , edgeBBoxes = edgeBBoxArr
       , edgeSegs = edgeSegsArr
       }

minDistanceInfo :: Bool -> EdgeIndexInfo -> Vec2 -> Double
minDistanceInfo usePseudo idx p =
  let (elo, ehi) = bounds idx.edges
  in if elo > ehi
     then 1e9
     else
       let (cx, cy) = pointCell idx p
           maxR = max idx.gridW idx.gridH
           scanRing minX maxX minY maxY r best0 =
             let goY y bestY
                   | y > maxY = bestY
                   | otherwise =
                       let goX x bestX
                             | x > maxX = bestX
                             | r == 0 || x == minX || x == maxX || y == minY || y == maxY =
                                 goX (x + 1) (scanCell idx p usePseudo bestX (x, y))
                             | otherwise = goX (x + 1) bestX
                           bestY' = goX minX bestY
                       in goY (y + 1) bestY'
             in goY minY best0
           go r best =
             let minX = max 0 (cx - r)
                 maxX = min (idx.gridW - 1) (cx + r)
                 minY = max 0 (cy - r)
                 maxY = min (idx.gridH - 1) (cy + r)
                 best' = scanRing minX maxX minY maxY r best
                 minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) p
                 fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
             in if best' <= minBoundary
                then best'
                else if fullGrid || r >= maxR
                     then best'
                     else go (r + 1) best'
       in go 0 1e18

scanCell :: EdgeIndexInfo -> Vec2 -> Bool -> Double -> (Int, Int) -> Double
scanCell idx p usePseudo best (x, y) =
  let cellIdx = cellIndex idx.gridW x y
      edgeIdxs = idx.cells ! cellIdx
      bestSq0 = best * best
      go acc _ [] = acc
      go acc accSq (i:is) =
        let bb = idx.edgeBBoxes ! i
            dSqBB = bboxDistanceSq bb p
        in if dSqBB >= accSq
           then go acc accSq is
           else
             let (dist, _) = edgeDistanceWithParam usePseudo p (idx.edges ! i)
                 acc' = if dist < acc then dist else acc
                 accSq' = if dist < acc then dist * dist else accSq
             in go acc' accSq' is
  in go best bestSq0 edgeIdxs

edgeDistanceWithParam :: Bool -> Vec2 -> EdgeInfo -> (Double, Double)
edgeDistanceWithParam usePseudo p info =
  let (dSq, t) = edgeDistanceSqWithParamFast p info
      d = if usePseudo then edgeDistancePseudoFrom p info dSq t else sqrt dSq
  in (d, t)

edgeDistanceSqWithParamFast :: Vec2 -> EdgeInfo -> (Double, Double)
edgeDistanceSqWithParamFast p info =
  case info.edgeData of
    EdgeLineData dx dy invLen2 ->
      let (x0, y0) = info.start
          (px, py) = p
          t = if invLen2 == 0
              then 0
              else ((px - x0) * dx + (py - y0) * dy) * invLen2
          t' = max 0 (min 1 t)
          cx = x0 + t' * dx
          cy = y0 + t' * dy
          ex = px - cx
          ey = py - cy
      in (ex * ex + ey * ey, t')
    EdgeQuadData ax ay bx by c3 c2 c1base ->
      case info.edge of
        EdgeQuad p0 p1 p2 ->
          let (x0, y0) = p0
              (px, py) = p
              cx = x0 - px
              cy = y0 - py
              c1 = c1base + 2 * (ax * cx + ay * cy)
              c0 = bx * cx + by * cy
              d0 = distanceSq2 x0 y0 px py
              (x2, y2) = p2
              d1 = distanceSq2 x2 y2 px py
              start = if d0 < d1 then (d0, 0) else (d1, 1)
              pick t (bestD, bestT) =
                if t >= 0 && t <= 1
                then
                  let d = distanceSqBezier2 p0 p1 p2 p t
                  in if d < bestD then (d, t) else (bestD, bestT)
                else (bestD, bestT)
          in foldCubicRoots c3 c2 c1 c0 start pick
        EdgeLine _ _ -> (1e18, 0)

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

distanceSq2 :: Double -> Double -> Double -> Double -> Double
distanceSq2 x0 y0 x1 y1 =
  let dx = x0 - x1
      dy = y0 - y1
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

edgeDistancePseudoFrom :: Vec2 -> EdgeInfo -> Double -> Double -> Double
edgeDistancePseudoFrom p info dSq t =
  let d = sqrt dSq
      eps = 1e-6
  in if t <= 0 + eps
     then pseudoAt info.start info.pseudoStart p d
     else if t >= 1 - eps
          then pseudoAt info.end info.pseudoEnd p d
          else d

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
  -> ST s (Maybe (STUArray s Int Bool))
buildScanlineInside rule width height offsetX offsetY segs = do
  let n = width * height
  arr <- newArray (0, n - 1) False :: ST s (STUArray s Int Bool)
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
              writeArray arr (y * width + xIdx) inside
              go (xIdx + 1) remaining w'
    go 0 hits 0
  pure (Just arr)

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
      base = max eps (len * 1e-3)
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

clampChannel :: Double -> Double -> Double -> Double
clampChannel d dAll threshold =
  if abs (d - dAll) > threshold then dAll else d

splitColoredEdges :: [(EdgeColor, EdgeInfo)] -> ([EdgeInfo], [EdgeInfo], [EdgeInfo])
splitColoredEdges colored =
  let edgesByMask mask = [ info | (col, info) <- colored, colorMask col .&. mask /= 0 ]
  in (edgesByMask 1, edgesByMask 2, edgesByMask 4)

colorMask :: EdgeColor -> Int
colorMask c =
  case c of
    ColorRed -> 1
    ColorGreen -> 2
    ColorBlue -> 4
    ColorYellow -> 3
    ColorMagenta -> 5
    ColorCyan -> 6
    ColorWhite -> 7

median3 :: Double -> Double -> Double -> Double
median3 a b c = max (min a b) (min (max a b) c)

detectClash :: (Double, Double, Double) -> (Double, Double, Double) -> Double -> Bool
detectClash (ar, ag, ab) (br, bg, bb) threshold =
  let am = median3 ar ag ab
      bm = median3 br bg bb
      devsA :: [(Int, Double)]
      devsA = sortOn (Down . snd)
        [ (0, abs (ar - am))
        , (1, abs (ag - am))
        , (2, abs (ab - am))
        ]
      devsB :: [(Int, Double)]
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

distanceToByte :: Int -> Double -> Word8
distanceToByte range dist =
  let r = fromIntegral (max 1 range) :: Double
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
