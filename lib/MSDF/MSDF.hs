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
import Data.List (sortOn)
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
  , edgeDistanceSqWithParam
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
      }
  , outline = OutlineConfig
      { windingFlatness = 0.02
      , contourEpsilon = 1e-6
      , normalizeOrientation = True
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
      contours = normalizeContours cfg.outline (glyphOutlineAt loc ttf glyphIndex)
  in renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours

renderGlyphMSDFWithContours :: Maybe VariationLocation -> MSDFConfig -> TTF -> Int -> [[Point]] -> GlyphMSDF
renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours =
  let base = glyphMetricsOnlyAt loc cfg ttf glyphIndex
      bbox = base.bbox
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      safeRange = max 1 cfg.range
  in if null contours
     then base
      else
        let contoursScaled = map (map (scalePoint scale)) contours
            eps = cfg.outline.contourEpsilon
            edgesByContour = map (filterDegenerateEdges eps . contourToEdges) contoursScaled
            edgeInfosByContour0 = buildEdgeInfos cfg.outline edgesByContour
            allInfos0 = concat edgeInfosByContour0
            overlapFlags = computeEdgeOverlaps cfg.outline.windingFlatness cfg.outline.contourEpsilon allInfos0
            overlapMap = Map.fromList
              [ (edgeKey info, flag)
              | (info, flag) <- zip allInfos0 overlapFlags
              ]
            edgeInfosByContour = map (map (applyOverlapFlag overlapMap)) edgeInfosByContour0
            coloredEdges = colorEdges cfg.coloring edgesByContour
            coloredInfos0 = attachEdgeInfo edgeInfosByContour coloredEdges
            allEdges0 = [ info | (_, info) <- coloredInfos0 ]
            segsAll = flattenEdges cfg.outline.windingFlatness [ info.edge | info <- allEdges0 ]
            coloredInfos =
              if cfg.distance.overlapSupport
              then
                let filtered =
                      filterBoundaryEdges
                        cfg.distance.fillRule
                        cfg.distance.overlapEpsilon
                        cfg.outline.windingFlatness
                        segsAll
                        coloredInfos0
                in if null filtered then coloredInfos0 else filtered
              else coloredInfos0
        in if null coloredInfos
           then base
           else
             let padding = fromIntegral (safeRange + 1) :: Double
                 width = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding))
                 height = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding))
                 offsetX = bbox.xMin - padding
                 offsetY = bbox.yMin - padding
                 pixels = renderBitmap cfg width height offsetX offsetY coloredInfos segsAll
             in GlyphMSDF
                 { index = glyphIndex
                 , codepoints = []
                 , advance = base.advance
                 , bearingX = base.bearingX
                 , bearingY = base.bearingY
                 , bbox = bbox
                 , bitmap = MSDFBitmap
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
     then renderGlyphMSDFWithContours loc cfg ttf glyphIndex (cache.contours ! glyphIndex)
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
          pure (renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours)
        Nothing -> do
          let contours = normalizeContours cfg.outline (glyphOutlineAt loc ttf glyphIndex)
          modifyIORef' cache.contoursRef (IntMap.insert glyphIndex contours)
          pure (renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours)

glyphMetricsOnly :: MSDFConfig -> TTF -> Int -> GlyphMSDF
glyphMetricsOnly cfg ttf glyphIndex =
  let loc = variationLocation cfg ttf
  in glyphMetricsOnlyAt loc cfg ttf glyphIndex

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
  }

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
      [ EdgeInfo
          { edgeRef = EdgeRef contourId i e
          , edge = e
          , start = edgeStartPoint e
          , end = edgeEndPoint e
          , pseudoStart = pseudoStarts !! i
          , pseudoEnd = pseudoEnds !! i
          , overlaps = False
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

computeEdgeOverlaps :: Double -> Double -> [EdgeInfo] -> [Bool]
computeEdgeOverlaps flatness eps infos =
  let n = length infos
      infoArr =
        if null infos
        then array (0, -1) []
        else array (0, n - 1) (zip [0..] infos)
      segsArr =
        if null infos
        then array (0, -1) []
        else array (0, n - 1)
          [ (i, flattenEdges flatness [info.edge]) | (i, info) <- zip [0..] infos ]
      bboxArr =
        if null infos
        then array (0, -1) []
        else array (0, n - 1)
          [ (i, edgeBBox info.edge) | (i, info) <- zip [0..] infos ]
      overlapPairs =
        [ (i, True)
        | i <- [0 .. n - 1]
        , j <- [i + 1 .. n - 1]
        , let infoA = infoArr ! i
              infoB = infoArr ! j
        , not (edgesShareEndpoint eps infoA infoB)
        , bboxOverlaps (bboxArr ! i) (bboxArr ! j)
        , segmentsIntersectAny eps (segsArr ! i) (segsArr ! j)
        ] ++
        [ (j, True)
        | i <- [0 .. n - 1]
        , j <- [i + 1 .. n - 1]
        , let infoA = infoArr ! i
              infoB = infoArr ! j
        , not (edgesShareEndpoint eps infoA infoB)
        , bboxOverlaps (bboxArr ! i) (bboxArr ! j)
        , segmentsIntersectAny eps (segsArr ! i) (segsArr ! j)
        ]
      overlapsArr = accumArray (||) False (0, n - 1) overlapPairs
  in if n == 0 then [] else [ overlapsArr ! i | i <- [0 .. n - 1] ]

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

renderBitmap :: MSDFConfig -> Int -> Int -> Double -> Double -> [(EdgeColor, EdgeInfo)] -> [LineSeg] -> UArray Int Word8
renderBitmap cfg width height offsetX offsetY coloredInfos segs =
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
                               let w1 = windingNumber segs q1 - windingNumber edgeSegs q1
                                   w2 = windingNumber segs q2 - windingNumber edgeSegs q2
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
                      go r best =
                        let minX = max 0 (cx - r)
                            maxX = min (idx.gridW - 1) (cx + r)
                            minY = max 0 (cy - r)
                            maxY = min (idx.gridH - 1) (cy + r)
                            cells =
                              [ (x, y)
                              | y <- [minY .. maxY]
                              , x <- [minX .. maxX]
                              , r == 0 || x == minX || x == maxX || y == minY || y == maxY
                              ]
                        in do
                          best' <- foldM (scanCellM idx p usePseudo) best cells
                          let minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) p
                              fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
                          if best' <= minBoundary
                            then pure best'
                            else if fullGrid || r >= maxR
                                 then pure best'
                                 else go (r + 1) best'
                      scanCellM idx' p' usePseudo' best (x, y) =
                        let cellIdx = cellIndex idx'.gridW x y
                            edgeIdxs = idx'.cells ! cellIdx
                        in foldM
                             (\acc i -> do
                                 let info = idx'.edges ! i
                                     edgeSegs = idx'.edgeSegs ! i
                                     (dist, t) = edgeDistanceWithParam usePseudo' p' info
                                 ok <- boundaryOk info t edgeSegs
                                 pure (min acc (if ok then dist else 1e18))
                             ) best edgeIdxs
                  in do
                    best <- go 0 1e18
                    if best > 9e17
                      then pure (minDistanceInfo usePseudo idx p)
                      else pure best
           useOverlap = cfg.distance.overlapSupport && case insideArr of
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

data EdgeIndexInfo = EdgeIndexInfo
  { cellSize :: Double
  , originX :: Double
  , originY :: Double
  , gridW :: Int
  , gridH :: Int
  , cells :: Array Int [Int]
  , edges :: Array Int EdgeInfo
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
           go r best =
             let minX = max 0 (cx - r)
                 maxX = min (idx.gridW - 1) (cx + r)
                 minY = max 0 (cy - r)
                 maxY = min (idx.gridH - 1) (cy + r)
                 best' = foldl (scanCell idx p usePseudo) best
                              [ (x, y)
                              | y <- [minY .. maxY]
                              , x <- [minX .. maxX]
                              , r == 0 || x == minX || x == maxX || y == minY || y == maxY
                              ]
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
      distFor info =
        let (dist, _) = edgeDistanceWithParam usePseudo p info
        in dist
  in foldl (\acc i -> min acc (distFor (idx.edges ! i))) best edgeIdxs

edgeDistanceWithParam :: Bool -> Vec2 -> EdgeInfo -> (Double, Double)
edgeDistanceWithParam usePseudo p info =
  let (dSq, t) = edgeDistanceSqWithParam p info.edge
      d = if usePseudo then edgeDistancePseudoFrom p info dSq t else sqrt dSq
  in (d, t)

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

edgeIsBoundary :: FillRule -> Double -> Double -> [LineSeg] -> EdgeInfo -> Bool
edgeIsBoundary rule eps flatness segsAll info =
  let edge = info.edge
      len = edgeLengthApprox edge
      base = max eps (len * 1e-3)
      steps = [base, base * 2]
      samples = case edge of
        EdgeLine _ _ -> [0.5]
        EdgeQuad _ _ _ -> [0.25, 0.5, 0.75]
      applyRule = applyFillRule rule
      segsEdge = flattenEdges flatness [edge]
      windingOther p = windingNumber segsAll p - windingNumber segsEdge p
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
                     w1 = windingOther p1
                     w2 = windingOther p2
                 in applyRule w1 /= applyRule w2
               )
               steps
  in if not info.overlaps
     then True
     else any check samples

filterBoundaryEdges
  :: FillRule
  -> Double
  -> Double
  -> [LineSeg]
  -> [(EdgeColor, EdgeInfo)]
  -> [(EdgeColor, EdgeInfo)]
filterBoundaryEdges rule eps flatness segsAll colored =
  let keep info = edgeIsBoundary rule eps flatness segsAll info
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
