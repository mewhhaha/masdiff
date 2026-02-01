module MSDF.MSDF
  ( MSDFConfig(..)
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
import Data.Array (Array, array, accumArray, bounds, (!) )
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray, listArray)
import Data.List (sort)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8)
import MSDF.Outline
import MSDF.TTF.Parser
import MSDF.TTF.Variations (applyGvarToContours, componentDeltas, hvarDeltas, vvarDeltas)
import MSDF.Types

-- | Configuration for MSDF generation.
data MSDFConfig = MSDFConfig
  { pixelSize :: Int
  , range :: Int
  , cornerThreshold :: Double
  , glyphSet :: GlyphSet
  , parallelism :: Int
  , variations :: [(String, Double)]
  , packAtlas :: Bool
  , atlasPadding :: Int
  , atlasMinSize :: Int
  , atlasMaxSize :: Int
  , atlasPowerOfTwo :: Bool
  , msdfCorrectionThreshold :: Double
  , outputFormat :: BitmapFormat
  , windingFlatness :: Double
  , speckleThreshold :: Double
  , edgeConflictThreshold :: Double
  }

-- | Glyph subset selection.
data GlyphSet
  = GlyphSetNone
  | GlyphSetAll
  | GlyphSetCodepoints [Int]

data GlyphCache = GlyphCache
  { location :: Maybe VariationLocation
  , contours :: Array Int [[Point]]
  } deriving (Eq, Show)

data GlyphCacheLazy = GlyphCacheLazy
  { location :: Maybe VariationLocation
  , contoursRef :: IORef (IntMap.IntMap [[Point]])
  }

instance Semigroup GlyphSet where
  GlyphSetAll <> _ = GlyphSetAll
  _ <> GlyphSetAll = GlyphSetAll
  GlyphSetNone <> x = x
  x <> GlyphSetNone = x
  GlyphSetCodepoints a <> GlyphSetCodepoints b = GlyphSetCodepoints (uniqueSorted (a ++ b))

instance Monoid GlyphSet where
  mempty = GlyphSetNone

-- | Default configuration.
defaultMSDFConfig :: MSDFConfig
defaultMSDFConfig = MSDFConfig
  { pixelSize = 32
  , range = 4
  , cornerThreshold = 3.0
  , glyphSet = GlyphSetAll
  , parallelism = 0
  , variations = []
  , packAtlas = True
  , atlasPadding = 1
  , atlasMinSize = 256
  , atlasMaxSize = 4096
  , atlasPowerOfTwo = True
  , msdfCorrectionThreshold = 0.05
  , outputFormat = BitmapMSDF
  , windingFlatness = 0.02
  , speckleThreshold = 1.0
  , edgeConflictThreshold = 1.0
  }

variationLocation :: MSDFConfig -> TTF -> Maybe VariationLocation
variationLocation cfg ttf =
  case ttf.variations of
    Nothing -> Nothing
    Just vars -> Just (normalizeLocation vars.fvar vars.avar cfg.variations)

renderGlyphMSDF :: MSDFConfig -> TTF -> Int -> GlyphMSDF
renderGlyphMSDF cfg ttf glyphIndex =
  let loc = variationLocation cfg ttf
      contours = normalizeContours (glyphOutlineAt loc ttf glyphIndex)
  in renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours

renderGlyphMSDFWithContours :: Maybe VariationLocation -> MSDFConfig -> TTF -> Int -> [[Point]] -> GlyphMSDF
renderGlyphMSDFWithContours loc cfg ttf glyphIndex contours =
  let base = glyphMetricsOnlyAt loc cfg ttf glyphIndex
      bbox = base.bbox
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      safeRange = max 1 cfg.range
      correction = max 0 cfg.msdfCorrectionThreshold
  in if null contours
     then base
     else
       let coloredEdges = concatMap (colorContourEdges cfg.cornerThreshold cfg.edgeConflictThreshold) (splitContoursEdges contours scale)
           allEdges = map snd coloredEdges
           segsAll = concatMap (flattenEdge cfg.windingFlatness) allEdges
           boundaryEdges = filterBoundaryEdges 0.25 segsAll allEdges
           coloredEdges' = filter (\(_, e) -> e `elem` boundaryEdges) coloredEdges
           colorEdges = splitColoredEdges coloredEdges'
           segsBoundary = concatMap (flattenEdge cfg.windingFlatness) boundaryEdges
           segs = if null segsBoundary then segsAll else segsBoundary
           padding = fromIntegral (safeRange + 1) :: Double
           width = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding))
           height = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding))
           offsetX = bbox.xMin - padding
           offsetY = bbox.yMin - padding
           pixels = renderBitmap cfg.outputFormat cfg.speckleThreshold width height offsetX offsetY safeRange correction cfg.edgeConflictThreshold colorEdges segs
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
        [ (i, normalizeContours (glyphOutlineAt loc ttf i)) | i <- [0 .. numGlyphs - 1] ]
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
          let contours = normalizeContours (glyphOutlineAt loc ttf glyphIndex)
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

normalizeContours :: [[Point]] -> [[Point]]
normalizeContours contours =
  map normalizeContourRaw (filter (not . null) contours)

normalizeContourRaw :: [Point] -> [Point]
normalizeContourRaw pts =
  let stripped = dropDuplicatePoints pts
  in case stripped of
       [] -> []
       (first:rest) ->
         let last' = lastPoint first rest
             xs = first : rest
             xs' = if samePoint first last' then init xs else xs
         in xs' ++ [first]

dropDuplicatePoints :: [Point] -> [Point]
dropDuplicatePoints [] = []
dropDuplicatePoints (p:ps) = p : go p ps
  where
    go _ [] = []
    go prev (x:xs)
      | samePoint prev x = go prev xs
      | otherwise = x : go x xs

samePoint :: Point -> Point -> Bool
samePoint a b = a.x == b.x && a.y == b.y && a.on == b.on

lastPoint :: Point -> [Point] -> Point
lastPoint prev [] = prev
lastPoint _ (x:xs) = lastPoint x xs

safeIndex :: Array Int Int -> Int -> Int
safeIndex arr i =
  let (lo, hi) = bounds arr
  in if i < lo || i > hi then 0 else arr ! i

inBounds :: Array Int Int -> Int -> Bool
inBounds arr i =
  let (lo, hi) = bounds arr
  in i >= lo && i <= hi

scaleEdge :: Double -> Edge -> Edge
scaleEdge s (EdgeLine (x0, y0) (x1, y1)) = EdgeLine (x0 * s, y0 * s) (x1 * s, y1 * s)
scaleEdge s (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) = EdgeQuad (x0 * s, y0 * s) (x1 * s, y1 * s) (x2 * s, y2 * s)

-- | Build colored edges per contour.
splitContoursEdges :: [[Point]] -> Double -> [[Edge]]
splitContoursEdges contours scale =
  let toEdgeList pts = map (scaleEdge scale) (contourToEdges pts)
  in map toEdgeList contours

colorContourEdges :: Double -> Double -> [Edge] -> [(Int, Edge)]
colorContourEdges _ _ [] = []
colorContourEdges cornerThreshold conflictThreshold edges =
  let n = length edges
      joinCorners =
        [ isCorner (edges !! i) (edges !! ((i + 1) `mod` n)) cornerThreshold
        | i <- [0 .. n - 1]
        ]
      cornerStarts = [ (i + 1) `mod` n | i <- [0 .. n - 1], joinCorners !! i ]
      starts = ensureMinSegments edges cornerStarts
      segments = buildSegments edges starts
      coloredSegments = zipWith (\c seg -> map (\e -> (c, e)) seg) (cycle [0,1,2]) segments
  in resolveEdgeColorConflicts conflictThreshold (concat coloredSegments)

isCorner :: Edge -> Edge -> Double -> Bool
isCorner e1 e2 thresholdDeg =
  let (x1, y1) = edgeEndDir e1
      (x2, y2) = edgeStartDir e2
      dotp = x1 * x2 + y1 * y2
      len1 = sqrt (x1 * x1 + y1 * y1)
      len2 = sqrt (x2 * x2 + y2 * y2)
      threshold = thresholdDeg * pi / 180
  in if len1 == 0 || len2 == 0
     then False
     else
       let cosang = max (-1) (min 1 (dotp / (len1 * len2)))
           ang = acos cosang
       in ang > threshold

ensureMinSegments :: [Edge] -> [Int] -> [Int]
ensureMinSegments edges starts =
  let n = length edges
      base = uniqueSorted starts
      minSegs = 3
      anchor = case base of
        (a:_) -> a
        [] -> 0
      merged =
        if length base >= minSegs
        then base
        else uniqueSorted (base ++ splitByLengthFrom edges anchor minSegs)
  in if n == 0
     then []
     else if length merged >= minSegs
          then merged
          else uniqueSorted (base ++ splitByIndexFrom anchor n minSegs)

splitByLengthFrom :: [Edge] -> Int -> Int -> [Int]
splitByLengthFrom edges anchor segments =
  let n = length edges
  in if n == 0 || segments <= 0
     then []
     else
       let idxs = indicesFrom anchor n
           lens = map (\i -> edgeLengthApprox (edges !! i)) idxs
           total = sum lens
       in if total <= 1e-6
          then splitByIndexFrom anchor n segments
          else
            let step = total / fromIntegral segments
                thresholds = [ step * fromIntegral k | k <- [1 .. segments - 1] ]
                (hits, _cum, _ts) = foldl' accumThresholds ([], 0.0, thresholds) (zip idxs lens)
            in uniqueSorted (anchor : hits)

accumThresholds :: ([Int], Double, [Double]) -> (Int, Double) -> ([Int], Double, [Double])
accumThresholds (acc, cum, ts) (idx, len) =
  let cum' = cum + len
      (hit, rest) = span (<= cum') ts
      acc' = acc ++ replicate (length hit) idx
  in (acc', cum', rest)

splitByIndexFrom :: Int -> Int -> Int -> [Int]
splitByIndexFrom anchor n segments =
  let step = max 1 (n `div` segments)
  in uniqueSorted
       [ (anchor + i * step) `mod` n
       | i <- [0 .. segments - 1]
       ]

indicesFrom :: Int -> Int -> [Int]
indicesFrom anchor n =
  take n (iterate (\i -> (i + 1) `mod` n) anchor)

edgeLengthApprox :: Edge -> Double
edgeLengthApprox (EdgeLine (x0, y0) (x1, y1)) =
  let dx = x1 - x0
      dy = y1 - y0
  in sqrt (dx * dx + dy * dy)
edgeLengthApprox (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
  let dx01 = x1 - x0
      dy01 = y1 - y0
      dx12 = x2 - x1
      dy12 = y2 - y1
      d01 = sqrt (dx01 * dx01 + dy01 * dy01)
      d12 = sqrt (dx12 * dx12 + dy12 * dy12)
  in d01 + d12

uniqueSorted :: [Int] -> [Int]
uniqueSorted xs =
  case sort xs of
    [] -> []
    (y:ys) -> y : go y ys
  where
    go _ [] = []
    go prev (z:zs)
      | z == prev = go prev zs
      | otherwise = z : go z zs

buildSegments :: [Edge] -> [Int] -> [[Edge]]
buildSegments edges starts =
  let n = length edges
      starts' = starts
      nexts = rotate starts'
  in zipWith (segmentFrom edges n) starts' nexts

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

segmentFrom :: [Edge] -> Int -> Int -> Int -> [Edge]
segmentFrom edges n start next =
  let count = if next >= start then next - start else n - start + next
  in [ edges !! ((start + i) `mod` n) | i <- [0 .. count - 1] ]

splitColoredEdges :: [(Int, Edge)] -> ([Edge], [Edge], [Edge])
splitColoredEdges colored =
  let edgesByColor c = [ e | (col,e) <- colored, col == c ]
  in (edgesByColor 0, edgesByColor 1, edgesByColor 2)

resolveEdgeColorConflicts :: Double -> [(Int, Edge)] -> [(Int, Edge)]
resolveEdgeColorConflicts conflictThreshold colored =
  let base = resolveAdjacentConflicts colored
  in if conflictThreshold <= 0 || length base <= 1
     then base
     else recolorConflicts conflictThreshold base

resolveAdjacentConflicts :: [(Int, Edge)] -> [(Int, Edge)]
resolveAdjacentConflicts colored =
  let n = length colored
  in if n <= 1
     then colored
     else
       let colors = map fst colored
           pick i =
             let (c, e) = colored !! i
                 prev = colors !! ((i - 1 + n) `mod` n)
                 next = colors !! ((i + 1) `mod` n)
             in if c == prev
                then (chooseColor c prev next, e)
                else (c, e)
       in [ pick i | i <- [0 .. n - 1] ]

chooseColor :: Int -> Int -> Int -> Int
chooseColor cur prev next =
  case filter (\c -> c /= prev && c /= next) [0,1,2] of
    (c:_) -> c
    [] -> (cur + 1) `mod` 3

recolorConflicts :: Double -> [(Int, Edge)] -> [(Int, Edge)]
recolorConflicts conflictThreshold colored =
  let passes = 5 :: Int
      go i edges =
        if i >= passes
        then edges
        else
          let edges' = recolorOnce edges
          in if edges' == edges then edges else go (i + 1) edges'
  in go 0 colored
  where
    recolorOnce edges =
      let (edges0, edges1, edges2) = splitColoredEdges edges
          allEdges = edges0 ++ edges1 ++ edges2
          (originX, originY, width, height) = gridForEdges allEdges
          cellSize = max 1 conflictThreshold
          idx0 = buildEdgeIndex cellSize originX originY width height edges0
          idx1 = buildEdgeIndex cellSize originX originY width height edges1
          idx2 = buildEdgeIndex cellSize originX originY width height edges2
          distTo idx e = minimum (map (minDistanceIndexed idx) (edgeSamplePoints e))
          d0 e = distTo idx0 e
          d1 e = distTo idx1 e
          d2 e = distTo idx2 e
          chooseConflictColor c e =
            case c of
              0 ->
                let dA = d1 e
                    dB = d2 e
                in if dA <= dB then 1 else 2
              1 ->
                let dA = d0 e
                    dB = d2 e
                in if dA <= dB then 0 else 2
              _ ->
                let dA = d0 e
                    dB = d1 e
                in if dA <= dB then 0 else 1
      in
      [ let conflict =
              case c of
                0 -> d1 e < conflictThreshold && d2 e < conflictThreshold
                1 -> d0 e < conflictThreshold && d2 e < conflictThreshold
                _ -> d0 e < conflictThreshold && d1 e < conflictThreshold
        in if conflict
           then (chooseConflictColor c e, e)
           else (c, e)
      | (c, e) <- edges
      ]

gridForEdges :: [Edge] -> (Double, Double, Int, Int)
gridForEdges edges =
  case edges of
    [] -> (0, 0, 1, 1)
    (e:es) ->
      let bb0 = edgeBBox e
          bb = foldl' (\acc ed -> bboxUnion acc (edgeBBox ed)) bb0 es
          w = max 1 (ceiling (bb.xMax - bb.xMin) + 1)
          h = max 1 (ceiling (bb.yMax - bb.yMin) + 1)
      in (bb.xMin, bb.yMin, w, h)

edgeSamplePoints :: Edge -> [Vec2]
edgeSamplePoints e =
  case e of
    EdgeLine _ _ -> map (edgePointAt e) [0.25, 0.5, 0.75]
    EdgeQuad _ _ _ -> map (edgePointAt e) [0.2, 0.5, 0.8]

edgePointAt :: Edge -> Double -> Vec2
edgePointAt (EdgeLine (x0, y0) (x1, y1)) t =
  (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)
edgePointAt (EdgeQuad p0 p1 p2) t =
  bezier p0 p1 p2 t

data EdgeIndex = EdgeIndex
  { cellSize :: Double
  , originX :: Double
  , originY :: Double
  , gridW :: Int
  , gridH :: Int
  , cells :: Array Int [Int]
  , edges :: Array Int Edge
  }

buildEdgeIndex :: Double -> Double -> Double -> Int -> Int -> [Edge] -> EdgeIndex
buildEdgeIndex cellSize' originX originY width height edgesList =
  let cellSize'' = max 1 cellSize'
      gridW' = max 1 (ceiling (fromIntegral width / cellSize''))
      gridH' = max 1 (ceiling (fromIntegral height / cellSize''))
      edgesArr =
        if null edgesList
        then array (0, -1) []
        else array (0, length edgesList - 1) (zip [0..] edgesList)
      pairs = concat
        [ [ (cellIndex gridW' x y, i) | x <- [x0 .. x1], y <- [y0 .. y1] ]
        | (i, e) <- zip [0..] edgesList
        , let bb = edgeBBox e
              x0 = clamp 0 (gridW' - 1) (toCell originX cellSize'' bb.xMin)
              x1 = clamp 0 (gridW' - 1) (toCell originX cellSize'' bb.xMax)
              y0 = clamp 0 (gridH' - 1) (toCell originY cellSize'' bb.yMin)
              y1 = clamp 0 (gridH' - 1) (toCell originY cellSize'' bb.yMax)
        ]
      cellsArr = accumArray (flip (:)) [] (0, gridW' * gridH' - 1) pairs
  in EdgeIndex
       { cellSize = cellSize''
       , originX = originX
       , originY = originY
       , gridW = gridW'
       , gridH = gridH'
       , cells = cellsArr
       , edges = edgesArr
       }

-- | Line segment representation for winding tests.
type LineSeg = (Vec2, Vec2)

windingNumber :: [LineSeg] -> Vec2 -> Int
windingNumber segs (px', py') =
  foldl' step 0 segs
  where
    step wn ((x0, y0), (x1, y1))
      | y0 <= py' =
          if y1 > py' && isLeft (x0, y0) (x1, y1) (px', py') > 0
          then wn + 1
          else wn
      | y1 <= py' =
          if isLeft (x0, y0) (x1, y1) (px', py') < 0
          then wn - 1
          else wn
      | otherwise = wn

isLeft :: Vec2 -> Vec2 -> Vec2 -> Double
isLeft (x0,y0) (x1,y1) (px',py') =
  (x1 - x0) * (py' - y0) - (px' - x0) * (y1 - y0)

filterBoundaryEdges :: Double -> [LineSeg] -> [Edge] -> [Edge]
filterBoundaryEdges eps segs = filter (edgeIsBoundary eps segs)

edgeIsBoundary :: Double -> [LineSeg] -> Edge -> Bool
edgeIsBoundary eps segs edge =
  let ts = case edge of
        EdgeLine _ _ -> [0.5]
        EdgeQuad _ _ _ -> [0.2, 0.5, 0.8]
      samples = [ sample t | t <- ts ]
      valid = [ v | Just v <- samples ]
  in case valid of
       [] -> True
       _ -> or valid
  where
    sample t =
      case unitPerp (edgeTangentAt edge t) of
        Nothing -> Nothing
        Just n ->
          let p = edgePointAt edge t
              pOut = vecAdd p (vecScale eps n)
              pIn = vecSub p (vecScale eps n)
              insideOut = windingNumber segs pOut /= 0
              insideIn = windingNumber segs pIn /= 0
          in Just (insideOut /= insideIn)

edgeTangentAt :: Edge -> Double -> Vec2
edgeTangentAt (EdgeLine a b) _ = vecSub b a
edgeTangentAt (EdgeQuad p0 p1 p2) t =
  let mt = 1 - t
      v0 = vecScale mt (vecSub p1 p0)
      v1 = vecScale t (vecSub p2 p1)
  in vecScale 2 (vecAdd v0 v1)

unitPerp :: Vec2 -> Maybe Vec2
unitPerp (x0, y0) =
  let nx = -y0
      ny = x0
      len = sqrt (nx * nx + ny * ny)
  in if len < 1e-12
     then Nothing
     else Just (nx / len, ny / len)

edgeBBox :: Edge -> BBox
edgeBBox (EdgeLine (x0, y0) (x1, y1)) =
  BBox
    { xMin = min x0 x1
    , yMin = min y0 y1
    , xMax = max x0 x1
    , yMax = max y0 y1
    }
edgeBBox (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
  let xs = [x0, x1, x2] ++ quadExtrema1D x0 x1 x2
      ys = [y0, y1, y2] ++ quadExtrema1D y0 y1 y2
  in BBox
      { xMin = minimum xs
      , yMin = minimum ys
      , xMax = maximum xs
      , yMax = maximum ys
      }

quadExtrema1D :: Double -> Double -> Double -> [Double]
quadExtrema1D a0 a1 a2 =
  let denom = a0 - 2 * a1 + a2
  in if abs denom < 1e-12
     then []
     else
       let t = (a0 - a1) / denom
       in if t > 0 && t < 1
          then [quadAt1D a0 a1 a2 t]
          else []

quadAt1D :: Double -> Double -> Double -> Double -> Double
quadAt1D a0 a1 a2 t =
  let mt = 1 - t
  in mt * mt * a0 + 2 * mt * t * a1 + t * t * a2

toCell :: Double -> Double -> Double -> Int
toCell origin size v =
  floor ((v - origin) / size)

cellIndex :: Int -> Int -> Int -> Int
cellIndex gridW' x y = y * gridW' + x

clamp :: Int -> Int -> Int -> Int
clamp lo hi v = max lo (min hi v)

minDistanceIndexed :: EdgeIndex -> Vec2 -> Double
minDistanceIndexed idx p@(px', py') =
  let (elo, ehi) = bounds idx.edges
  in if elo > ehi
     then 1e9
     else
       let (cx, cy) = pointCell idx p
           maxR = max idx.gridW idx.gridH
           go r bestSq =
             let minX = max 0 (cx - r)
                 maxX = min (idx.gridW - 1) (cx + r)
                 minY = max 0 (cy - r)
                 maxY = min (idx.gridH - 1) (cy + r)
                 bestSq' = foldl' (scanCell idx p) bestSq
                              [ (x, y)
                              | y <- [minY .. maxY]
                              , x <- [minX .. maxX]
                              , r == 0 || x == minX || x == maxX || y == minY || y == maxY
                              ]
                 minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py')
                 fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
             in if bestSq' < 1e18 && bestSq' <= minBoundary * minBoundary
                then sqrt bestSq'
                else if fullGrid || r >= maxR
                     then sqrt bestSq'
                     else go (r + 1) bestSq'
       in go 0 1e18

scanCell :: EdgeIndex -> Vec2 -> Double -> (Int, Int) -> Double
scanCell idx p bestSq (x, y) =
  let cellIdx = cellIndex idx.gridW x y
      edgeIdxs = idx.cells ! cellIdx
  in foldl' (\acc i -> min acc (edgeDistanceSq p (idx.edges ! i))) bestSq edgeIdxs

distanceToBoundary :: EdgeIndex -> (Int, Int) -> (Int, Int) -> Vec2 -> Double
distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py') =
  let x0 = idx.originX + fromIntegral minX * idx.cellSize
      x1 = idx.originX + fromIntegral (maxX + 1) * idx.cellSize
      y0 = idx.originY + fromIntegral minY * idx.cellSize
      y1 = idx.originY + fromIntegral (maxY + 1) * idx.cellSize
      dx = min (px' - x0) (x1 - px')
      dy = min (py' - y0) (y1 - py')
  in max 0 (min dx dy)

pointCell :: EdgeIndex -> Vec2 -> (Int, Int)
pointCell idx (px', py') =
  let cx = clamp 0 (idx.gridW - 1) (toCell idx.originX idx.cellSize px')
      cy = clamp 0 (idx.gridH - 1) (toCell idx.originY idx.cellSize py')
  in (cx, cy)

renderBitmap :: BitmapFormat -> Double -> Int -> Int -> Double -> Double -> Int -> Double -> Double -> ([Edge], [Edge], [Edge]) -> [LineSeg] -> UArray Int Word8
renderBitmap fmt speckleThreshold width height offsetX offsetY range correction conflictThreshold (edgesR, edgesG, edgesB) segs =
  let edgesAll = edgesR ++ edgesG ++ edgesB
      cellSz = max 4 (fromIntegral range)
      idxR = buildEdgeIndex cellSz offsetX offsetY width height edgesR
      idxG = buildEdgeIndex cellSz offsetX offsetY width height edgesG
      idxB = buildEdgeIndex cellSz offsetX offsetY width height edgesB
      idxAll = buildEdgeIndex cellSz offsetX offsetY width height edgesAll
      n = width * height
      channels = bitmapChannels fmt
  in runST (do
       dRArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dGArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dBArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       dAllArr <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
       (dAllArr0, insideLow) <-
         if fmt == BitmapMTSDF
         then do
           (dAll, insideArr) <- trueSignedDistanceSupersampled width height offsetX offsetY segs range 8
           pure (Just dAll, Just insideArr)
         else pure (Nothing, Nothing)
       forM_ [0 .. height - 1] $ \y -> do
         forM_ [0 .. width - 1] $ \x -> do
           let i = y * width + x
               px' = offsetX + fromIntegral x + 0.5
               py' = offsetY + fromIntegral y + 0.5 + 1e-6
               p = (px', py')
           inside <- case insideLow of
             Just arr -> readArray arr i
             Nothing -> pure (windingNumber segs p /= 0)
           let sign = if inside then 1.0 else -1.0
               dR0 = sign * minDistanceIndexed idxR p
               dG0 = sign * minDistanceIndexed idxG p
               dB0 = sign * minDistanceIndexed idxB p
               dAll0 = sign * minDistanceIndexed idxAll p
           writeArray dRArr i dR0
           writeArray dGArr i dG0
           writeArray dBArr i dB0
           writeArray dAllArr i dAll0
       let neighbors8 =
             [ (-1, -1), (0, -1), (1, -1)
             , (-1,  0),          (1,  0)
             , (-1,  1), (0,  1), (1,  1)
             ]
       dAllArr0' <- case dAllArr0 of
         Just arr -> pure arr
         Nothing -> pure dAllArr
       dAllArr' <- if speckleThreshold > 0
        then do
          let passes = 2 :: Int
          let pass src = do
                dst <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
                forM_ [0 .. height - 1] $ \y -> do
                  forM_ [0 .. width - 1] $ \x -> do
                    let i = y * width + x
                    d <- readArray src i
                    let sign :: Int
                        sign = if d < 0 then -1 else 1
                    (negCount, posCount, total) <-
                      foldM
                        (\(nC, pC, tC) (dx, dy) ->
                          let nx = x + dx
                              ny = y + dy
                          in if nx < 0 || nx >= width || ny < 0 || ny >= height
                             then pure (nC, pC, tC)
                             else do
                               let ni = ny * width + nx
                               nd <- readArray src ni
                               let s :: Int
                                   s = if nd < 0 then -1 else 1
                               pure (if s < 0 then (nC + 1, pC, tC + 1) else (nC, pC + 1, tC + 1))
                        ) (0 :: Int, 0 :: Int, 0 :: Int) neighbors8
                    let majoritySign
                          | negCount >= 5 = -1
                          | posCount >= 5 = 1
                          | otherwise = sign
                        d' =
                          if total >= 5 && majoritySign /= sign && abs d < speckleThreshold
                          then fromIntegral majoritySign * abs d
                          else d
                    writeArray dst i d'
                pure dst
          let loop i src =
                if i <= 0
                then pure src
                else do
                  dst <- pass src
                  loop (i - 1) dst
          loop passes dAllArr0'
        else pure dAllArr0'
       dAllArr'' <- pure dAllArr'
       out <- newArray (0, n * channels - 1) 0 :: ST s (STUArray s Int Word8)
       forM_ [0 .. height - 1] $ \y -> do
         forM_ [0 .. width - 1] $ \x -> do
           let i = y * width + x
               base = i * channels
           dR0 <- readArray dRArr i
           dG0 <- readArray dGArr i
           dB0 <- readArray dBArr i
           dAll <- readArray dAllArr'' i
           let sd = median3 dR0 dG0 dB0
               spread = max (abs (dR0 - dG0)) (max (abs (dR0 - dB0)) (abs (dG0 - dB0)))
               sameSign = sd * dAll >= 0
               conflict =
                 conflictThreshold > 0 &&
                 ( (abs dR0 < conflictThreshold && abs dG0 < conflictThreshold)
                || (abs dR0 < conflictThreshold && abs dB0 < conflictThreshold)
                || (abs dG0 < conflictThreshold && abs dB0 < conflictThreshold)
                 )
               forceAll = conflict || (correction > 0 && (not sameSign || abs (sd - dAll) > correction || spread > correction))
               (dR, dG, dB) =
                 if forceAll
                 then (dAll, dAll, dAll)
                 else (dR0, dG0, dB0)
               r = distanceToByte range dR
               g = distanceToByte range dG
               b = distanceToByte range dB
           case fmt of
             BitmapMSDF -> do
               writeArray out base r
               writeArray out (base + 1) g
               writeArray out (base + 2) b
             BitmapMTSDF -> do
               writeArray out base r
               writeArray out (base + 1) g
               writeArray out (base + 2) b
               writeArray out (base + 3) (distanceToByte range dAll)
       freeze out)

trueSignedDistanceSupersampled :: Int -> Int -> Double -> Double -> [LineSeg] -> Int -> Int -> ST s (STUArray s Int Double, STUArray s Int Bool)
trueSignedDistanceSupersampled width height offsetX offsetY segs range ss = do
  let pad = max 2 (range + 1)
      hiW = (width + 2 * pad) * ss
      hiH = (height + 2 * pad) * ss
      hiN = hiW * hiH
      inf = 1e20 :: Double
      offX = offsetX - fromIntegral pad
      offY = offsetY - fromIntegral pad
  hiMask <- newArray (0, hiN - 1) False :: ST s (STUArray s Int Bool)
  forM_ [0 .. hiH - 1] $ \hy -> do
    forM_ [0 .. hiW - 1] $ \hx -> do
      let px' = offX + (fromIntegral hx + 0.5) / fromIntegral ss + 1e-6
          py' = offY + (fromIntegral hy + 0.5) / fromIntegral ss + 1e-6
          inside = windingNumber segs (px', py') /= 0
          i = hy * hiW + hx
      writeArray hiMask i inside
  let fOut i = do
        inside <- readArray hiMask i
        pure (if inside then inf else 0)
      fIn i = do
        inside <- readArray hiMask i
        pure (if inside then 0 else inf)
  distOut <- distanceTransform2D hiW hiH fOut
  distIn <- distanceTransform2D hiW hiH fIn
  let n = width * height
  out <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
  insideLow <- newArray (0, n - 1) False :: ST s (STUArray s Int Bool)
  let blockArea = ss * ss
      insideThreshold = (blockArea `div` 2) + 1
  forM_ [0 .. height - 1] $ \y -> do
    forM_ [0 .. width - 1] $ \x -> do
      let baseX = (x + pad) * ss
          baseY = (y + pad) * ss
          hiCenter = (baseY + ss `div` 2) * hiW + (baseX + ss `div` 2)
          i = y * width + x
      insideCount <- foldM
        (\acc dy -> do
          let row = (baseY + dy) * hiW
          foldM
            (\acc2 dx -> do
              inside <- readArray hiMask (row + baseX + dx)
              pure (acc2 + if inside then 1 else 0)
            ) acc [0 .. ss - 1]
        ) (0 :: Int) [0 .. ss - 1]
      let inside = insideCount >= insideThreshold
      dOut <- readArray distOut hiCenter
      dIn <- readArray distIn hiCenter
      let dAbs = sqrt (max dOut dIn)
          d = (if inside then dAbs else -dAbs) / fromIntegral ss
      writeArray insideLow i inside
      writeArray out i d
  pure (out, insideLow)

distanceTransform2D :: Int -> Int -> (Int -> ST s Double) -> ST s (STUArray s Int Double)
distanceTransform2D width height f = do
  let n = width * height
  tmp <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
  out <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
  forM_ [0 .. height - 1] $ \y -> do
    let fRow x = f (y * width + x)
    dRow <- distanceTransform1DST width fRow
    forM_ [0 .. width - 1] $ \x -> do
      d <- readArray dRow x
      writeArray tmp (y * width + x) d
  forM_ [0 .. width - 1] $ \x -> do
    let fCol y = readArray tmp (y * width + x)
    dCol <- distanceTransform1DST height fCol
    forM_ [0 .. height - 1] $ \y -> do
      d <- readArray dCol y
      writeArray out (y * width + x) d
  pure out

distanceTransform1DST :: Int -> (Int -> ST s Double) -> ST s (STUArray s Int Double)
distanceTransform1DST n f = do
  v <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
  z <- newArray (0, n) 0.0 :: ST s (STUArray s Int Double)
  d <- newArray (0, n - 1) 0.0 :: ST s (STUArray s Int Double)
  let sq x = fromIntegral (x * x) :: Double
      findK k s q = do
        zk <- readArray z k
        if s <= zk
          then do
            let k' = k - 1
            vk' <- readArray v k'
            fq <- f q
            fvk <- f vk'
            let s' = ((fq + sq q) - (fvk + sq vk')) / (2 * fromIntegral (q - vk'))
            findK k' s' q
          else pure (k, s)
      advanceK kRef q = do
        k <- readSTRef kRef
        zk1 <- readArray z (k + 1)
        if zk1 < fromIntegral q
          then do
            let k' = k + 1
            writeSTRef kRef k'
            advanceK kRef q
          else pure k
  writeArray v 0 0
  writeArray z 0 (negate (1 / 0))
  writeArray z 1 (1 / 0)
  kRef <- newSTRef 0
  forM_ [1 .. n - 1] $ \q -> do
    k <- readSTRef kRef
    vk <- readArray v k
    fq <- f q
    fvk <- f vk
    let s0 = ((fq + sq q) - (fvk + sq vk)) / (2 * fromIntegral (q - vk))
    (k', s') <- findK k s0 q
    let k'' = k' + 1
    writeArray v k'' q
    writeArray z k'' s'
    writeArray z (k'' + 1) (1 / 0)
    writeSTRef kRef k''
  kRef2 <- newSTRef 0
  forM_ [0 .. n - 1] $ \q -> do
    k <- advanceK kRef2 q
    vk <- readArray v k
    fvk <- f vk
    let val = (fromIntegral (q - vk) ^ (2 :: Int)) + fvk
    writeArray d q val
  pure d

median3 :: Double -> Double -> Double -> Double
median3 a b c = max (min a b) (min (max a b) c)

distanceSqPointLine :: Vec2 -> Vec2 -> Vec2 -> Double
distanceSqPointLine (px', py') (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else ((px' - x0) * dx + (py' - y0) * dy) / len2
      t' = max 0 (min 1 t)
      cx = x0 + t' * dx
      cy = y0 + t' * dy
      ex = px' - cx
      ey = py' - cy
  in ex * ex + ey * ey

edgeDistanceSq :: Vec2 -> Edge -> Double
edgeDistanceSq p (EdgeLine a b) = distanceSqPointLine p a b
edgeDistanceSq p (EdgeQuad p0 p1 p2) = distanceSqPointQuad p0 p1 p2 p

distanceSqPointQuad :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double
distanceSqPointQuad p0 p1 p2 p =
  let a = vecSub (vecAdd p0 p2) (vecScale 2 p1)
      b = vecScale 2 (vecSub p1 p0)
      c = vecSub p0 p
      c3 = 2 * dot a a
      c2 = 3 * dot a b
      c1 = dot b b + 2 * dot a c
      c0 = dot b c
      roots = solveCubic c3 c2 c1 c0
      ts = filter (\t -> t >= 0 && t <= 1) roots ++ [0,1]
  in minimum [ distanceSq (bezier p0 p1 p2 t) p | t <- ts ]

bezier :: Vec2 -> Vec2 -> Vec2 -> Double -> Vec2
bezier p0 p1 p2 t =
  let u = 1 - t
      tt = t * t
      uu = u * u
      p0' = vecScale uu p0
      p1' = vecScale (2 * u * t) p1
      p2' = vecScale tt p2
  in vecAdd p0' (vecAdd p1' p2')

distanceSq :: Vec2 -> Vec2 -> Double
distanceSq (x0,y0) (x1,y1) =
  let dx = x0 - x1
      dy = y0 - y1
  in dx * dx + dy * dy

dot :: Vec2 -> Vec2 -> Double
dot (x0,y0) (x1,y1) = x0 * x1 + y0 * y1

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

vecSub :: Vec2 -> Vec2 -> Vec2
vecSub (x0,y0) (x1,y1) = (x0 - x1, y0 - y1)

vecScale :: Double -> Vec2 -> Vec2
vecScale s (x,y) = (s * x, s * y)

solveCubic :: Double -> Double -> Double -> Double -> [Double]
solveCubic a b c d
  | abs a < 1e-12 = solveQuadratic b c d
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
           in [u + v - a' / 3]
         else if abs disc < 1e-12
              then
                let u = cbrt (-q / 2)
                in [2 * u - a' / 3, -u - a' / 3]
              else
                let r = sqrt (-(p * p * p) / 27)
                    phi = acos (-q / (2 * r))
                    t = 2 * cbrt r
                in [ t * cos (phi / 3) - a' / 3
                   , t * cos ((phi + 2 * pi) / 3) - a' / 3
                   , t * cos ((phi + 4 * pi) / 3) - a' / 3
                   ]

solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c
  | abs a < 1e-12 = solveLinear b c
  | otherwise =
      let disc = b * b - 4 * a * c
      in if disc < 0
         then []
         else
           let sqrtDisc = sqrt disc
               t1 = (-b + sqrtDisc) / (2 * a)
               t2 = (-b - sqrtDisc) / (2 * a)
           in [t1, t2]

solveLinear :: Double -> Double -> [Double]
solveLinear a b
  | abs a < 1e-12 = []
  | otherwise = [-b / a]

cbrt :: Double -> Double
cbrt x = if x < 0 then -((abs x) ** (1 / 3)) else x ** (1 / 3)

distanceToByte :: Int -> Double -> Word8
distanceToByte range dist =
  let r = fromIntegral range :: Double
      v = 0.5 + dist / (2 * r)
      v' = max 0 (min 1 v)
  in fromIntegral (round (v' * 255) :: Int)
