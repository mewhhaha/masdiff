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

import Data.Array (Array, array, accumArray, bounds, (!) )
import Data.Array.Unboxed (UArray, listArray)
import Data.List (sort)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
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
  , msdfCorrectionThreshold = 0.5
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
       let coloredEdges = concatMap (colorContourEdges cfg.cornerThreshold) (splitContoursEdges contours scale)
           allLines = concatMap (flattenEdge 0.25) (map snd coloredEdges)
           colorEdges = splitColoredEdges coloredEdges
           padding = fromIntegral (safeRange + 1) :: Double
           width = max 1 (ceiling ((bbox.xMax - bbox.xMin) + 2 * padding))
           height = max 1 (ceiling ((bbox.yMax - bbox.yMin) + 2 * padding))
           offsetX = bbox.xMin - padding
           offsetY = bbox.yMin - padding
           pixels = renderBitmap width height offsetX offsetY safeRange correction colorEdges allLines
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
      , bitmap = emptyBitmap
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

emptyBitmap :: MSDFBitmap
emptyBitmap = MSDFBitmap
  { width = 0
  , height = 0
  , offsetX = 0
  , offsetY = 0
  , pixels = listArray (0, -1) []
  }

normalizeContours :: [[Point]] -> [[Point]]
normalizeContours contours =
  let cleaned = map normalizeContourRaw (filter (not . null) contours)
      infos = map contourInfo cleaned
      orient (idx, info) =
        let depth = contourDepth idx info infos
            desiredCCW = even depth
            area = signedArea info.contour
            isCCW = area >= 0
        in if isCCW == desiredCCW then info.contour else reverseContour info.contour
  in map orient (zip [0..] infos)

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

data ContourInfo = ContourInfo
  { contour :: [Point]
  , samplePoint :: Vec2
  , bbox :: BBox
  , segments :: [LineSeg]
  }

contourInfo :: [Point] -> ContourInfo
contourInfo pts =
  let pts' = if null pts then [] else pts
      (sx, sy) = case pts' of
        (p:_) -> (p.x, p.y)
        [] -> (0, 0)
      bbox' = contourBBox pts'
      segments' = contourSegments pts'
  in ContourInfo
     { contour = pts'
     , samplePoint = (sx, sy)
     , bbox = bbox'
     , segments = segments'
     }

contourBBox :: [Point] -> BBox
contourBBox pts =
  case pts of
    [] -> BBox 0 0 0 0
    _ ->
      let xs = map (\p -> p.x) pts
          ys = map (\p -> p.y) pts
      in BBox
         { xMin = minimum xs
         , yMin = minimum ys
         , xMax = maximum xs
         , yMax = maximum ys
         }

contourSegments :: [Point] -> [LineSeg]
contourSegments pts =
  let edges = contourToEdges pts
  in concatMap (flattenEdge 0.25) edges

contourDepth :: Int -> ContourInfo -> [ContourInfo] -> Int
contourDepth idx info infos =
  length
    [ ()
    | (j, other) <- zip [0..] infos
    , j /= idx
    , bboxContains other.bbox info.samplePoint
    , windingNumber other.segments info.samplePoint /= 0
    ]

bboxContains :: BBox -> Vec2 -> Bool
bboxContains bb (px', py') =
  px' >= bb.xMin && px' <= bb.xMax && py' >= bb.yMin && py' <= bb.yMax

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

reverseContour :: [Point] -> [Point]
reverseContour pts =
  case reverse pts of
    [] -> []
    xs -> xs

signedArea :: [Point] -> Double
signedArea pts =
  case pts of
    [] -> 0
    [_] -> 0
    _ ->
      let pairs = zip pts (drop 1 pts)
          sumCross = foldl' (\acc (a,b) -> acc + (a.x * b.y - b.x * a.y)) 0 pairs
      in 0.5 * sumCross

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

colorContourEdges :: Double -> [Edge] -> [(Int, Edge)]
colorContourEdges _ [] = []
colorContourEdges cornerThreshold edges =
  let n = length edges
      joinCorners = [ isCorner (edges !! i) (edges !! ((i + 1) `mod` n)) cornerThreshold
                    | i <- [0 .. n - 1] ]
      cornerStarts = [ (i + 1) `mod` n | i <- [0 .. n - 1], joinCorners !! i ]
  in if null cornerStarts
     then resolveEdgeColorConflicts (zipWith (\i e -> (i `mod` 3, e)) [0..] edges)
     else
       let starts = uniqueSorted cornerStarts
           segments = buildSegments edges starts
           coloredSegments = zipWith (\c seg -> map (\e -> (c, e)) seg) (cycle [0,1,2]) segments
       in resolveEdgeColorConflicts (concat coloredSegments)

isCorner :: Edge -> Edge -> Double -> Bool
isCorner e1 e2 threshold =
  let (x1, y1) = edgeEndDir e1
      (x2, y2) = edgeStartDir e2
      dotp = x1 * x2 + y1 * y2
      len1 = sqrt (x1 * x1 + y1 * y1)
      len2 = sqrt (x2 * x2 + y2 * y2)
  in if len1 == 0 || len2 == 0
     then False
     else
       let cosang = max (-1) (min 1 (dotp / (len1 * len2)))
           ang = acos cosang
       in ang < threshold

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

resolveEdgeColorConflicts :: [(Int, Edge)] -> [(Int, Edge)]
resolveEdgeColorConflicts colored =
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

-- | Line segment representation.
type LineSeg = (Vec2, Vec2)

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

edgeBBox :: Edge -> BBox
edgeBBox (EdgeLine (x0, y0) (x1, y1)) =
  BBox
    { xMin = min x0 x1
    , yMin = min y0 y1
    , xMax = max x0 x1
    , yMax = max y0 y1
    }
edgeBBox (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
  BBox
    { xMin = minimum [x0, x1, x2]
    , yMin = minimum [y0, y1, y2]
    , xMax = maximum [x0, x1, x2]
    , yMax = maximum [y0, y1, y2]
    }

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

renderBitmap :: Int -> Int -> Double -> Double -> Int -> Double -> ([Edge], [Edge], [Edge]) -> [LineSeg] -> UArray Int Word8
renderBitmap width height offsetX offsetY range correction (edgesR, edgesG, edgesB) allLines =
  let edgesAll = edgesR ++ edgesG ++ edgesB
      cellSz = max 4 (fromIntegral range)
      idxR = buildEdgeIndex cellSz offsetX offsetY width height edgesR
      idxG = buildEdgeIndex cellSz offsetX offsetY width height edgesG
      idxB = buildEdgeIndex cellSz offsetX offsetY width height edgesB
      idxAll = buildEdgeIndex cellSz offsetX offsetY width height edgesAll
      pixels = [ channelValue
               | y <- [0 .. height - 1]
               , x <- [0 .. width - 1]
               , let p = (offsetX + fromIntegral x + 0.5, offsetY + fromIntegral y + 0.5)
                     inside = windingNumber allLines p /= 0
                     dR0 = signedDistance inside idxR p
                     dG0 = signedDistance inside idxG p
                     dB0 = signedDistance inside idxB p
                     dAll = signedDistance inside idxAll p
                     sd = median3 dR0 dG0 dB0
                     spread = max (abs (dR0 - dG0)) (max (abs (dR0 - dB0)) (abs (dG0 - dB0)))
                     (dR, dG, dB) =
                       if correction > 0 && (abs (sd - dAll) > correction || spread > correction)
                       then (dAll, dAll, dAll)
                       else (dR0, dG0, dB0)
               , channelValue <- [ distanceToByte range dR
                                 , distanceToByte range dG
                                 , distanceToByte range dB ]
               ]
  in listArray (0, length pixels - 1) pixels

median3 :: Double -> Double -> Double -> Double
median3 a b c = max (min a b) (min (max a b) c)

signedDistance :: Bool -> EdgeIndex -> Vec2 -> Double
signedDistance inside idx p =
  let dist = minDistanceIndexed idx p
  in if inside then -dist else dist

windingNumber :: [LineSeg] -> Vec2 -> Int
windingNumber segs (px', py') =
  foldl' (\wn ((x0,y0),(x1,y1)) ->
    if y0 <= py'
    then if y1 > py' && isLeft (x0,y0) (x1,y1) (px',py') > 0 then wn + 1 else wn
    else if y1 <= py' && isLeft (x0,y0) (x1,y1) (px',py') < 0 then wn - 1 else wn
  ) 0 segs

isLeft :: Vec2 -> Vec2 -> Vec2 -> Double
isLeft (x0,y0) (x1,y1) (px',py') =
  (x1 - x0) * (py' - y0) - (px' - x0) * (y1 - y0)

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
