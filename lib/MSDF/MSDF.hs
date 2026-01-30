module MSDF.MSDF
  ( MSDFConfig(..)
  , GlyphSet(..)
  , defaultMSDFConfig
  , renderGlyphMSDF
  , glyphMetricsOnly
  ) where

import Data.Array ( (!) )
import Data.Array.Unboxed (UArray, listArray)
import Data.List (sort)
import Data.Word (Word8)
import MSDF.Outline
import MSDF.TTF.Parser
import MSDF.Types

-- | Configuration for MSDF generation.
data MSDFConfig = MSDFConfig
  { cfgPixelSize :: Int
  , cfgRange :: Int
  , cfgCornerThreshold :: Double
  , cfgGlyphSet :: GlyphSet
  , cfgParallelism :: Int
  }

-- | Glyph subset selection.
data GlyphSet
  = GlyphSetNone
  | GlyphSetAll
  | GlyphSetCodepoints [Int]

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
  { cfgPixelSize = 32
  , cfgRange = 4
  , cfgCornerThreshold = 3.0
  , cfgGlyphSet = GlyphSetAll
  , cfgParallelism = 0
  }

renderGlyphMSDF :: MSDFConfig -> TTF -> Int -> GlyphMSDF
renderGlyphMSDF cfg ttf glyphIndex =
  let contours = glyphOutline ttf glyphIndex
      base = glyphMetricsOnly cfg ttf glyphIndex
      bbox = glyphBBox base
      unitsPerEm = headUnitsPerEm (ttfHead ttf)
      scale = fromIntegral (cfgPixelSize cfg) / fromIntegral unitsPerEm
      safeRange = max 1 (cfgRange cfg)
  in if null contours
     then base
     else
       let coloredEdges = concatMap (colorContourEdges (cfgCornerThreshold cfg)) (splitContoursEdges contours scale)
           allLines = concatMap (flattenEdge 0.25) (map snd coloredEdges)
           colorEdges = splitColoredEdges coloredEdges
           padding = fromIntegral (cfgRange cfg + 1) :: Double
           width = max 1 (ceiling ((bboxXMax bbox - bboxXMin bbox) + 2 * padding))
           height = max 1 (ceiling ((bboxYMax bbox - bboxYMin bbox) + 2 * padding))
           offsetX = bboxXMin bbox - padding
           offsetY = bboxYMin bbox - padding
           pixels = renderBitmap width height offsetX offsetY safeRange colorEdges allLines
       in GlyphMSDF
            { glyphIndex = glyphIndex
            , glyphCodepoints = []
            , glyphAdvance = glyphAdvance base
            , glyphBearingX = glyphBearingX base
            , glyphBearingY = glyphBearingY base
            , glyphBBox = bbox
            , glyphBitmap = MSDFBitmap
                { bmpWidth = width
                , bmpHeight = height
                , bmpOffsetX = offsetX
                , bmpOffsetY = offsetY
                , bmpPixels = pixels
                }
            }

glyphMetricsOnly :: MSDFConfig -> TTF -> Int -> GlyphMSDF
glyphMetricsOnly cfg ttf glyphIndex =
  let (xMin, yMin, xMax, yMax) = glyphBBoxRaw ttf glyphIndex
      metricsGlyph = case compositeMetricsGlyph ttf glyphIndex of
                       Just g -> g
                       Nothing -> glyphIndex
      unitsPerEm = headUnitsPerEm (ttfHead ttf)
      scale = fromIntegral (cfgPixelSize cfg) / fromIntegral unitsPerEm
      advance = fromIntegral (hmtxAdvances (ttfHmtx ttf) ! metricsGlyph) * scale
      lsb = fromIntegral (hmtxLSB (ttfHmtx ttf) ! metricsGlyph) * scale
      bearingY = fromIntegral yMax * scale
      bbox = BBox
        { bboxXMin = fromIntegral xMin * scale
        , bboxYMin = fromIntegral yMin * scale
        , bboxXMax = fromIntegral xMax * scale
        , bboxYMax = fromIntegral yMax * scale
        }
  in GlyphMSDF
      { glyphIndex = glyphIndex
      , glyphCodepoints = []
      , glyphAdvance = advance
      , glyphBearingX = lsb
      , glyphBearingY = bearingY
      , glyphBBox = bbox
      , glyphBitmap = emptyBitmap
      }

emptyBitmap :: MSDFBitmap
emptyBitmap = MSDFBitmap
  { bmpWidth = 0
  , bmpHeight = 0
  , bmpOffsetX = 0
  , bmpOffsetY = 0
  , bmpPixels = listArray (0, -1) []
  }

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
     then zipWith (\i e -> (i `mod` 3, e)) [0..] edges
     else
       let starts = uniqueSorted cornerStarts
           segments = buildSegments edges starts
           coloredSegments = zipWith (\c seg -> map (\e -> (c, e)) seg) (cycle [0,1,2]) segments
       in concat coloredSegments

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

-- | Line segment representation.
type LineSeg = (Vec2, Vec2)

renderBitmap :: Int -> Int -> Double -> Double -> Int -> ([Edge], [Edge], [Edge]) -> [LineSeg] -> UArray Int Word8
renderBitmap width height offsetX offsetY range (edgesR, edgesG, edgesB) allLines =
  let pixels = [ channelValue
               | y <- [0 .. height - 1]
               , x <- [0 .. width - 1]
               , let p = (offsetX + fromIntegral x + 0.5, offsetY + fromIntegral y + 0.5)
                     inside = windingNumber allLines p /= 0
                     dR = signedDistance inside edgesR p
                     dG = signedDistance inside edgesG p
                     dB = signedDistance inside edgesB p
               , channelValue <- [ distanceToByte range dR
                                 , distanceToByte range dG
                                 , distanceToByte range dB ]
               ]
  in listArray (0, length pixels - 1) pixels

signedDistance :: Bool -> [Edge] -> Vec2 -> Double
signedDistance inside edges p =
  let dist = minDistance edges p
  in if inside then -dist else dist

minDistance :: [Edge] -> Vec2 -> Double
minDistance [] _ = 1e9
minDistance edges p =
  sqrt (foldl' min 1e18 [ edgeDistanceSq p e | e <- edges ])

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
