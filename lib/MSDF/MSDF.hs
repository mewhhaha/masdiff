module MSDF.MSDF
  ( MSDFConfig(..)
  , GlyphSet(..)
  , defaultMSDFConfig
  , renderGlyphMSDF
  , glyphMetricsOnly
  ) where

import Data.Array ( (!) )
import Data.Array.Unboxed (UArray, listArray)
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
  }

-- | Glyph subset selection.
data GlyphSet
  = GlyphSetAll
  | GlyphSetCodepoints [Int]

-- | Default configuration.
defaultMSDFConfig :: MSDFConfig
defaultMSDFConfig = MSDFConfig
  { cfgPixelSize = 32
  , cfgRange = 4
  , cfgCornerThreshold = 3.0
  , cfgGlyphSet = GlyphSetAll
  }

renderGlyphMSDF :: MSDFConfig -> TTF -> Int -> Maybe GlyphMSDF
renderGlyphMSDF cfg ttf glyphIndex =
  let contours = glyphOutline ttf glyphIndex
      base = glyphMetricsOnly cfg ttf glyphIndex
      bbox = glyphBBox base
      unitsPerEm = headUnitsPerEm (ttfHead ttf)
      scale = fromIntegral (cfgPixelSize cfg) / fromIntegral unitsPerEm
  in if null contours
     then Just base
     else
       let coloredEdges = concatMap (colorContourEdges (cfgCornerThreshold cfg)) (splitContoursEdges contours scale)
           allLines = concatMap (flattenEdge 0.25) (map snd coloredEdges)
           colorLines = splitColoredLines coloredEdges
           padding = fromIntegral (cfgRange cfg + 1) :: Double
           width = max 1 (ceiling ((bboxXMax bbox - bboxXMin bbox) + 2 * padding))
           height = max 1 (ceiling ((bboxYMax bbox - bboxYMin bbox) + 2 * padding))
           offsetX = bboxXMin bbox - padding
           offsetY = bboxYMin bbox - padding
           pixels = renderBitmap width height offsetX offsetY (cfgRange cfg) colorLines allLines
       in Just GlyphMSDF
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
      unitsPerEm = headUnitsPerEm (ttfHead ttf)
      scale = fromIntegral (cfgPixelSize cfg) / fromIntegral unitsPerEm
      advance = fromIntegral (hmtxAdvances (ttfHmtx ttf) ! glyphIndex) * scale
      lsb = fromIntegral (hmtxLSB (ttfHmtx ttf) ! glyphIndex) * scale
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
      dot = x1 * x2 + y1 * y2
      len1 = sqrt (x1 * x1 + y1 * y1)
      len2 = sqrt (x2 * x2 + y2 * y2)
  in if len1 == 0 || len2 == 0
     then False
     else
       let cosang = max (-1) (min 1 (dot / (len1 * len2)))
           ang = acos cosang
       in ang < threshold

uniqueSorted :: [Int] -> [Int]
uniqueSorted = foldl' (\acc x -> if null acc || last acc /= x then acc ++ [x] else acc) [] . sort
  where
    sort = foldl' insert []
    insert [] x = [x]
    insert (y:ys) x
      | x <= y = x:y:ys
      | otherwise = y : insert ys x

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

splitColoredLines :: [(Int, Edge)] -> ([LineSeg], [LineSeg], [LineSeg])
splitColoredLines colored =
  let toLines (c, e) = map (\(a,b) -> (c, a, b)) (flattenEdge 0.25 e)
      linesAll = concatMap toLines colored
      linesByColor c = [ (a,b) | (col,a,b) <- linesAll, col == c ]
  in (linesByColor 0, linesByColor 1, linesByColor 2)

-- | Line segment representation.
type LineSeg = (Vec2, Vec2)

renderBitmap :: Int -> Int -> Double -> Double -> Int -> ([LineSeg], [LineSeg], [LineSeg]) -> [LineSeg] -> UArray Int Word8
renderBitmap width height offsetX offsetY range (linesR, linesG, linesB) allLines =
  let pixels = [ channelValue
               | y <- [0 .. height - 1]
               , x <- [0 .. width - 1]
               , let p = (offsetX + fromIntegral x + 0.5, offsetY + fromIntegral y + 0.5)
                     inside = windingNumber allLines p /= 0
                     dR = signedDistance inside linesR p
                     dG = signedDistance inside linesG p
                     dB = signedDistance inside linesB p
               , channelValue <- [ distanceToByte range dR
                                 , distanceToByte range dG
                                 , distanceToByte range dB ]
               ]
  in listArray (0, length pixels - 1) pixels

signedDistance :: Bool -> [LineSeg] -> Vec2 -> Double
signedDistance inside segs p =
  let dist = minDistance segs p
  in if inside then -dist else dist

minDistance :: [LineSeg] -> Vec2 -> Double
minDistance [] _ = 1e9
minDistance segs p =
  sqrt (foldl' min 1e18 [ distanceSqPointLine p a b | (a,b) <- segs ])

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

distanceToByte :: Int -> Double -> Word8
distanceToByte range dist =
  let r = fromIntegral range :: Double
      v = 0.5 + dist / (2 * r)
      v' = max 0 (min 1 v)
  in fromIntegral (round (v' * 255) :: Int)
