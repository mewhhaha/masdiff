{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.TextRender
  ( ShaderCfg (..),
    ScreenPxRange (..),
    mkShaderCfg,
    shadeMtsdfImg,
    shadeMtsdfImgTo,
    resampleBilinear,
    solidImg,
    hcatWithGap,
    addBorder,
  )
where

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8)
import MSDF.Types (ImgRGBA8 (..), mkImgRGBA8)

data ShaderCfg = ShaderCfg
  { spr :: !ScreenPxRange,
    alphaFallback :: !Bool,
    fallbackThreshold :: !Double,
    ssaa :: !Int
  }
  deriving stock (Eq, Show)

data ScreenPxRange
  = AutoPxRange !Double
  | FixedPxRange !Double
  deriving stock (Eq, Show)

mkShaderCfg :: ScreenPxRange -> Bool -> Double -> Either String ShaderCfg
mkShaderCfg spr alphaFallback fallbackThreshold
  | not (isFinite pxRange) || pxRange <= 0 = Left "screenPxRange must be finite and > 0."
  | not (isFinite fallbackThreshold) || fallbackThreshold < 0 = Left "fallbackThreshold must be finite and >= 0."
  | otherwise =
      Right
        ShaderCfg
          { spr = spr,
            alphaFallback = alphaFallback,
            fallbackThreshold = fallbackThreshold,
            ssaa = 4
          }
  where
    pxRange = case spr of
      AutoPxRange x -> x
      FixedPxRange x -> x

shadeMtsdfImg :: ShaderCfg -> ImgRGBA8 -> Either String ImgRGBA8
shadeMtsdfImg shader img = shadeMtsdfImgTo shader img.w img.h img

shadeMtsdfImgTo :: ShaderCfg -> Int -> Int -> ImgRGBA8 -> Either String ImgRGBA8
shadeMtsdfImgTo shader outW outH img
  | outW <= 0 || outH <= 0 = Left "shade output dimensions must be > 0."
  | otherwise = mkImgRGBA8 outW outH (BS.pack pixels)
  where
    screenPxRange = resolveScreenPxRange shader.spr img.w img.h outW outH
    side = max 1 shader.ssaa
    offsets =
      [ ((fromIntegral ix + 0.5) / fromIntegral side) - 0.5
        | ix <- [0 .. side - 1]
      ]
    subSamples =
      [ (ox, oy)
        | oy <- offsets,
          ox <- offsets
      ]
    nSamples = fromIntegral (length subSamples)

    pixels =
      concat
        [ let gray = g
           in [gray, gray, gray, 255]
          | g <- finalGrays
        ]

    rawCovs =
      [ pixelCoverage x y
        | y <- [0 .. outH - 1],
          x <- [0 .. outW - 1]
      ]

    healedCovs = healSeams outW outH (healSeams outW outH rawCovs)
    finalGrays =
      healGraySpecks
        outW
        outH
        (healGraySpecks outW outH (fmap covToGray healedCovs))

    sampleAt :: Double -> Double -> Double
    sampleAt uvx uvy =
      let sample =
            mix4At
              img
              ((uvx * fromIntegral img.w) - 0.5)
              ((uvy * fromIntegral img.h) - 0.5)
       in sampleCoverage screenPxRange shader sample

    pixelCoverage :: Int -> Int -> Double
    pixelCoverage x y =
      foldl'
        ( \acc (ox, oy) ->
            acc
              + sampleAt
                ((fromIntegral x + 0.5 + ox) / fromIntegral outW)
                ((fromIntegral y + 0.5 + oy) / fromIntegral outH)
        )
        0
        subSamples
        / nSamples

resolveScreenPxRange :: ScreenPxRange -> Int -> Int -> Int -> Int -> Double
resolveScreenPxRange spr srcW srcH outW outH =
  case spr of
    FixedPxRange fixed -> fixed
    AutoPxRange pxRange ->
      max
        1.0
        (0.5 * (pxRange / fromIntegral srcW * fromIntegral outW + pxRange / fromIntegral srcH * fromIntegral outH))

resampleBilinear :: Int -> Int -> ImgRGBA8 -> Either String ImgRGBA8
resampleBilinear outW outH img
  | outW <= 0 || outH <= 0 = Left "resample output dimensions must be > 0."
  | otherwise =
      mkImgRGBA8 outW outH (BS.pack pixels)
  where
    inW = fromIntegral img.w
    inH = fromIntegral img.h
    mapX x = (((fromIntegral x + 0.5) / fromIntegral outW) * inW) - 0.5
    mapY y = (((fromIntegral y + 0.5) / fromIntegral outH) * inH) - 0.5
    toWord8Unit x = round (255 * clamp01 x)
    pxOut x y =
      let sample = mix4At img (mapX x) (mapY y)
       in [ toWord8Unit (fst4 sample),
            toWord8Unit (snd4 sample),
            toWord8Unit (trd4 sample),
            toWord8Unit (frt4 sample)
          ]
    pixels =
      concat
        [ pxOut x y
          | y <- [0 .. outH - 1],
            x <- [0 .. outW - 1]
        ]

solidImg :: Int -> Int -> (Word8, Word8, Word8, Word8) -> Either String ImgRGBA8
solidImg w h (r, g, b, a) =
  mkImgRGBA8 w h (BS.pack (concat (replicate (w * h) [r, g, b, a])))

hcatWithGap :: Int -> [ImgRGBA8] -> Either String ImgRGBA8
hcatWithGap gap imgs
  | gap < 0 = Left "gap must be >= 0."
  | otherwise =
      case imgs of
        [] -> Left "Need at least one image to compose."
        firstImg : _ ->
          let h0 = firstImg.h
           in if any (\img -> img.h /= h0) imgs
                then Left "All images must have the same height."
                else do
                  let totalW = sum (fmap (.w) imgs) + gap * (length imgs - 1)
                  let gapBytes = BS.replicate (gap * 4) 255
                  let row y =
                        BS.intercalate gapBytes $
                          fmap (\img -> sliceRow img y) imgs
                  let px = BS.concat [row y | y <- [0 .. h0 - 1]]
                  mkImgRGBA8 totalW h0 px

addBorder :: Int -> ImgRGBA8 -> Either String ImgRGBA8
addBorder border img
  | border < 0 = Left "border must be >= 0."
  | border == 0 = Right img
  | otherwise = do
      let wOut = img.w + (2 * border)
      let hOut = img.h + (2 * border)
      let whiteRow = BS.replicate (wOut * 4) 255
      let sidePad = BS.replicate (border * 4) 255
      let midRows =
            [ sidePad <> sliceRow img y <> sidePad
              | y <- [0 .. img.h - 1]
            ]
      let px =
            BS.concat
              ( replicate border whiteRow
                  <> midRows
                  <> replicate border whiteRow
              )
      mkImgRGBA8 wOut hOut px

sampleCoverage :: Double -> ShaderCfg -> (Double, Double, Double, Double) -> Double
sampleCoverage spr shader (r, g, b, a) =
  if shader.alphaFallback && abs (msdfSd - sdfSd) > shader.fallbackThreshold
    then max msdfCov sdfCov
    else msdfCov
  where
    msdfSd = median3 r g b - 0.5
    sdfSd = a - 0.5
    msdfCov = clamp01 ((spr * msdfSd) + 0.5)
    sdfCov = clamp01 ((spr * sdfSd) + 0.5)

rgbaAt :: ImgRGBA8 -> Int -> Int -> (Double, Double, Double, Double)
rgbaAt img x y =
  let i = ((y * img.w) + x) * 4
   in ( toUnit (BS.index img.px i),
        toUnit (BS.index img.px (i + 1)),
        toUnit (BS.index img.px (i + 2)),
        toUnit (BS.index img.px (i + 3))
      )

mix4At :: ImgRGBA8 -> Double -> Double -> (Double, Double, Double, Double)
mix4At img x y =
  let sx = clamp 0 (fromIntegral (img.w - 1)) x
      sy = clamp 0 (fromIntegral (img.h - 1)) y
      x0 = floor sx
      y0 = floor sy
      x1 = min (img.w - 1) (x0 + 1)
      y1 = min (img.h - 1) (y0 + 1)
      tx = sx - fromIntegral x0
      ty = sy - fromIntegral y0
      c00 = rgbaAt img x0 y0
      c10 = rgbaAt img x1 y0
      c01 = rgbaAt img x0 y1
      c11 = rgbaAt img x1 y1
   in mix4 tx ty c00 c10 c01 c11

mix4 ::
  Double ->
  Double ->
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double)
mix4 tx ty c00 c10 c01 c11 =
  ( bilerp tx ty (fst4 c00) (fst4 c10) (fst4 c01) (fst4 c11),
    bilerp tx ty (snd4 c00) (snd4 c10) (snd4 c01) (snd4 c11),
    bilerp tx ty (trd4 c00) (trd4 c10) (trd4 c01) (trd4 c11),
    bilerp tx ty (frt4 c00) (frt4 c10) (frt4 c01) (frt4 c11)
  )

bilerp :: Double -> Double -> Double -> Double -> Double -> Double -> Double
bilerp tx ty v00 v10 v01 v11 =
  lerp ty (lerp tx v00 v10) (lerp tx v01 v11)

lerp :: Double -> Double -> Double -> Double
lerp t a b = a + (t * (b - a))

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

trd4 :: (a, b, c, d) -> c
trd4 (_, _, c, _) = c

frt4 :: (a, b, c, d) -> d
frt4 (_, _, _, d) = d

covToGray :: Double -> Word8
covToGray cov =
  round (255 * (1 - clamp01 cov))

healGraySpecks :: Int -> Int -> [Word8] -> [Word8]
healGraySpecks w h grays
  | w <= 0 || h <= 0 = grays
  | otherwise =
      [ healPixel x y
        | y <- [0 .. h - 1],
          x <- [0 .. w - 1]
      ]
  where
    gMap = IM.fromDistinctAscList (zip [0 ..] grays)

    grayAt :: Int -> Int -> Int
    grayAt x y =
      let cx = clampInt 0 (w - 1) x
          cy = clampInt 0 (h - 1) y
       in fromIntegral (IM.findWithDefault 255 ((cy * w) + cx) gMap)

    neighbors :: Int -> Int -> [Int]
    neighbors x y =
      [ grayAt (x + dx) (y + dy)
        | dy <- [-1 .. 1],
          dx <- [-1 .. 1],
          not (dx == 0 && dy == 0)
      ]

    healPixel :: Int -> Int -> Word8
    healPixel x y =
      let g = grayAt x y
          ns = neighbors x y
          nMax = maximum ns
          nMin = minimum ns
          nAvg = sum ns `div` length ns
          darkNs = length (filter (<= 72) ns)
          brightBump = g - nAvg >= 10
          -- Treat isolated light pinholes by checking neighborhood darkness, not center darkness.
          deepInk = nAvg <= 72
          isolatedBump =
            deepInk
              && nMax <= 128
              && darkNs >= 6
              && brightBump
       in if isolatedBump
            then fromIntegral nMin
            else fromIntegral g

healSeams :: Int -> Int -> [Double] -> [Double]
healSeams w h covs
  | w <= 0 || h <= 0 = covs
  | otherwise =
      [ healPixel x y
        | y <- [0 .. h - 1],
          x <- [0 .. w - 1]
      ]
  where
    covMap = IM.fromDistinctAscList (zip [0 ..] covs)

    covAt :: Int -> Int -> Double
    covAt x y =
      let cx = clampInt 0 (w - 1) x
          cy = clampInt 0 (h - 1) y
       in IM.findWithDefault 0.0 ((cy * w) + cx) covMap

    neighborhood :: Int -> Int -> [Double]
    neighborhood x y =
      [ covAt (x + dx) (y + dy)
        | dy <- [-1 .. 1],
          dx <- [-1 .. 1],
          not (dx == 0 && dy == 0)
      ]

    healPixel :: Int -> Int -> Double
    healPixel x y =
      let c = covAt x y
          left = covAt (x - 1) y
          right = covAt (x + 1) y
          up = covAt x (y - 1)
          down = covAt x (y + 1)
          ns = neighborhood x y
          nMin = minimum ns
          nMax = maximum ns
          orthNeighbors = [left, right, up, down]
          diagNeighbors =
            [ covAt (x - 1) (y - 1),
              covAt (x + 1) (y - 1),
              covAt (x - 1) (y + 1),
              covAt (x + 1) (y + 1)
            ]
          orthHighCount = countAbove 0.82 orthNeighbors
          diagHighCount = countAbove 0.78 diagNeighbors
          orthLowCount = countBelow 0.70 orthNeighbors
          highCount = countAbove 0.80 ns
          stable = (nMax - nMin) < 0.30
          orthMin = minimum orthNeighbors
          orthAvg = (left + right + up + down) / 4
          fillValue = (left + right + up + down) / 4
          isolatedHole =
            c < 0.60
              && minimum orthNeighbors > 0.84
              && minimum diagNeighbors > 0.72
          hardSpeck =
            c < 0.95
              && highCount >= 7
              && orthMin > 0.82
              && (nMax - c) > 0.05
          microPit =
            c < 0.90
              && orthMin > 0.82
              && highCount >= 7
              && (orthAvg - c) > 0.035
          pinhole =
            c < 0.72
              && stable
              && (nMax - c) > 0.14
              && highCount >= 5
              && orthHighCount >= 2
              && diagHighCount >= 1
              && orthLowCount <= 1
          cusp =
            c < 0.68
              && stable
              && (nMax - c) > 0.12
              && highCount >= 5
              && orthHighCount >= 2
              && diagHighCount >= 1
              && orthLowCount <= 1
              && ( (left > 0.82 && right > 0.82)
                     || (left > 0.82 && up > 0.82)
                     || (left > 0.82 && down > 0.82)
                     || (right > 0.82 && up > 0.82)
                     || (right > 0.82 && down > 0.82)
                     || (up > 0.82 && down > 0.82)
                 )
       in if hardSpeck
            then nMax
            else
              if isolatedHole || microPit || pinhole || cusp
                then max c fillValue
                else c

    countAbove :: Double -> [Double] -> Int
    countAbove threshold values = length (filter (> threshold) values)

    countBelow :: Double -> [Double] -> Int
    countBelow threshold values = length (filter (< threshold) values)

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

toUnit :: Word8 -> Double
toUnit x = fromIntegral x / 255.0

median3 :: Double -> Double -> Double -> Double
median3 x y z = max (min x y) (min (max x y) z)

clamp01 :: Double -> Double
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

clamp :: Double -> Double -> Double -> Double
clamp lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

sliceRow :: ImgRGBA8 -> Int -> BS.ByteString
sliceRow img y =
  BS.take rowBytes (BS.drop (y * rowBytes) img.px)
  where
    rowBytes = img.w * 4

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
