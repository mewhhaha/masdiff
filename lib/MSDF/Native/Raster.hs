{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native.Raster
  ( rasterizeOutline,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8)
import MSDF.Native.Types
  ( Channel (..),
    Contour (..),
    Edge (..),
    Outline (..),
    Pt (..),
    buildEdgeContours,
  )
import MSDF.Types
  ( GenCfg (..),
    GenErr (..),
    GenOut (..),
    ImgRGBA8,
    Metrics (..),
    mkImgRGBA8,
    unDim,
    unPxRange,
  )

data Frame = Frame
  { scale :: !Double,
    tx :: !Double,
    ty :: !Double,
    rangeLo :: !Double,
    rangeHi :: !Double
  }
  deriving stock (Eq, Show)

rasterizeOutline :: GenCfg -> Outline -> Either GenErr GenOut
rasterizeOutline cfg outline = do
  let dim = unDim cfg.dim
  let frame = computeFrame cfg outline
  let edgeContours = buildEdgeContours cfg.seed outline.contours
  let edges = concat edgeContours
  img <-
    if null edges
      then mkImage dim dim (BS.replicate (dim * dim * 4) 0)
      else do
        let pxRange = unPxRange cfg.pxr
        let rendered = renderImage dim frame pxRange outline.contours edgeContours
        mkImage dim dim rendered
  pure
    GenOut
      { img = img,
        metrics =
          Metrics
            { adv = outline.adv,
              bounds = outline.bounds,
              scale = Just frame.scale,
              translate = Just (frame.tx, frame.ty),
              range = Just (frame.rangeLo, frame.rangeHi)
            }
      }

mkImage :: Int -> Int -> ByteString -> Either GenErr ImgRGBA8
mkImage w h bytes =
  case mkImgRGBA8 w h bytes of
    Left err -> Left (ParseFailed ("Failed to build native RGBA image: " <> err))
    Right img -> Right img

computeFrame :: GenCfg -> Outline -> Frame
computeFrame cfg outline =
  if cfg.autoframe
    then frameAuto
    else
      Frame
        { scale = 1.0,
          tx = 0.0,
          ty = 0.0,
          rangeLo = negate (unPxRange cfg.pxr / 2.0),
          rangeHi = unPxRange cfg.pxr / 2.0
        }
  where
    (xmin, ymin, xmax, ymax) = outline.bounds
    dim = fromIntegral (unDim cfg.dim)
    pxr = unPxRange cfg.pxr
    lower = negate (pxr / 2.0)
    frameX = dim + (2.0 * lower)
    frameY = dim + (2.0 * lower)
    baseL
      | xmin < xmax = xmin
      | otherwise = 0.0
    baseB
      | ymin < ymax = ymin
      | otherwise = 0.0
    baseR
      | xmin < xmax = xmax
      | otherwise = 1.0
    baseT
      | ymin < ymax = ymax
      | otherwise = 1.0
    width = max 1.0e-9 (baseR - baseL)
    height = max 1.0e-9 (baseT - baseB)
    rawFrame =
      if frameX > 0.0 && frameY > 0.0
        then
          if width * frameY < height * frameX
            then
              let sc = frameY / height
                  tx0 = (0.5 * ((frameX / frameY) * height - width)) - baseL
                  ty0 = negate baseB
               in (sc, tx0, ty0)
            else
              let sc = frameX / width
                  tx0 = negate baseL
                  ty0 = (0.5 * ((frameY / frameX) * width - height)) - baseB
               in (sc, tx0, ty0)
        else (1.0, 0.0, 0.0)
    frameAuto =
      case rawFrame of
        (scRaw, txRaw, tyRaw)
          | isFinite scRaw && scRaw > 0 ->
              let txAdj = txRaw - (lower / scRaw)
                  tyAdj = tyRaw - (lower / scRaw)
                  lo = lower / scRaw
                  hi = negate lo
               in Frame
                    { scale = scRaw,
                      tx = txAdj,
                      ty = tyAdj,
                      rangeLo = lo,
                      rangeHi = hi
                    }
        _ ->
          Frame
            { scale = 1.0,
              tx = 0.0,
              ty = 0.0,
              rangeLo = lower,
              rangeHi = negate lower
            }

renderImage :: Int -> Frame -> Double -> [Contour] -> [[Edge]] -> ByteString
renderImage dim frame pxRange _contours edgeContours =
  BS.pack
    [ channel
      | px <- correctedPixels,
        channel <- encodePixel px
    ]
  where
    selectorContours = map contourSelectorInput edgeContours
    allEdges = concat edgeContours
    fillRows =
      [ scanlineRowFill dim frame allEdges y
        | y <- [0 .. dim - 1]
      ]

    rawPixels =
      [ samplePixel x y fill
        | (y, rowFill) <- zip [0 ..] fillRows,
          (x, fill) <- zip [0 ..] rowFill
      ]

    signCorrectedPixels = applyAmbiguousSignFix dim dim rawPixels
    correctionInputPixels = fmap floatLikePixel signCorrectedPixels
    correctionThreshold = minDeviationRatioModern / max 1.0e-9 pxRange
    correctedPixels =
      if useModernErrorCorrection
        then applyModernErrorCorrection dim dim frame pxRange edgeContours correctionInputPixels
        else applyLegacyErrorCorrection dim dim correctionThreshold correctionInputPixels
    useModernErrorCorrection = True

    samplePixel x y fill =
      let glyphPoint = pixelToGlyph frame dim x y
          samples = accumulateSamples glyphPoint selectorContours
          dA = samples.dA
          dR = samples.dR
          dG = samples.dG
          dB = samples.dB
          scaleToPx d = d * frame.scale
          rRaw = 0.5 + (scaleToPx dR / pxRange)
          gRaw = 0.5 + (scaleToPx dG / pxRange)
          bRaw = 0.5 + (scaleToPx dB / pxRange)
          aRaw = 0.5 + (scaleToPx dA / pxRange)
          med = corrMedian3 rRaw gRaw bRaw
          (rSigned, gSigned, bSigned, matchVal)
            | med == 0.5 = (rRaw, gRaw, bRaw, 0)
            | (med > 0.5) /= fill = (1.0 - rRaw, 1.0 - gRaw, 1.0 - bRaw, -1)
            | otherwise = (rRaw, gRaw, bRaw, 1)
          aSigned
            | (aRaw > 0.5) /= fill = 1.0 - aRaw
            | otherwise = aRaw
       in PixelSample
            { px =
                Pixel
                  { r = rSigned,
                    g = gSigned,
                    b = bSigned,
                    a = aSigned
                  },
              match = matchVal
            }

    encodePixel px =
      [ toWord8 px.r,
        toWord8 px.g,
        toWord8 px.b,
        toWord8 px.a
      ]

contourTriples :: [Edge] -> [(Edge, Edge, Edge)]
contourTriples edges =
  case edges of
    [] -> []
    _ -> zip3 (rotateRight (rotateRight edges)) (rotateRight edges) edges

data ContourSelectorInput = ContourSelectorInput
  { triples :: ![(Edge, Edge, Edge)],
    winding :: !Int
  }
  deriving stock (Eq, Show)

contourSelectorInput :: [Edge] -> ContourSelectorInput
contourSelectorInput edges =
  ContourSelectorInput
    { triples = contourTriples edges,
      winding = contourWinding edges
    }

contourWinding :: [Edge] -> Int
contourWinding edges =
  case edges of
    [] -> 0
    [e0] ->
      let a = edgePoint e0 0.0
          b = edgePoint e0 (1.0 / 3.0)
          c = edgePoint e0 (2.0 / 3.0)
          total = shoelace a b + shoelace b c + shoelace c a
       in signum0 total
    [e0, e1] ->
      let a = edgePoint e0 0.0
          b = edgePoint e0 0.5
          c = edgePoint e1 0.0
          d = edgePoint e1 0.5
          total = shoelace a b + shoelace b c + shoelace c d + shoelace d a
       in signum0 total
    _ ->
      let starts = fmap (`edgePoint` 0.0) edges
          total = sum (zipWith shoelace (rotateRight starts) starts)
       in signum0 total

edgePoint :: Edge -> Double -> Pt
edgePoint edge t =
  case edge.c of
    Nothing ->
      let u = 1.0 - t
       in Pt
            { x = (u * edge.a.x) + (t * edge.b.x),
              y = (u * edge.a.y) + (t * edge.b.y)
            }
    Just ctrl ->
      let u = 1.0 - t
          w0 = u * u
          w1 = 2.0 * u * t
          w2 = t * t
       in Pt
            { x = (w0 * edge.a.x) + (w1 * ctrl.x) + (w2 * edge.b.x),
              y = (w0 * edge.a.y) + (w1 * ctrl.y) + (w2 * edge.b.y)
            }

shoelace :: Pt -> Pt -> Double
shoelace a b = (b.x - a.x) * (a.y + b.y)

signum0 :: Double -> Int
signum0 x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

data Pixel = Pixel
  { r :: !Double,
    g :: !Double,
    b :: !Double,
    a :: !Double
  }
  deriving stock (Eq, Show)

data PixelSample = PixelSample
  { px :: !Pixel,
    match :: !Int
  }
  deriving stock (Eq, Show)

floatLike :: Double -> Double
floatLike x = realToFrac (realToFrac x :: Float)

floatLikePixel :: Pixel -> Pixel
floatLikePixel pixel =
  Pixel
    { r = floatLike pixel.r,
      g = floatLike pixel.g,
      b = floatLike pixel.b,
      a = floatLike pixel.a
    }

corrMedian3 :: Double -> Double -> Double -> Double
corrMedian3 a b c =
  let aF = realToFrac a :: Float
      bF = realToFrac b :: Float
      cF = realToFrac c :: Float
      mF = msMaxF (msMinF aF bF) (msMinF (msMaxF aF bF) cF)
   in realToFrac mF

msMinF :: Float -> Float -> Float
msMinF a b =
  if b < a
    then b
    else a

msMaxF :: Float -> Float -> Float
msMaxF a b =
  if a < b
    then b
    else a

msMinD :: Double -> Double -> Double
msMinD a b =
  if b < a
    then b
    else a

msMaxD :: Double -> Double -> Double
msMaxD a b =
  if a < b
    then b
    else a

cxxMedian3 :: Double -> Double -> Double -> Double
cxxMedian3 x y z = msMaxD (msMinD x y) (msMinD (msMaxD x y) z)

applyAmbiguousSignFix :: Int -> Int -> [PixelSample] -> [Pixel]
applyAmbiguousSignFix w h samples =
  let count = w * h
      sampleMap = IM.fromDistinctAscList (zip [0 .. count - 1] samples)
   in fmap (resolvePixel sampleMap) [0 .. count - 1]
  where
    resolvePixel sampleMap idx =
      let sample = lookupSample sampleMap idx
       in if sample.match /= 0
            then sample.px
            else
              let (x, y) = fromIndex w idx
                  neighborSum =
                    sum
                      [ (lookupSample sampleMap (toIndex w nx ny)).match
                        | (dx, dy) <- neighborOffsetsCardinal,
                          let nx = x + dx,
                          let ny = y + dy,
                          nx >= 0,
                          nx < w,
                          ny >= 0,
                          ny < h
                      ]
               in if neighborSum < 0
                    then flipRgb sample.px
                    else sample.px

fromIndex :: Int -> Int -> (Int, Int)
fromIndex w idx = (idx `mod` w, idx `div` w)

lookupSample :: IM.IntMap PixelSample -> Int -> PixelSample
lookupSample samples idx =
  case IM.lookup idx samples of
    Just sample -> sample
    Nothing ->
      PixelSample
        { px =
            Pixel
              { r = 0.0,
                g = 0.0,
                b = 0.0,
                a = 0.0
              },
          match = 1
        }

flipRgb :: Pixel -> Pixel
flipRgb px =
  px
    { r = 1.0 - px.r,
      g = 1.0 - px.g,
      b = 1.0 - px.b
    }

minDeviationRatioModern :: Double
minDeviationRatioModern = 1.11111111111111111

minImproveRatioDistanceAware :: Double
minImproveRatioDistanceAware = 1.11111111111111111

protectionRadiusTolerance :: Double
protectionRadiusTolerance = 1.001

artifactTEpsilon :: Double
artifactTEpsilon = 0.01

stencilErrorFlag :: Int
stencilErrorFlag = 1

stencilProtectedFlag :: Int
stencilProtectedFlag = 2

classifierFlagCandidate :: Int
classifierFlagCandidate = 1

classifierFlagArtifact :: Int
classifierFlagArtifact = 2

data BaseArtifactClassifier = BaseArtifactClassifier
  { span :: !Double,
    protectedFlag :: !Bool
  }
  deriving stock (Eq, Show)

data DistanceAwareEval = DistanceAwareEval
  { evalW :: !Int,
    evalFrame :: !Frame,
    evalH :: !Int,
    evalPxRange :: !Double,
    evalSelectorContours :: ![ContourSelectorInput],
    evalPixels :: !(IM.IntMap Pixel),
    evalX :: !Int,
    evalY :: !Int,
    evalCenter :: !Pixel,
    evalCenterMedian :: !Double
  }
  deriving stock (Eq, Show)

applyModernErrorCorrection :: Int -> Int -> Frame -> Double -> [[Edge]] -> [Pixel] -> [Pixel]
applyModernErrorCorrection w h frame pxRange edgeContours pixels =
  let count = w * h
      pixelMap = IM.fromDistinctAscList (zip [0 .. count - 1] pixels)
      selectorContours = fmap contourSelectorInput edgeContours
      safePxRange = max 1.0e-9 pxRange
      hvDelta = 1.0 / safePxRange
      dDelta = sqrt 2.0 * hvDelta
      hSpan = minDeviationRatioModern * hvDelta
      vSpan = minDeviationRatioModern * hvDelta
      dSpan = minDeviationRatioModern * dDelta
      hRadius = protectionRadiusTolerance * hvDelta
      vRadius = protectionRadiusTolerance * hvDelta
      dRadius = protectionRadiusTolerance * dDelta
      stencil0 = IM.empty
      stencil1 = protectCornersModern w h frame edgeContours stencil0
      stencil2 = protectEdgesModern w h hRadius vRadius dRadius pixelMap stencil1
      stencil3 = findErrorsModern w h hSpan vSpan dSpan pixelMap stencil2
      stencil4 =
        if useDistanceAwareSecondPass
          then
            let stencilProtected = protectAllTexels w h stencil3
             in findErrorsModernDistanceAware w h frame safePxRange selectorContours hSpan vSpan dSpan pixelMap stencilProtected
          else stencil3
      useDistanceAwareSecondPass = False
   in fmap (applyStencilCorrection stencil4 pixelMap) [0 .. count - 1]

applyStencilCorrection :: IM.IntMap Int -> IM.IntMap Pixel -> Int -> Pixel
applyStencilCorrection stencil pixels idx =
  let px = lookupPixel pixels idx
   in if hasStencilFlag stencilErrorFlag (lookupStencilValue stencil idx)
        then equalizePixel px
        else px

lookupStencilValue :: IM.IntMap Int -> Int -> Int
lookupStencilValue stencil idx = IM.findWithDefault 0 idx stencil

setStencilFlag :: Int -> Int -> IM.IntMap Int -> IM.IntMap Int
setStencilFlag flag idx stencil =
  IM.insert idx (lookupStencilValue stencil idx .|. flag) stencil

hasStencilFlag :: Int -> Int -> Bool
hasStencilFlag flag value = (value .&. flag) /= 0

protectAllTexels :: Int -> Int -> IM.IntMap Int -> IM.IntMap Int
protectAllTexels w h stencil0 =
  foldl'
    (\stencil idx -> setStencilFlag stencilProtectedFlag idx stencil)
    stencil0
    [0 .. (w * h) - 1]

projectGlyphToTexel :: Frame -> Int -> Pt -> Pt
projectGlyphToTexel frame h point =
  Pt
    { x = (point.x + frame.tx) * frame.scale,
      y = fromIntegral h - ((point.y + frame.ty) * frame.scale)
    }

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds w h x y = x >= 0 && x < w && y >= 0 && y < h

protectCornersModern :: Int -> Int -> Frame -> [[Edge]] -> IM.IntMap Int -> IM.IntMap Int
protectCornersModern w h frame edgeContours stencil0 =
  foldl' protectContour stencil0 edgeContours
  where
    protectContour stencil edges =
      case edges of
        [] -> stencil
        _ -> foldl' protectCorner stencil (zip (rotateRight edges) edges)

    protectCorner stencil (prevEdge, edge) =
      let commonColor = fromIntegral (prevEdge.col .&. edge.col) :: Int
       in if (commonColor .&. (commonColor - 1)) == 0
            then
              let corner = projectGlyphToTexel frame h edge.a
                  l = floor (corner.x - 0.5)
                  b = floor (corner.y - 0.5)
                  marked =
                    [ (l, b),
                      (l + 1, b),
                      (l, b + 1),
                      (l + 1, b + 1)
                    ]
               in foldl' markProtected stencil marked
            else stencil

    markProtected stencil (x, y)
      | inBounds w h x y = setStencilFlag stencilProtectedFlag (toIndex w x y) stencil
      | otherwise = stencil

protectEdgesModern ::
  Int ->
  Int ->
  Double ->
  Double ->
  Double ->
  IM.IntMap Pixel ->
  IM.IntMap Int ->
  IM.IntMap Int
protectEdgesModern w h hRadius vRadius dRadius pixels stencil0 =
  let stencilH =
        foldl'
          protectHorizontal
          stencil0
          [ (x, y)
            | y <- [0 .. h - 1],
              x <- [0 .. w - 2]
          ]
      stencilV =
        foldl'
          protectVertical
          stencilH
          [ (x, y)
            | y <- [0 .. h - 2],
              x <- [0 .. w - 1]
          ]
   in foldl'
        protectDiagonal
        stencilV
        [ (x, y)
          | y <- [0 .. h - 2],
            x <- [0 .. w - 2]
        ]
  where
    protectHorizontal stencil (x, y) =
      let left = lookupPixel pixels (toIndex w x y)
          right = lookupPixel pixels (toIndex w (x + 1) y)
          lm = corrMedian3 left.r left.g left.b
          rm = corrMedian3 right.r right.g right.b
       in if abs (lm - 0.5) + abs (rm - 0.5) < hRadius
            then
              let mask = edgeBetweenTexels left right
                  stencil1 = protectExtremeChannelsAt w x y left lm mask stencil
               in protectExtremeChannelsAt w (x + 1) y right rm mask stencil1
            else stencil

    protectVertical stencil (x, y) =
      let bottom = lookupPixel pixels (toIndex w x y)
          top = lookupPixel pixels (toIndex w x (y + 1))
          bm = corrMedian3 bottom.r bottom.g bottom.b
          tm = corrMedian3 top.r top.g top.b
       in if abs (bm - 0.5) + abs (tm - 0.5) < vRadius
            then
              let mask = edgeBetweenTexels bottom top
                  stencil1 = protectExtremeChannelsAt w x y bottom bm mask stencil
               in protectExtremeChannelsAt w x (y + 1) top tm mask stencil1
            else stencil

    protectDiagonal stencil (x, y) =
      let lb = lookupPixel pixels (toIndex w x y)
          rb = lookupPixel pixels (toIndex w (x + 1) y)
          lt = lookupPixel pixels (toIndex w x (y + 1))
          rt = lookupPixel pixels (toIndex w (x + 1) (y + 1))
          mlb = corrMedian3 lb.r lb.g lb.b
          mrb = corrMedian3 rb.r rb.g rb.b
          mlt = corrMedian3 lt.r lt.g lt.b
          mrt = corrMedian3 rt.r rt.g rt.b
          stencil1 =
            if abs (mlb - 0.5) + abs (mrt - 0.5) < dRadius
              then
                let mask = edgeBetweenTexels lb rt
                    stencilA = protectExtremeChannelsAt w x y lb mlb mask stencil
                 in protectExtremeChannelsAt w (x + 1) (y + 1) rt mrt mask stencilA
              else stencil
       in if abs (mrb - 0.5) + abs (mlt - 0.5) < dRadius
            then
              let mask = edgeBetweenTexels rb lt
                  stencilA = protectExtremeChannelsAt w (x + 1) y rb mrb mask stencil1
               in protectExtremeChannelsAt w x (y + 1) lt mlt mask stencilA
            else stencil1

protectExtremeChannelsAt ::
  Int ->
  Int ->
  Int ->
  Pixel ->
  Double ->
  Int ->
  IM.IntMap Int ->
  IM.IntMap Int
protectExtremeChannelsAt w x y pixel med mask stencil =
  if
      (mask .&. 1 /= 0 && pixel.r /= med)
        || (mask .&. 2 /= 0 && pixel.g /= med)
        || (mask .&. 4 /= 0 && pixel.b /= med)
    then setStencilFlag stencilProtectedFlag (toIndex w x y) stencil
    else stencil

edgeBetweenTexels :: Pixel -> Pixel -> Int
edgeBetweenTexels a b =
  (if edgeBetweenTexelsChannel a b 0 then 1 else 0)
    .|. (if edgeBetweenTexelsChannel a b 1 then 2 else 0)
    .|. (if edgeBetweenTexelsChannel a b 2 then 4 else 0)

edgeBetweenTexelsChannel :: Pixel -> Pixel -> Int -> Bool
edgeBetweenTexelsChannel a b channel =
  let aCh = channelValue channel a
      bCh = channelValue channel b
      denom = aCh - bCh
   in if denom == 0.0
        then False
        else
          let t = (aCh - 0.5) / denom
           in if t > 0.0 && t < 1.0
                then
                  let cR = mixValue a.r b.r t
                      cG = mixValue a.g b.g t
                      cB = mixValue a.b b.b t
                      cM = corrMedian3 cR cG cB
                      cCh =
                        case channel of
                          0 -> cR
                          1 -> cG
                          _ -> cB
                   in cM == cCh
                else False

channelValue :: Int -> Pixel -> Double
channelValue channel pixel =
  case channel of
    0 -> pixel.r
    1 -> pixel.g
    _ -> pixel.b

mixValue :: Double -> Double -> Double -> Double
mixValue a b t = floatLike (((1.0 - t) * a) + (t * b))

findErrorsModern ::
  Int ->
  Int ->
  Double ->
  Double ->
  Double ->
  IM.IntMap Pixel ->
  IM.IntMap Int ->
  IM.IntMap Int
findErrorsModern w h hSpan vSpan dSpan pixels stencil0 =
  foldl' markErrorAt stencil0 [0 .. count - 1]
  where
    count = w * h

    markErrorAt stencil idx =
      let (x, y) = fromIndex w idx
          center = lookupPixel pixels idx
          centerMedian = corrMedian3 center.r center.g center.b
          protected = hasStencilFlag stencilProtectedFlag (lookupStencilValue stencil idx)
          left = lookupPixel pixels (toIndex w (x - 1) y)
          right = lookupPixel pixels (toIndex w (x + 1) y)
          up = lookupPixel pixels (toIndex w x (y - 1))
          down = lookupPixel pixels (toIndex w x (y + 1))
          leftUp = lookupPixel pixels (toIndex w (x - 1) (y - 1))
          rightUp = lookupPixel pixels (toIndex w (x + 1) (y - 1))
          leftDown = lookupPixel pixels (toIndex w (x - 1) (y + 1))
          rightDown = lookupPixel pixels (toIndex w (x + 1) (y + 1))
          eLeft = x > 0 && hasLinearArtifact (BaseArtifactClassifier hSpan protected) centerMedian center left
          eUp = y > 0 && hasLinearArtifact (BaseArtifactClassifier vSpan protected) centerMedian center up
          eRight = x < w - 1 && hasLinearArtifact (BaseArtifactClassifier hSpan protected) centerMedian center right
          eDown = y < h - 1 && hasLinearArtifact (BaseArtifactClassifier vSpan protected) centerMedian center down
          eLeftUp = x > 0 && y > 0 && hasDiagonalArtifact (BaseArtifactClassifier dSpan protected) centerMedian center left up leftUp
          eRightUp = x < w - 1 && y > 0 && hasDiagonalArtifact (BaseArtifactClassifier dSpan protected) centerMedian center right up rightUp
          eLeftDown = x > 0 && y < h - 1 && hasDiagonalArtifact (BaseArtifactClassifier dSpan protected) centerMedian center left down leftDown
          eRightDown = x < w - 1 && y < h - 1 && hasDiagonalArtifact (BaseArtifactClassifier dSpan protected) centerMedian center right down rightDown
          hasError = eLeft || eUp || eRight || eDown || eLeftUp || eRightUp || eLeftDown || eRightDown
       in if hasError
            then setStencilFlag stencilErrorFlag idx stencil
            else stencil

findErrorsModernDistanceAware ::
  Int ->
  Int ->
  Frame ->
  Double ->
  [ContourSelectorInput] ->
  Double ->
  Double ->
  Double ->
  IM.IntMap Pixel ->
  IM.IntMap Int ->
  IM.IntMap Int
findErrorsModernDistanceAware w h frame safePxRange selectorContours hSpan vSpan dSpan pixels stencil0 =
  foldl' markErrorAt stencil0 [0 .. count - 1]
  where
    count = w * h

    markErrorAt stencil idx
      | hasStencilFlag stencilErrorFlag (lookupStencilValue stencil idx) = stencil
      | otherwise =
          let (x, y) = fromIndex w idx
              center = lookupPixel pixels idx
              centerMedian = corrMedian3 center.r center.g center.b
              protected = hasStencilFlag stencilProtectedFlag (lookupStencilValue stencil idx)
              eval =
                DistanceAwareEval
                  { evalW = w,
                    evalFrame = frame,
                    evalH = h,
                    evalPxRange = safePxRange,
                    evalSelectorContours = selectorContours,
                    evalPixels = pixels,
                    evalX = x,
                    evalY = y,
                    evalCenter = center,
                    evalCenterMedian = centerMedian
                  }
              left = lookupPixel pixels (toIndex w (x - 1) y)
              right = lookupPixel pixels (toIndex w (x + 1) y)
              up = lookupPixel pixels (toIndex w x (y - 1))
              down = lookupPixel pixels (toIndex w x (y + 1))
              hasError =
                (x > 0 && hasLinearArtifactDistanceAware (BaseArtifactClassifier hSpan protected) eval left (-1.0) 0.0)
                  || (y > 0 && hasLinearArtifactDistanceAware (BaseArtifactClassifier vSpan protected) eval up 0.0 (-1.0))
                  || (x < w - 1 && hasLinearArtifactDistanceAware (BaseArtifactClassifier hSpan protected) eval right 1.0 0.0)
                  || (y < h - 1 && hasLinearArtifactDistanceAware (BaseArtifactClassifier vSpan protected) eval down 0.0 1.0)
                  || (x > 0 && y > 0 && hasDiagonalArtifactDistanceAware (BaseArtifactClassifier dSpan protected) eval left up (lookupPixel pixels (toIndex w (x - 1) (y - 1))) (-1.0) (-1.0))
                  || (x < w - 1 && y > 0 && hasDiagonalArtifactDistanceAware (BaseArtifactClassifier dSpan protected) eval right up (lookupPixel pixels (toIndex w (x + 1) (y - 1))) 1.0 (-1.0))
                  || (x > 0 && y < h - 1 && hasDiagonalArtifactDistanceAware (BaseArtifactClassifier dSpan protected) eval left down (lookupPixel pixels (toIndex w (x - 1) (y + 1))) (-1.0) 1.0)
                  || (x < w - 1 && y < h - 1 && hasDiagonalArtifactDistanceAware (BaseArtifactClassifier dSpan protected) eval right down (lookupPixel pixels (toIndex w (x + 1) (y + 1))) 1.0 1.0)
           in if hasError
                then setStencilFlag stencilErrorFlag idx stencil
                else stencil

rangeTest :: BaseArtifactClassifier -> Double -> Double -> Double -> Double -> Double -> Double -> Int
rangeTest classifier at bt xt am bm xm =
  let amF = floatLike am
      bmF = floatLike bm
      xmF = floatLike xm
      inversionArtifact =
        (amF > 0.5 && bmF > 0.5 && xmF <= 0.5)
          || (amF < 0.5 && bmF < 0.5 && xmF >= 0.5)
      outsideMedian = corrMedian3 amF bmF xmF /= xmF
   in if inversionArtifact || (not classifier.protectedFlag && outsideMedian)
        then
          let axSpan = (xt - at) * classifier.span
              bxSpan = (bt - xt) * classifier.span
              withinRange =
                xmF >= amF - axSpan
                  && xmF <= amF + axSpan
                  && xmF >= bmF - bxSpan
                  && xmF <= bmF + bxSpan
           in if withinRange
                then classifierFlagCandidate
                else classifierFlagCandidate .|. classifierFlagArtifact
        else 0

classifierEvaluatesArtifact :: BaseArtifactClassifier -> Double -> Double -> Int -> Bool
classifierEvaluatesArtifact _classifier _t _m flags = (flags .&. classifierFlagArtifact) /= 0

interpolatedRgbLinear :: Pixel -> Pixel -> Double -> (Double, Double, Double)
interpolatedRgbLinear a b t =
  ( mixValue a.r b.r t,
    mixValue a.g b.g t,
    mixValue a.b b.b t
  )

interpolatedMedianLinear :: Pixel -> Pixel -> Double -> Double
interpolatedMedianLinear a b t =
  let (rCh, gCh, bCh) = interpolatedRgbLinear a b t
   in corrMedian3 rCh gCh bCh

interpolatedRgbBilinear :: Pixel -> (Double, Double, Double) -> (Double, Double, Double) -> Double -> (Double, Double, Double)
interpolatedRgbBilinear a linearTerms quadraticTerms t =
  let (lR, lG, lB) = linearTerms
      (qR, qG, qB) = quadraticTerms
   in ( t * ((t * qR) + lR) + a.r,
        t * ((t * qG) + lG) + a.g,
        t * ((t * qB) + lB) + a.b
      )

interpolatedMedianBilinear :: Pixel -> (Double, Double, Double) -> (Double, Double, Double) -> Double -> Double
interpolatedMedianBilinear a linearTerms quadraticTerms t =
  let (rCh, gCh, bCh) = interpolatedRgbBilinear a linearTerms quadraticTerms t
   in floatLike (cxxMedian3 rCh gCh bCh)

hasLinearArtifactInner ::
  BaseArtifactClassifier ->
  Double ->
  Double ->
  Pixel ->
  Pixel ->
  Double ->
  Double ->
  Bool
hasLinearArtifactInner classifier am bm a b dA dB =
  let denom = dA - dB
   in if denom == 0.0
        then False
        else
          let t = dA / denom
           in if t > artifactTEpsilon && t < 1.0 - artifactTEpsilon
                then
                  let xm = interpolatedMedianLinear a b t
                   in classifierEvaluatesArtifact classifier t xm (rangeTest classifier 0.0 1.0 t am bm xm)
                else False

hasLinearArtifact :: BaseArtifactClassifier -> Double -> Pixel -> Pixel -> Bool
hasLinearArtifact classifier am a b =
  let bm = corrMedian3 b.r b.g b.b
   in abs (am - 0.5) >= abs (bm - 0.5)
        && ( hasLinearArtifactInner classifier am bm a b (a.g - a.r) (b.g - b.r)
               || hasLinearArtifactInner classifier am bm a b (a.b - a.g) (b.b - b.g)
               || hasLinearArtifactInner classifier am bm a b (a.r - a.b) (b.r - b.b)
           )

hasDiagonalArtifactInner ::
  BaseArtifactClassifier ->
  Double ->
  Double ->
  Pixel ->
  (Double, Double, Double) ->
  (Double, Double, Double) ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Bool
hasDiagonalArtifactInner classifier am dm a linearTerms quadraticTerms dA dBC dD tEx0 tEx1 =
  let quadA = dD - dBC + dA
      quadB = dBC - dA - dA
   in any checkRoot (solveQuadraticGeneral quadA quadB dA)
  where
    checkRoot t
      | t > artifactTEpsilon && t < 1.0 - artifactTEpsilon =
          let xm = interpolatedMedianBilinear a linearTerms quadraticTerms t
              rangeFlags0 = rangeTest classifier 0.0 1.0 t am dm xm
              rangeFlags1 = rangeWithLocalExtreme rangeFlags0 xm tEx0 t
              rangeFlags2 = rangeWithLocalExtreme rangeFlags1 xm tEx1 t
           in classifierEvaluatesArtifact classifier t xm rangeFlags2
      | otherwise = False

    rangeWithLocalExtreme rangeFlags xm tEx t
      | tEx > 0.0 && tEx < 1.0 =
          let emEx = interpolatedMedianBilinear a linearTerms quadraticTerms tEx
              (t0, t1, em0, em1) =
                if tEx > t
                  then (0.0, tEx, am, emEx)
                  else (tEx, 1.0, emEx, dm)
           in rangeFlags .|. rangeTest classifier t0 t1 t em0 em1 xm
      | otherwise = rangeFlags

hasDiagonalArtifact :: BaseArtifactClassifier -> Double -> Pixel -> Pixel -> Pixel -> Pixel -> Bool
hasDiagonalArtifact classifier am a b c d =
  let dm = corrMedian3 d.r d.g d.b
   in if abs (am - 0.5) >= abs (dm - 0.5)
        then
          let abcR = a.r - b.r - c.r
              abcG = a.g - b.g - c.g
              abcB = a.b - b.b - c.b
              lR = negate a.r - abcR
              lG = negate a.g - abcG
              lB = negate a.b - abcB
              qR = d.r + abcR
              qG = d.g + abcG
              qB = d.b + abcB
              linearTerms = (lR, lG, lB)
              quadraticTerms = (qR, qG, qB)
              tExR = ((-0.5) * lR) / qR
              tExG = ((-0.5) * lG) / qG
              tExB = ((-0.5) * lB) / qB
              dA_rg = a.g - a.r
              dBC_rg = b.g - b.r + c.g - c.r
              dD_rg = d.g - d.r
              dA_bg = a.b - a.g
              dBC_bg = b.b - b.g + c.b - c.g
              dD_bg = d.b - d.g
              dA_br = a.r - a.b
              dBC_br = b.r - b.b + c.r - c.b
              dD_br = d.r - d.b
           in hasDiagonalArtifactInner classifier am dm a linearTerms quadraticTerms dA_rg dBC_rg dD_rg tExR tExG
                || hasDiagonalArtifactInner classifier am dm a linearTerms quadraticTerms dA_bg dBC_bg dD_bg tExG tExB
                || hasDiagonalArtifactInner classifier am dm a linearTerms quadraticTerms dA_br dBC_br dD_br tExB tExR
        else False

evaluateDistanceAwareCandidate ::
  DistanceAwareEval ->
  Double ->
  Double ->
  Int ->
  Bool
evaluateDistanceAwareCandidate eval tx ty rangeFlags
  | (rangeFlags .&. classifierFlagArtifact) /= 0 = True
  | (rangeFlags .&. classifierFlagCandidate) == 0 = False
  | otherwise =
      let (oldR, oldG, oldB) = bilinearSampleRgb eval tx ty
          oldMedian = corrMedian3 oldR oldG oldB
          aWeight = (1.0 - abs tx) * (1.0 - abs ty)
          newR = oldR + ((eval.evalCenterMedian - eval.evalCenter.r) * aWeight)
          newG = oldG + ((eval.evalCenterMedian - eval.evalCenter.g) * aWeight)
          newB = oldB + ((eval.evalCenterMedian - eval.evalCenter.b) * aWeight)
          newMedian = corrMedian3 newR newG newB
          refMedian = exactDistanceReferenceMedian eval tx ty
       in minImproveRatioDistanceAware * abs (newMedian - refMedian) < abs (oldMedian - refMedian)

bilinearSampleRgb :: DistanceAwareEval -> Double -> Double -> (Double, Double, Double)
bilinearSampleRgb eval tx ty =
  let ax = abs tx
      ay = abs ty
      xStep = if tx < 0.0 then -1 else 1
      yStep = if ty < 0.0 then -1 else 1
      center = eval.evalCenter
      x = eval.evalX
      y = eval.evalY
      w = eval.evalW
      pixels = eval.evalPixels
      edgeX = lookupPixel pixels (toIndex w (x + xStep) y)
      edgeY = lookupPixel pixels (toIndex w x (y + yStep))
      diag = lookupPixel pixels (toIndex w (x + xStep) (y + yStep))
      wA = (1.0 - ax) * (1.0 - ay)
      wB = ax * (1.0 - ay)
      wC = (1.0 - ax) * ay
      wD = ax * ay
   in ( (wA * center.r) + (wB * edgeX.r) + (wC * edgeY.r) + (wD * diag.r),
        (wA * center.g) + (wB * edgeX.g) + (wC * edgeY.g) + (wD * diag.g),
        (wA * center.b) + (wB * edgeX.b) + (wC * edgeY.b) + (wD * diag.b)
      )

exactDistanceReferenceMedian :: DistanceAwareEval -> Double -> Double -> Double
exactDistanceReferenceMedian eval tx ty =
  let centerX = pixelCenterGlyphX eval.evalFrame eval.evalX
      centerY = pixelCenterGlyphY eval.evalFrame eval.evalH eval.evalY
      point =
        Pt
          { x = centerX + (tx / eval.evalFrame.scale),
            y = centerY - (ty / eval.evalFrame.scale)
          }
      contourDistances = fmap (distanceForContour point) eval.evalSelectorContours
      shapeSet = foldl' (\acc contourDistance -> mergeSelectorSet acc contourDistance.set) initialSelectorSet contourDistances
      shapeDistance = distanceFromSet point shapeSet
   in floatLike (0.5 + ((shapeDistance.a * eval.evalFrame.scale) / eval.evalPxRange))

hasLinearArtifactInnerDistanceAware ::
  BaseArtifactClassifier ->
  DistanceAwareEval ->
  Double ->
  Pixel ->
  Double ->
  Double ->
  Double ->
  Double ->
  Bool
hasLinearArtifactInnerDistanceAware classifier eval bm b txStep tyStep dA dB =
  let denom = dA - dB
      a = eval.evalCenter
      am = eval.evalCenterMedian
   in if denom == 0.0
        then False
        else
          let t = dA / denom
           in if t > artifactTEpsilon && t < 1.0 - artifactTEpsilon
                then
                  let (oldR, oldG, oldB) = interpolatedRgbLinear a b t
                      xm = median3 oldR oldG oldB
                      rangeFlags = rangeTest classifier 0.0 1.0 t am bm xm
                   in evaluateDistanceAwareCandidate eval (txStep * t) (tyStep * t) rangeFlags
                else False

hasLinearArtifactDistanceAware ::
  BaseArtifactClassifier ->
  DistanceAwareEval ->
  Pixel ->
  Double ->
  Double ->
  Bool
hasLinearArtifactDistanceAware classifier eval b txStep tyStep =
  let a = eval.evalCenter
      am = eval.evalCenterMedian
      bm = corrMedian3 b.r b.g b.b
   in abs (am - 0.5) >= abs (bm - 0.5)
        && ( hasLinearArtifactInnerDistanceAware classifier eval bm b txStep tyStep (a.g - a.r) (b.g - b.r)
               || hasLinearArtifactInnerDistanceAware classifier eval bm b txStep tyStep (a.b - a.g) (b.b - b.g)
               || hasLinearArtifactInnerDistanceAware classifier eval bm b txStep tyStep (a.r - a.b) (b.r - b.b)
           )

hasDiagonalArtifactInnerDistanceAware ::
  BaseArtifactClassifier ->
  DistanceAwareEval ->
  Double ->
  (Double, Double, Double) ->
  (Double, Double, Double) ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Bool
hasDiagonalArtifactInnerDistanceAware classifier eval dm linearTerms quadraticTerms dA dBC dD tEx0 tEx1 txStep tyStep =
  let quadA = dD - dBC + dA
      quadB = dBC - dA - dA
   in any checkRoot (solveQuadraticGeneral quadA quadB dA)
  where
    a = eval.evalCenter
    am = eval.evalCenterMedian

    checkRoot t
              | t > artifactTEpsilon && t < 1.0 - artifactTEpsilon =
          let (oldR, oldG, oldB) = interpolatedRgbBilinear a linearTerms quadraticTerms t
              xm = floatLike (cxxMedian3 oldR oldG oldB)
              rangeFlags0 = rangeTest classifier 0.0 1.0 t am dm xm
              rangeFlags1 = rangeWithLocalExtreme rangeFlags0 xm tEx0 t
              rangeFlags2 = rangeWithLocalExtreme rangeFlags1 xm tEx1 t
           in evaluateDistanceAwareCandidate eval (txStep * t) (tyStep * t) rangeFlags2
      | otherwise = False

    rangeWithLocalExtreme rangeFlags xm tEx t
      | tEx > 0.0 && tEx < 1.0 =
          let emEx = interpolatedMedianBilinear a linearTerms quadraticTerms tEx
              (t0, t1, em0, em1) =
                if tEx > t
                  then (0.0, tEx, am, emEx)
                  else (tEx, 1.0, emEx, dm)
           in rangeFlags .|. rangeTest classifier t0 t1 t em0 em1 xm
      | otherwise = rangeFlags

hasDiagonalArtifactDistanceAware ::
  BaseArtifactClassifier ->
  DistanceAwareEval ->
  Pixel ->
  Pixel ->
  Pixel ->
  Double ->
  Double ->
  Bool
hasDiagonalArtifactDistanceAware classifier eval b c d txStep tyStep =
  let a = eval.evalCenter
      am = eval.evalCenterMedian
      dm = corrMedian3 d.r d.g d.b
   in if abs (am - 0.5) >= abs (dm - 0.5)
         then
           let abcR = a.r - b.r - c.r
               abcG = a.g - b.g - c.g
               abcB = a.b - b.b - c.b
               lR = negate a.r - abcR
               lG = negate a.g - abcG
               lB = negate a.b - abcB
               qR = d.r + abcR
               qG = d.g + abcG
               qB = d.b + abcB
               linearTerms = (lR, lG, lB)
               quadraticTerms = (qR, qG, qB)
               tExR = ((-0.5) * lR) / qR
               tExG = ((-0.5) * lG) / qG
               tExB = ((-0.5) * lB) / qB
               dA_rg = a.g - a.r
               dBC_rg = b.g - b.r + c.g - c.r
               dD_rg = d.g - d.r
               dA_bg = a.b - a.g
               dBC_bg = b.b - b.g + c.b - c.g
               dD_bg = d.b - d.g
               dA_br = a.r - a.b
               dBC_br = b.r - b.b + c.r - c.b
               dD_br = d.r - d.b
            in hasDiagonalArtifactInnerDistanceAware classifier eval dm linearTerms quadraticTerms dA_rg dBC_rg dD_rg tExR tExG txStep tyStep
                 || hasDiagonalArtifactInnerDistanceAware classifier eval dm linearTerms quadraticTerms dA_bg dBC_bg dD_bg tExG tExB txStep tyStep
                 || hasDiagonalArtifactInnerDistanceAware classifier eval dm linearTerms quadraticTerms dA_br dBC_br dD_br tExB tExR txStep tyStep
        else False

applyLegacyErrorCorrection :: Int -> Int -> Double -> [Pixel] -> [Pixel]
applyLegacyErrorCorrection w h pixelsThreshold pixels =
  let count = w * h
      arr0 = IM.fromDistinctAscList (zip [0 .. count - 1] pixels)
      arr1 = applyCorrectionPass w h pixelsThreshold neighborOffsetsCardinal arr0
      arr2 = applyCorrectionPass w h (pixelsThreshold * 2.0) neighborOffsetsDiagonal arr1
   in fmap (lookupPixel arr2) [0 .. count - 1]

applyCorrectionPass :: Int -> Int -> Double -> [(Int, Int)] -> IM.IntMap Pixel -> IM.IntMap Pixel
applyCorrectionPass w h threshold offsets arr =
  let clashIndices =
        [ idx
          | y <- [0 .. h - 1],
            x <- [0 .. w - 1],
            let idx = toIndex w x y,
            any (isClashAt idx x y) offsets
        ]
   in foldl' (\m idx -> IM.adjust equalizePixel idx m) arr clashIndices
  where
    isClashAt idx x y (dx, dy) =
      let nx = x + dx
          ny = y + dy
       in if nx >= 0 && nx < w && ny >= 0 && ny < h
            then
              let nIdx = toIndex w nx ny
               in detectClash (lookupPixel arr idx) (lookupPixel arr nIdx) threshold
            else False

neighborOffsetsCardinal :: [(Int, Int)]
neighborOffsetsCardinal = [(-1, 0), (1, 0), (0, -1), (0, 1)]

neighborOffsetsDiagonal :: [(Int, Int)]
neighborOffsetsDiagonal = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

toIndex :: Int -> Int -> Int -> Int
toIndex w x y = (y * w) + x

lookupPixel :: IM.IntMap Pixel -> Int -> Pixel
lookupPixel pixels idx =
  case IM.lookup idx pixels of
    Just px -> px
    Nothing ->
      Pixel
        { r = 0.0,
          g = 0.0,
          b = 0.0,
          a = 0.0
        }

equalizePixel :: Pixel -> Pixel
equalizePixel px =
  let med = corrMedian3 px.r px.g px.b
   in px {r = med, g = med, b = med}

median3 :: Double -> Double -> Double -> Double
median3 x y z = max (min x y) (min (max x y) z)

detectClash :: Pixel -> Pixel -> Double -> Bool
detectClash a b threshold =
  case sortedPairs of
    [p0, p1, p2] ->
      let (_a0, b0) = p0
          (a1, b1) = p1
          (a2, b2) = p2
       in abs (b1 - a1) >= threshold
            && not (b0 == b1 && b0 == b2)
            && abs (a2 - 0.5) >= abs (b2 - 0.5)
    _ -> False
  where
    sortedPairs =
      fmap snd $
        sortBy (flip (comparing fst))
          [ (abs (b.r - a.r), (a.r, b.r)),
            (abs (b.g - a.g), (a.g, b.g)),
            (abs (b.b - a.b), (a.b, b.b))
          ]

pixelToGlyph :: Frame -> Int -> Int -> Int -> Pt
pixelToGlyph frame dim x y =
  Pt
    { x = pixelCenterGlyphX frame x,
      y = pixelCenterGlyphY frame dim y
    }

pixelCenterGlyphX :: Frame -> Int -> Double
pixelCenterGlyphX frame x = ((fromIntegral x + 0.5) / frame.scale) - frame.tx

pixelCenterGlyphY :: Frame -> Int -> Int -> Double
pixelCenterGlyphY frame dim y =
  ((fromIntegral dim - (fromIntegral y + 0.5)) / frame.scale) - frame.ty

data ScanlineIntersection = ScanlineIntersection
  { x :: !Double,
    direction :: !Int
  }
  deriving stock (Eq, Show)

data QuadScanState = QuadScanState
  { total :: !Int,
    nextDY :: !Int,
    x0 :: !Double,
    x1 :: !Double,
    dy0 :: !Int,
    dy1 :: !Int
  }
  deriving stock (Eq, Show)

scanlineRowFill :: Int -> Frame -> [Edge] -> Int -> [Bool]
scanlineRowFill dim frame edges y =
  let yLine = pixelCenterGlyphY frame dim y
      intersections = preprocessScanlineIntersections (scanlineIntersectionsAt edges yLine)
   in fillRowFromIntersections dim frame intersections

scanlineIntersectionsAt :: [Edge] -> Double -> [ScanlineIntersection]
scanlineIntersectionsAt edges yLine =
  concatMap (\edge -> scanlineIntersectionsEdge edge yLine) edges

scanlineIntersectionsEdge :: Edge -> Double -> [ScanlineIntersection]
scanlineIntersectionsEdge edge yLine =
  case edge.c of
    Nothing -> linearScanlineIntersections edge yLine
    Just ctrl -> quadraticScanlineIntersections edge ctrl yLine

linearScanlineIntersections :: Edge -> Double -> [ScanlineIntersection]
linearScanlineIntersections edge yLine
  | (yLine >= y0 && yLine < y1) || (yLine >= y1 && yLine < y0) =
      let param = (yLine - y0) / (y1 - y0)
          sampleX = edge.a.x + ((edge.b.x - edge.a.x) * param)
       in [ScanlineIntersection {x = sampleX, direction = signum0 (y1 - y0)}]
  | otherwise = []
  where
    y0 = edge.a.y
    y1 = edge.b.y

quadraticScanlineIntersections :: Edge -> Pt -> Double -> [ScanlineIntersection]
quadraticScanlineIntersections edge ctrl yLine =
  let p0 = edge.a
      p2 = edge.b
      st0 =
        QuadScanState
          { total = 0,
            nextDY = if yLine > p0.y then 1 else -1,
            x0 = p0.x,
            x1 = 0.0,
            dy0 = 0,
            dy1 = 0
          }
      st1
        | p0.y == yLine =
            if p0.y < ctrl.y || (p0.y == ctrl.y && p0.y < p2.y)
              then quadPushCurrent st0 1
              else st0 {nextDY = 1}
        | otherwise = st0
      ab = diffPt ctrl p0
      br = diffPt (diffPt p2 ctrl) ab
      roots = sortQuadraticRoots (solveQuadraticScanline br.y (2.0 * ab.y) (p0.y - yLine))
      st2 = foldl' (processQuadraticRoot p0 ab br) st1 roots
      st3
        | p2.y == yLine =
            let stA =
                  if st2.nextDY > 0 && st2.total > 0
                    then (quadPopLast st2) {nextDY = -1}
                    else st2
                stB =
                  if (p2.y < ctrl.y || (p2.y == ctrl.y && p2.y < p0.y)) && stA.total < 2
                    then
                      let stAX = quadSetX stA p2.x
                       in if stAX.nextDY < 0
                            then (quadPushCurrent stAX (-1)) {nextDY = 1}
                            else stAX
                    else stA
             in stB
        | otherwise = st2
      expectedNext = if yLine >= p2.y then 1 else -1
      st4
        | st3.nextDY /= expectedNext =
            if st3.total > 0
              then quadPopLast st3
              else
                let stX =
                      if abs (p2.y - yLine) < abs (p0.y - yLine)
                        then quadSetX st3 p2.x
                        else st3
                 in quadPushCurrent stX stX.nextDY
        | otherwise = st3
   in quadToIntersections st4

processQuadraticRoot :: Pt -> Pt -> Pt -> QuadScanState -> Double -> QuadScanState
processQuadraticRoot p0 ab br st t
  | st.total >= 2 = st
  | t < 0.0 || t > 1.0 = st
  | otherwise =
      let sampleX = p0.x + (2.0 * t * ab.x) + (t * t * br.x)
          stX = quadSetX st sampleX
          tangentY = ab.y + (t * br.y)
       in if (fromIntegral stX.nextDY * tangentY) >= 0.0
            then (quadPushCurrent stX stX.nextDY) {nextDY = negate stX.nextDY}
            else stX

quadSetX :: QuadScanState -> Double -> QuadScanState
quadSetX st sampleX
  | st.total == 0 = st {x0 = sampleX}
  | otherwise = st {x1 = sampleX}

quadPushCurrent :: QuadScanState -> Int -> QuadScanState
quadPushCurrent st dyVal =
  case st.total of
    0 -> st {total = 1, dy0 = dyVal}
    1 -> st {total = 2, dy1 = dyVal}
    _ -> st

quadPopLast :: QuadScanState -> QuadScanState
quadPopLast st
  | st.total > 0 = st {total = st.total - 1}
  | otherwise = st

quadToIntersections :: QuadScanState -> [ScanlineIntersection]
quadToIntersections st =
  case st.total of
    0 -> []
    1 ->
      [ ScanlineIntersection
          { x = st.x0,
            direction = st.dy0
          }
      ]
    _ ->
      [ ScanlineIntersection
          { x = st.x0,
            direction = st.dy0
          },
        ScanlineIntersection
          { x = st.x1,
            direction = st.dy1
          }
      ]

sortQuadraticRoots :: [Double] -> [Double]
sortQuadraticRoots roots =
  case roots of
    [r0, r1]
      | r0 > r1 -> [r1, r0]
    _ -> roots

solveQuadraticScanline :: Double -> Double -> Double -> [Double]
solveQuadraticScanline a b c
  | a == 0.0 || abs b > (1.0e12 * abs a) =
      if b == 0.0
        then []
        else [(- c) / b]
  | disc > 0.0 =
      let s = sqrt disc
       in [((- b) + s) / (2.0 * a), ((- b) - s) / (2.0 * a)]
  | disc == 0.0 = [(- b) / (2.0 * a)]
  | otherwise = []
  where
    disc = (b * b) - (4.0 * a * c)

preprocessScanlineIntersections :: [ScanlineIntersection] -> [ScanlineIntersection]
preprocessScanlineIntersections intersections =
  go 0 (sortBy (comparing (\intersection -> intersection.x)) intersections)
  where
    go _ [] = []
    go cumulative (intersection : rest) =
      let cumulative' = cumulative + intersection.direction
       in intersection {direction = cumulative'} : go cumulative' rest

fillRowFromIntersections :: Int -> Frame -> [ScanlineIntersection] -> [Bool]
fillRowFromIntersections dim frame intersections = go 0 0 intersections
  where
    go x _ _ | x >= dim = []
    go x currentDir remaining =
      let sampleX = pixelCenterGlyphX frame x
          (currentDir', remaining') = consumeIntersections sampleX currentDir remaining
       in (currentDir' /= 0) : go (x + 1) currentDir' remaining'

consumeIntersections :: Double -> Int -> [ScanlineIntersection] -> (Int, [ScanlineIntersection])
consumeIntersections sampleX currentDir remaining =
  case remaining of
    intersection : rest
      | sampleX >= intersection.x ->
          consumeIntersections sampleX intersection.direction rest
    _ -> (currentDir, remaining)

data SignedDist = SignedDist
  { distance :: !Double,
    dot :: !Double
  }
  deriving stock (Eq, Show)

data Selector = Selector
  { minTrue :: !SignedDist,
    nearEdge :: !(Maybe Edge),
    nearParam :: !Double,
    minNegPerp :: !Double,
    minPosPerp :: !Double
  }
  deriving stock (Eq, Show)

data Samples = Samples
  { dA :: !Double,
    dR :: !Double,
    dG :: !Double,
    dB :: !Double
  }
  deriving stock (Eq, Show)

hugeDistance :: Double
hugeDistance = 1.0e300

initialSelector :: Selector
initialSelector =
  Selector
    { minTrue = SignedDist {distance = negate hugeDistance, dot = 0.0},
      nearEdge = Nothing,
      nearParam = 0.0,
      minNegPerp = negate hugeDistance,
      minPosPerp = hugeDistance
    }

data SelectorSet = SelectorSet
  { rSel :: !Selector,
    gSel :: !Selector,
    bSel :: !Selector
  }
  deriving stock (Eq, Show)

data ContourDistance = ContourDistance
  { set :: !SelectorSet,
    dist :: !DistanceValue,
    winding :: !Int
  }
  deriving stock (Eq, Show)

data DistanceValue = DistanceValue
  { r :: !Double,
    g :: !Double,
    b :: !Double,
    a :: !Double
  }
  deriving stock (Eq, Show)

initialSelectorSet :: SelectorSet
initialSelectorSet =
  SelectorSet
    { rSel = initialSelector,
      gSel = initialSelector,
      bSel = initialSelector
    }

accumulateSamples :: Pt -> [ContourSelectorInput] -> Samples
accumulateSamples point contourInputs =
  let contourDistances = fmap (distanceForContour point) contourInputs
      shapeSet = foldl' mergeContour initialSelectorSet contourDistances
      shapeDistance = distanceFromSet point shapeSet
      innerSet = foldl' mergeInner initialSelectorSet contourDistances
      outerSet = foldl' mergeOuter initialSelectorSet contourDistances
      innerDistance = distanceFromSet point innerSet
      outerDistance = distanceFromSet point outerSet
      innerScalar = resolveDistance innerDistance
      outerScalar = resolveDistance outerDistance
      chosen =
        if innerScalar >= 0.0 && abs innerScalar <= abs outerScalar
          then
            let base = innerDistance
                chosen' = foldl' (pickPositiveContour outerScalar) base contourDistances
             in (chosen', 1)
          else
            if outerScalar <= 0.0 && abs outerScalar < abs innerScalar
              then
                let base = outerDistance
                    chosen' = foldl' (pickNegativeContour innerScalar) base contourDistances
                 in (chosen', -1)
              else (shapeDistance, 0)
      distance1 =
        if snd chosen == 0
          then fst chosen
          else foldl' (pickOppositeContour (snd chosen)) (fst chosen) contourDistances
      distance2 =
        if resolveDistance distance1 == resolveDistance shapeDistance
          then shapeDistance
          else distance1
   in Samples
        { dA = distance2.a,
          dR = distance2.r,
          dG = distance2.g,
          dB = distance2.b
        }
  where
    mergeContour acc contourDistance = mergeSelectorSet acc contourDistance.set

    mergeInner acc contourDistance
      | contourDistance.winding > 0 && resolveDistance contourDistance.dist >= 0 =
          mergeSelectorSet acc contourDistance.set
      | otherwise = acc

    mergeOuter acc contourDistance
      | contourDistance.winding < 0 && resolveDistance contourDistance.dist <= 0 =
          mergeSelectorSet acc contourDistance.set
      | otherwise = acc

    pickPositiveContour outerScalar current contourDistance
      | contourDistance.winding > 0
          && abs (resolveDistance contourDistance.dist) < abs outerScalar
          && resolveDistance contourDistance.dist > resolveDistance current =
          contourDistance.dist
      | otherwise = current

    pickNegativeContour innerScalar current contourDistance
      | contourDistance.winding < 0
          && abs (resolveDistance contourDistance.dist) < abs innerScalar
          && resolveDistance contourDistance.dist < resolveDistance current =
          contourDistance.dist
      | otherwise = current

    pickOppositeContour chosenWinding current contourDistance
      | contourDistance.winding /= chosenWinding
          && resolveDistance contourDistance.dist * resolveDistance current >= 0
          && abs (resolveDistance contourDistance.dist) < abs (resolveDistance current) =
          contourDistance.dist
      | otherwise = current

distanceForContour :: Pt -> ContourSelectorInput -> ContourDistance
distanceForContour point contourInput =
  let selectors =
        foldl'
          step
          initialSelectorSet
          contourInput.triples
      distance = distanceFromSet point selectors
   in ContourDistance
        { set = selectors,
          dist = distance,
          winding = contourInput.winding
        }
  where
    step selectors (prevEdge, curEdge, nextEdge) =
      selectors
        { rSel =
            if hasChannel ChR curEdge
              then selectorAddEdge point prevEdge curEdge nextEdge selectors.rSel
              else selectors.rSel,
          gSel =
            if hasChannel ChG curEdge
              then selectorAddEdge point prevEdge curEdge nextEdge selectors.gSel
              else selectors.gSel,
          bSel =
            if hasChannel ChB curEdge
              then selectorAddEdge point prevEdge curEdge nextEdge selectors.bSel
              else selectors.bSel
        }

distanceFromSet :: Pt -> SelectorSet -> DistanceValue
distanceFromSet point selectors =
  DistanceValue
    { r = selectorDistance point selectors.rSel,
      g = selectorDistance point selectors.gSel,
      b = selectorDistance point selectors.bSel,
      a = (selectorSetTrueDistance selectors).distance
    }

selectorSetTrueDistance :: SelectorSet -> SignedDist
selectorSetTrueDistance selectors =
  pickMinTrue selectors.rSel.minTrue (pickMinTrue selectors.gSel.minTrue selectors.bSel.minTrue)

pickMinTrue :: SignedDist -> SignedDist -> SignedDist
pickMinTrue lhs rhs =
  if lessSignedDist rhs lhs
    then rhs
    else lhs

resolveDistance :: DistanceValue -> Double
resolveDistance distance = median3 distance.r distance.g distance.b

mergeSelectorSet :: SelectorSet -> SelectorSet -> SelectorSet
mergeSelectorSet lhs rhs =
  SelectorSet
    { rSel = mergeSelector lhs.rSel rhs.rSel,
      gSel = mergeSelector lhs.gSel rhs.gSel,
      bSel = mergeSelector lhs.bSel rhs.bSel
    }

mergeSelector :: Selector -> Selector -> Selector
mergeSelector lhs rhs =
  let rhsCloser = lessSignedDist rhs.minTrue lhs.minTrue
      minTrue' = if rhsCloser then rhs.minTrue else lhs.minTrue
      nearEdge' = if rhsCloser then rhs.nearEdge else lhs.nearEdge
      nearParam' = if rhsCloser then rhs.nearParam else lhs.nearParam
   in lhs
        { minTrue = minTrue',
          nearEdge = nearEdge',
          nearParam = nearParam',
          minNegPerp = max lhs.minNegPerp rhs.minNegPerp,
          minPosPerp = min lhs.minPosPerp rhs.minPosPerp
        }

rotateRight :: [a] -> [a]
rotateRight values =
  case reverse values of
    [] -> []
    v : rest -> v : reverse rest

hasChannel :: Channel -> Edge -> Bool
hasChannel channel edge =
  edge.col .&. channelBit channel /= 0

channelBit :: Channel -> Word8
channelBit channel =
  case channel of
    ChR -> 1
    ChG -> 2
    ChB -> 4

lessSignedDist :: SignedDist -> SignedDist -> Bool
lessSignedDist a b =
  abs a.distance < abs b.distance
    || (abs a.distance == abs b.distance && a.dot < b.dot)

selectorAddEdge :: Pt -> Edge -> Edge -> Edge -> Selector -> Selector
selectorAddEdge point prevEdge curEdge nextEdge selector =
  let (edgeDistance, edgeParam) = signedDistance point curEdge
      selectorTrue =
        if lessSignedDist edgeDistance selector.minTrue
          then selector {minTrue = edgeDistance, nearEdge = Just curEdge, nearParam = edgeParam}
          else selector
      ap = diffPt point curEdge.a
      bp = diffPt point curEdge.b
      aDir = normalizeAllowZero (edgeStartTangent curEdge)
      bDir = normalizeAllowZero (edgeEndTangent curEdge)
      prevDir = normalizeAllowZero (edgeEndTangent prevEdge)
      nextDir = normalizeAllowZero (edgeStartTangent nextEdge)
      add = dotVec ap (normalizeAllowZero (addPt prevDir aDir))
      bdd = negate (dotVec bp (normalizeAllowZero (addPt bDir nextDir)))
      selectorA =
        if add > 0.0
          then
            case getPerpendicularDistance edgeDistance.distance ap (negPt aDir) of
              Just pd -> addPerpendicularDistance selectorTrue (negate pd)
              Nothing -> selectorTrue
          else selectorTrue
      selectorB =
        if bdd > 0.0
          then
            case getPerpendicularDistance edgeDistance.distance bp bDir of
              Just pd -> addPerpendicularDistance selectorA pd
              Nothing -> selectorA
          else selectorA
   in selectorB

addPerpendicularDistance :: Selector -> Double -> Selector
addPerpendicularDistance selector pd
  | pd <= 0.0 && pd > selector.minNegPerp =
      selector {minNegPerp = pd}
  | pd >= 0.0 && pd < selector.minPosPerp =
      selector {minPosPerp = pd}
  | otherwise = selector

selectorDistance :: Pt -> Selector -> Double
selectorDistance point selector =
  let baseDistance =
        if selector.minTrue.distance < 0.0
          then selector.minNegPerp
          else selector.minPosPerp
   in case selector.nearEdge of
        Nothing -> baseDistance
        Just edge ->
          let corrected = distanceToPerpendicularDistance selector.minTrue point edge selector.nearParam
           in if abs corrected.distance < abs baseDistance
                then corrected.distance
                else baseDistance

getPerpendicularDistance :: Double -> Pt -> Pt -> Maybe Double
getPerpendicularDistance distance0 ep edgeDir =
  let ts = dotVec ep edgeDir
   in if ts > 0.0
        then
          let perpendicularDistance = crossVec ep edgeDir
           in if abs perpendicularDistance < abs distance0
                then Just perpendicularDistance
                else Nothing
        else Nothing

signedDistance :: Pt -> Edge -> (SignedDist, Double)
signedDistance origin edge =
  case edge.c of
    Nothing -> signedDistanceLine origin edge
    Just ctrl -> signedDistanceQuad origin edge.a ctrl edge.b

signedDistanceLine :: Pt -> Edge -> (SignedDist, Double)
signedDistanceLine origin edge
  | abLenSq <= 1.0e-18 =
      let endpoint = diffPt edge.a origin
          dist = lengthVec endpoint
       in (SignedDist {distance = dist, dot = 0.0}, 0.0)
  | otherwise =
      let aq = diffPt origin edge.a
          paramVal = dotVec aq ab / abLenSq
          endpointVec =
            if paramVal > 0.5
              then diffPt edge.b origin
              else diffPt edge.a origin
          endpointDist = lengthVec endpointVec
       in if paramVal > 0.0 && paramVal < 1.0
            then
              let orthoDistance = dotVec (orthonormalFalse ab) aq
               in if abs orthoDistance < endpointDist
                    then (SignedDist {distance = orthoDistance, dot = 0.0}, paramVal)
                    else (endpointDistanceSample aq endpointVec endpointDist paramVal, paramVal)
            else (endpointDistanceSample aq endpointVec endpointDist paramVal, paramVal)
  where
    ab = diffPt edge.b edge.a
    abLenSq = dotVec ab ab
    endpointDistanceSample aq endpointVec endpointDist _ =
      SignedDist
        { distance = fromIntegral (nonZeroSign (crossVec aq ab)) * endpointDist,
          dot = abs (dotVec (normalizeDefault ab) (normalizeDefault endpointVec))
        }

signedDistanceQuad :: Pt -> Pt -> Pt -> Pt -> (SignedDist, Double)
signedDistanceQuad origin p0 p1 p2 =
  let qa = diffPt p0 origin
      ab = diffPt p1 p0
      br = diffPt (diffPt p2 p1) ab
      aCoeff = dotVec br br
      bCoeff = 3.0 * dotVec ab br
      cCoeff = (2.0 * dotVec ab ab) + dotVec qa br
      dCoeff = dotVec qa ab
      roots = solveCubicReal aCoeff bCoeff cCoeff dCoeff
      dir0 = ab
      startDistance = fromIntegral (nonZeroSign (crossVec dir0 qa)) * lengthVec qa
      startParam = negate (dotVec qa dir0) / dotVec dir0 dir0
      endVec = diffPt p2 origin
      endDistanceMag = lengthVec endVec
      (minDistance0, minParam0, endDir) =
        if endDistanceMag < abs startDistance
          then
            let dir1 = diffPt p2 p1
                minDistance1 = fromIntegral (nonZeroSign (crossVec dir1 endVec)) * endDistanceMag
                minParam1 = dotVec (diffPt origin p1) dir1 / dotVec dir1 dir1
             in (minDistance1, minParam1, dir1)
          else (startDistance, startParam, dir0)
      (minDistance1, minParam1) = foldl' (considerRoot qa ab br) (minDistance0, minParam0) roots
      dotTerm
        | minParam1 >= 0.0 && minParam1 <= 1.0 = 0.0
        | minParam1 < 0.5 = abs (dotVec (normalizeDefault dir0) (normalizeDefault qa))
        | otherwise = abs (dotVec (normalizeDefault endDir) (normalizeDefault endVec))
   in (SignedDist {distance = minDistance1, dot = dotTerm}, minParam1)

considerRoot :: Pt -> Pt -> Pt -> (Double, Double) -> Double -> (Double, Double)
considerRoot qa ab br (minDistance, minParam) t
  | t <= 0.0 || t >= 1.0 = (minDistance, minParam)
  | otherwise =
      let qe = addPt qa (addPt (scalePt (2.0 * t) ab) (scalePt (t * t) br))
          distance = lengthVec qe
       in if distance <= abs minDistance
            then
              let minDistance' = fromIntegral (nonZeroSign (crossVec (addPt ab (scalePt t br)) qe)) * distance
               in (minDistance', t)
            else (minDistance, minParam)

solveCubicReal :: Double -> Double -> Double -> Double -> [Double]
solveCubicReal a b c d
  | a /= 0.0 =
      let bn = b / a
       in if abs bn < 1.0e6
            then solveCubicNormed bn (c / a) (d / a)
            else solveQuadraticGeneral b c d
  | otherwise = solveQuadraticGeneral b c d

solveQuadraticGeneral :: Double -> Double -> Double -> [Double]
solveQuadraticGeneral a b c
  | a == 0.0 || abs b > (1.0e12 * abs a) =
      if b == 0.0
        then []
        else [(- c) / b]
  | disc > 0.0 =
      let s = sqrt disc
       in [((- b) + s) / (2.0 * a), ((- b) - s) / (2.0 * a)]
  | disc == 0.0 = [(- b) / (2.0 * a)]
  | otherwise = []
  where
    disc = (b * b) - (4.0 * a * c)

solveCubicNormed :: Double -> Double -> Double -> [Double]
solveCubicNormed a b c
  | r2 < q3 =
      let tRaw = r / sqrt q3
          t = clampNeg1Pos1 tRaw
          ang = acos t
          qRoot = -2.0 * sqrt q
          x0 = qRoot * cos (ang / 3.0) - aShift
          x1 = qRoot * cos ((ang + (2.0 * pi)) / 3.0) - aShift
          x2 = qRoot * cos ((ang - (2.0 * pi)) / 3.0) - aShift
       in [x0, x1, x2]
  | otherwise =
      let uSign = if r < 0.0 then 1.0 else -1.0
          u = uSign * ((abs r + sqrt (r2 - q3)) ** (1.0 / 3.0))
          v = if u == 0.0 then 0.0 else q / u
          x0 = (u + v) - aShift
       in if u == v || abs (u - v) < (1.0e-12 * abs (u + v))
            then [x0, (-0.5 * (u + v)) - aShift]
            else [x0]
  where
    a2 = a * a
    q = (a2 - (3.0 * b)) / 9.0
    r = (a * ((2.0 * a2) - (9.0 * b)) + (27.0 * c)) / 54.0
    r2 = r * r
    q3 = q * q * q
    aShift = a / 3.0

safeDiv :: Double -> Double -> Double
safeDiv n d
  | abs d <= 1.0e-18 = 0.0
  | otherwise = n / d

distanceToPerpendicularDistance :: SignedDist -> Pt -> Edge -> Double -> SignedDist
distanceToPerpendicularDistance sample origin edge paramVal
  | paramVal < 0.0 =
      let dir = normalizeDefault (edgeStartTangent edge)
          aq = diffPt origin edge.a
          ts = dotVec aq dir
       in if ts < 0.0
            then tryPerpendicular aq dir
            else sample
  | paramVal > 1.0 =
      let dir = normalizeDefault (edgeEndTangent edge)
          bq = diffPt origin edge.b
          ts = dotVec bq dir
       in if ts > 0.0
            then tryPerpendicular bq dir
            else sample
  | otherwise = sample
  where
    tryPerpendicular pointVec dir =
      let perpendicularDistance = crossVec pointVec dir
       in if abs perpendicularDistance <= abs sample.distance
            then sample {distance = perpendicularDistance, dot = 0.0}
            else sample

diffPt :: Pt -> Pt -> Pt
diffPt p q =
  Pt
    { x = p.x - q.x,
      y = p.y - q.y
    }

addPt :: Pt -> Pt -> Pt
addPt p q =
  Pt
    { x = p.x + q.x,
      y = p.y + q.y
    }

negPt :: Pt -> Pt
negPt p =
  Pt
    { x = negate p.x,
      y = negate p.y
    }

dotVec :: Pt -> Pt -> Double
dotVec a b = (a.x * b.x) + (a.y * b.y)

crossVec :: Pt -> Pt -> Double
crossVec a b = (a.x * b.y) - (a.y * b.x)

lengthVec :: Pt -> Double
lengthVec v = sqrt ((v.x * v.x) + (v.y * v.y))

scalePt :: Double -> Pt -> Pt
scalePt s p =
  Pt
    { x = p.x * s,
      y = p.y * s
    }

edgeStartTangent :: Edge -> Pt
edgeStartTangent edge =
  case edge.c of
    Nothing -> diffPt edge.b edge.a
    Just ctrl ->
      let tangent = scalePt 2 (diffPt ctrl edge.a)
          chord = diffPt edge.b edge.a
       in if tangent.x == 0.0 && tangent.y == 0.0
            then chord
            else tangent

edgeEndTangent :: Edge -> Pt
edgeEndTangent edge =
  case edge.c of
    Nothing -> diffPt edge.b edge.a
    Just ctrl ->
      let tangent = scalePt 2 (diffPt edge.b ctrl)
          chord = diffPt edge.b edge.a
       in if tangent.x == 0.0 && tangent.y == 0.0
            then chord
            else tangent

quadPoint :: Pt -> Pt -> Pt -> Double -> Pt
quadPoint p0 p1 p2 t =
  let u = 1.0 - t
      w0 = u * u
      w1 = 2.0 * u * t
      w2 = t * t
   in Pt
        { x = (w0 * p0.x) + (w1 * p1.x) + (w2 * p2.x),
          y = (w0 * p0.y) + (w1 * p1.y) + (w2 * p2.y)
        }

quadTangent :: Pt -> Pt -> Pt -> Double -> Pt
quadTangent p0 p1 p2 t =
  let u = 1.0 - t
      a = scalePt (2.0 * u) (diffPt p1 p0)
      b = scalePt (2.0 * t) (diffPt p2 p1)
   in addPt a b

orthonormalFalse :: Pt -> Pt
orthonormalFalse v =
  let len = lengthVec v
   in if len > 0.0
        then
          Pt
            { x = v.y / len,
              y = negate v.x / len
            }
        else
          Pt
            { x = 0.0,
              y = -1.0
            }

normalizeDefault :: Pt -> Pt
normalizeDefault v =
  let len = lengthVec v
   in if len > 0.0
        then
          Pt
            { x = v.x / len,
              y = v.y / len
            }
        else
          Pt
            { x = 0.0,
              y = 1.0
            }

normalizeAllowZero :: Pt -> Pt
normalizeAllowZero v =
  let len = lengthVec v
   in if len > 0.0
        then
          Pt
            { x = v.x / len,
              y = v.y / len
            }
        else
          Pt
            { x = 0.0,
              y = 0.0
            }

nonZeroSign :: Double -> Int
nonZeroSign n =
  if n > 0.0
    then 1
    else -1

toWord8 :: Double -> Word8
toWord8 x =
  let v = clamp01Float ((realToFrac x :: Float) + 2.0e-8)
      n = truncate (255.5 - (255.0 * v)) :: Int
   in fromIntegral (255 - n)

clamp01Float :: Float -> Float
clamp01Float x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

clampNeg1Pos1 :: Double -> Double
clampNeg1Pos1 x
  | x < (-1) = -1
  | x > 1 = 1
  | otherwise = x

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
