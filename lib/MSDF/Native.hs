{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native
  ( RasterPreparedIO,
    PreparedGlyph (..),
    PreparedLineSeg (..),
    hasProperSelfIntersection,
    requiresNonZeroWinding,
    prepareGlyphNativeIO,
    prepareGlyphBatchNativeIO,
    metricsPrepared,
    preparedLineSegs,
    rasterPreparedCpu,
    generateGlyphNativeWithIO,
    generateGlyphBatchNativeWithIO,
    generateGlyphNativeIO,
    generateGlyphBatchNativeIO,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (evaluate, mask, onException, throwIO)
import Data.Int (Int8)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import MSDF.Native.Raster (filterBoundaryEdges, outlineMetrics, rasterizeOutline)
import MSDF.Native.TTF (VariationAxes (..), loadOutlineIO, loadOutlinesIO)
import MSDF.Native.Types (Edge (..), Outline (..), Pt (..), buildEdgeContours)
import MSDF.Types
  ( AxisMap,
    AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut,
    GlyphCode,
    Metrics,
    Mode (..),
    unDim,
  )
import System.Directory (doesFileExist)

type RasterPreparedIO = GenCfg -> PreparedGlyph -> IO (Either GenErr GenOut)

data PreparedGlyph = PreparedGlyph
  { ol :: !Outline
  }
  deriving stock (Eq, Show)

data PreparedLineSeg = PreparedLineSeg
  { x0 :: !Float,
    y0 :: !Float,
    x1 :: !Float,
    y1 :: !Float,
    col :: !Word8,
    caps :: !Word8,
    cid :: !Word8,
    cw :: !Int8
  }
  deriving stock (Eq, Show)

hasProperSelfIntersection :: [PreparedLineSeg] -> Bool
hasProperSelfIntersection segs = go (fmap toSeg segs)
  where
    toSeg seg =
      ( realToFrac seg.x0 :: Double,
        realToFrac seg.y0 :: Double,
        realToFrac seg.x1 :: Double,
        realToFrac seg.y1 :: Double
      )

    go remaining =
      case remaining of
        [] -> False
        seg : rest ->
          any (segmentsIntersectProper seg) rest || go rest

requiresNonZeroWinding :: [PreparedLineSeg] -> Bool
requiresNonZeroWinding segs =
  hasProperSelfIntersection segs
    && windingDisagreementRatio segs >= nonZeroWindingDisagreementMin

nonZeroWindingDisagreementMin :: Double
nonZeroWindingDisagreementMin = 0.075

windingDisagreementRatio :: [PreparedLineSeg] -> Double
windingDisagreementRatio segs =
  case boundsOfSegs segs of
    Nothing -> 0
    Just (minX, minY, maxX, maxY) ->
      let w = maxX - minX
          h = maxY - minY
       in if w <= coordEps || h <= coordEps
            then 0
            else
              let n = windingProbeGridN
                  dx = w / fromIntegral n
                  dy = h / fromIntegral n
                  boundaryEps = max (1.0e-4) (0.001 * max w h)
                  probePoint ix iy =
                    let x = minX + ((fromIntegral ix + 0.5) * dx)
                        y = minY + ((fromIntegral iy + 0.5) * dy)
                     in (x, y)
                  isInteriorProbe point =
                    pointToSegmentsDistance segs point > boundaryEps
                  countConsidered acc point =
                    if isInteriorProbe point
                      then acc + 1
                      else acc
                  countMismatch acc point
                    | not (isInteriorProbe point) = acc
                    | otherwise =
                        let insideParity = parityInsideAt segs point
                            insideNonZero = nonZeroInsideAt segs point
                         in if insideParity /= insideNonZero
                              then acc + 1
                              else acc
                  points = [probePoint ix iy | iy <- [0 .. n - 1], ix <- [0 .. n - 1]]
                  considered = foldl' countConsidered (0 :: Int) points
                  mismatches = foldl' countMismatch (0 :: Int) points
               in if considered == 0
                    then 0
                    else fromIntegral mismatches / fromIntegral considered

windingProbeGridN :: Int
windingProbeGridN = 24

boundsOfSegs :: [PreparedLineSeg] -> Maybe (Double, Double, Double, Double)
boundsOfSegs segs =
  case segs of
    [] -> Nothing
    firstSeg : rest ->
      let x00 = realToFrac firstSeg.x0 :: Double
          y00 = realToFrac firstSeg.y0 :: Double
          x01 = realToFrac firstSeg.x1 :: Double
          y01 = realToFrac firstSeg.y1 :: Double
          initial =
            ( min x00 x01,
              min y00 y01,
              max x00 x01,
              max y00 y01
            )
       in Just (foldl' step initial rest)
  where
    step (minX, minY, maxX, maxY) seg =
      let xa = realToFrac seg.x0 :: Double
          ya = realToFrac seg.y0 :: Double
          xb = realToFrac seg.x1 :: Double
          yb = realToFrac seg.y1 :: Double
       in ( min minX (min xa xb),
            min minY (min ya yb),
            max maxX (max xa xb),
            max maxY (max ya yb)
          )

pointToSegmentsDistance :: [PreparedLineSeg] -> (Double, Double) -> Double
pointToSegmentsDistance segs point =
  foldl' min hugeDistance (fmap (pointToSegmentDistance point) segs)
  where
    hugeDistance = 1.0e12

pointToSegmentDistance :: (Double, Double) -> PreparedLineSeg -> Double
pointToSegmentDistance (px, py) seg =
  let ax = realToFrac seg.x0 :: Double
      ay = realToFrac seg.y0 :: Double
      bx = realToFrac seg.x1 :: Double
      by = realToFrac seg.y1 :: Double
      vx = bx - ax
      vy = by - ay
      wx = px - ax
      wy = py - ay
      vv = (vx * vx) + (vy * vy)
   in if vv <= coordEpsSq
        then sqrt ((wx * wx) + (wy * wy))
        else
          let t = max 0.0 (min 1.0 (((wx * vx) + (wy * vy)) / vv))
              qx = ax + (t * vx)
              qy = ay + (t * vy)
              dx = px - qx
              dy = py - qy
           in sqrt ((dx * dx) + (dy * dy))

parityInsideAt :: [PreparedLineSeg] -> (Double, Double) -> Bool
parityInsideAt segs point =
  odd (foldl' step 0 segs)
  where
    step acc seg = acc + parityWindingStep point seg

nonZeroInsideAt :: [PreparedLineSeg] -> (Double, Double) -> Bool
nonZeroInsideAt segs point =
  foldl' step 0 segs /= 0
  where
    step acc seg = acc + nonZeroWindingStep point seg

parityWindingStep :: (Double, Double) -> PreparedLineSeg -> Int
parityWindingStep (px, py) seg
  | not crosses = 0
  | abs dy <= coordEps = 0
  | xInt > px = 1
  | otherwise = 0
  where
    ax = realToFrac seg.x0 :: Double
    ay = realToFrac seg.y0 :: Double
    bx = realToFrac seg.x1 :: Double
    by = realToFrac seg.y1 :: Double
    crosses = (ay > py) /= (by > py)
    dy = by - ay
    xInt = ax + ((py - ay) * (bx - ax) / dy)

nonZeroWindingStep :: (Double, Double) -> PreparedLineSeg -> Int
nonZeroWindingStep (px, py) seg
  | ay <= py =
      if by > py && isLeft > coordEps
        then 1
        else 0
  | otherwise =
      if by <= py && isLeft < negate coordEps
        then -1
        else 0
  where
    ax = realToFrac seg.x0 :: Double
    ay = realToFrac seg.y0 :: Double
    bx = realToFrac seg.x1 :: Double
    by = realToFrac seg.y1 :: Double
    isLeft = ((bx - ax) * (py - ay)) - ((px - ax) * (by - ay))

segmentsIntersectProper ::
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double) ->
  Bool
segmentsIntersectProper (ax, ay, bx, by) (cx, cy, dx, dy)
  | not (bboxOverlaps (ax, ay, bx, by) (cx, cy, dx, dy)) = False
  | lengthSq (rx, ry) <= coordEpsSq || lengthSq (sx, sy) <= coordEpsSq = False
  | abs denom > coordEps =
      t > paramEps
        && t < (1.0 - paramEps)
        && u > paramEps
        && u < (1.0 - paramEps)
  | otherwise = False
  where
    rx = bx - ax
    ry = by - ay
    sx = dx - cx
    sy = dy - cy
    qmx = cx - ax
    qmy = cy - ay
    denom = cross2 (rx, ry) (sx, sy)
    t = cross2 (qmx, qmy) (sx, sy) / denom
    u = cross2 (qmx, qmy) (rx, ry) / denom

coordEps :: Double
coordEps = 1.0e-5

coordEpsSq :: Double
coordEpsSq = coordEps * coordEps

paramEps :: Double
paramEps = 0.2

lengthSq :: (Double, Double) -> Double
lengthSq (x, y) = (x * x) + (y * y)

cross2 :: (Double, Double) -> (Double, Double) -> Double
cross2 (x0, y0) (x1, y1) = (x0 * y1) - (y0 * x1)

bboxOverlaps ::
  (Double, Double, Double, Double) ->
  (Double, Double, Double, Double) ->
  Bool
bboxOverlaps (ax, ay, bx, by) (cx, cy, dx, dy) =
  not
    ( max ax bx < min cx dx - coordEps
        || max cx dx < min ax bx - coordEps
        || max ay by < min cy dy - coordEps
        || max cy dy < min ay by - coordEps
    )

prepareGlyphNativeIO :: FontSrc -> GlyphCode -> IO (Either GenErr PreparedGlyph)
prepareGlyphNativeIO src glyph =
  case parseSource src of
    Left err -> pure (Left err)
    Right (fontPath, axes) -> do
      exists <- doesFileExist fontPath
      if not exists
        then pure (Left (MissingInput ("Font file not found: " <> fontPath)))
        else do
          outlineResult <- loadOutlineIO fontPath axes glyph
          pure (PreparedGlyph <$> firstExecFailed outlineResult)

prepareGlyphBatchNativeIO :: FontSrc -> [GlyphCode] -> IO [Either GenErr PreparedGlyph]
prepareGlyphBatchNativeIO src glyphs =
  case parseSource src of
    Left err -> pure (fmap (const (Left err)) glyphs)
    Right (fontPath, axes) -> do
      exists <- doesFileExist fontPath
      if not exists
        then pure (fmap (const (Left (MissingInput ("Font file not found: " <> fontPath)))) glyphs)
        else do
          outlines <- loadOutlinesIO fontPath axes glyphs
          pure (fmap (fmap PreparedGlyph . firstExecFailed) outlines)

metricsPrepared :: GenCfg -> PreparedGlyph -> Metrics
metricsPrepared cfg prepared = outlineMetrics cfg prepared.ol

preparedLineSegs :: GenCfg -> PreparedGlyph -> [PreparedLineSeg]
preparedLineSegs cfg prepared =
  concat
    [ flattenContour contourIx (contourWindingSign edges) edges
      | (contourIx, edges) <- zip [0 :: Int ..] edgeContours
    ]
  where
    edgeContoursRaw = buildEdgeContours cfg.seed prepared.ol.contours
    edgeContours =
      if cfg.ovlp
        then filterBoundaryEdges edgeContoursRaw
        else edgeContoursRaw
    quadSteps =
      max
        4
        (min 24 (unDim cfg.dim `quot` 16))

    flattenContour contourIx contourWinding0 edges =
      let contourId = fromIntegral (contourIx `mod` 256) :: Word8
          contourWinding = fromIntegral (max (-1) (min 1 contourWinding0)) :: Int8
       in concatMap (flattenEdge contourId contourWinding quadSteps) edges

    flattenEdge contourId contourWinding steps edge =
      case edge.c of
        Nothing ->
          [mkSeg edge.a edge.b edge.col contourId contourWinding True True]
        Just _ ->
          [ mkSeg
              (edgePoint edge t0)
              (edgePoint edge t1)
              edge.col
              contourId
              contourWinding
              (i == 0)
              (i == (steps - 1))
            | i <- [0 .. steps - 1],
              let t0 = fromIntegral i / fromIntegral steps,
              let t1 = fromIntegral (i + 1) / fromIntegral steps
          ]

    mkSeg p0 p1 color contourId contourWinding capStart capEnd =
      PreparedLineSeg
        { x0 = realToFrac p0.x,
          y0 = realToFrac p0.y,
          x1 = realToFrac p1.x,
          y1 = realToFrac p1.y,
          col = color,
          caps =
            (if capStart then 1 else 0)
              + (if capEnd then 2 else 0),
          cid = contourId,
          cw = contourWinding
        }

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

contourWindingSign :: [Edge] -> Int
contourWindingSign edges =
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

shoelace :: Pt -> Pt -> Double
shoelace a b = (b.x - a.x) * (a.y + b.y)

signum0 :: Double -> Int
signum0 x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

rotateRight :: [a] -> [a]
rotateRight values =
  case reverse values of
    [] -> []
    v : rest -> v : reverse rest

rasterPreparedCpu :: GenCfg -> PreparedGlyph -> Either GenErr GenOut
rasterPreparedCpu cfg prepared = rasterizeOutline cfg prepared.ol

generateGlyphNativeWithIO :: RasterPreparedIO -> GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphNativeWithIO raster cfg src glyph =
  case validateCfg cfg of
    Left err -> pure (Left err)
    Right () -> do
      preparedResult <- prepareGlyphNativeIO src glyph
      case preparedResult of
        Left err -> pure (Left err)
        Right prepared -> raster cfg prepared

generateGlyphBatchNativeWithIO :: Int -> RasterPreparedIO -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
generateGlyphBatchNativeWithIO jobs raster cfg src glyphs =
  case validateCfg cfg of
    Left err -> pure (fmap (const (Left err)) glyphs)
    Right () -> do
      prepared <- prepareGlyphBatchNativeIO src glyphs
      renderPreparedBatchWithIO jobs cfg raster prepared

generateGlyphNativeIO :: GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphNativeIO =
  generateGlyphNativeWithIO (\cfg prepared -> pure (rasterPreparedCpu cfg prepared))

generateGlyphBatchNativeIO :: Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
generateGlyphBatchNativeIO jobs cfg src glyphs =
  case validateCfg cfg of
    Left err -> pure (fmap (const (Left err)) glyphs)
    Right () -> do
      prepared <- prepareGlyphBatchNativeIO src glyphs
      renderPreparedBatchCpuIO jobs cfg prepared

validateCfg :: GenCfg -> Either GenErr ()
validateCfg cfg =
  case cfg.mode of
    Mtsdf -> Right ()

parseSource :: FontSrc -> Either GenErr (FilePath, VariationAxes)
parseSource src =
  case src of
    FontFile path -> Right (path, VariationAxes {wght = Nothing, opsz = Nothing})
    VarFontFile path axes -> do
      parsedAxes <- parseAxes axes
      pure (path, parsedAxes)

parseAxes :: AxisMap -> Either GenErr VariationAxes
parseAxes axes = go (Map.toAscList axes) VariationAxes {wght = Nothing, opsz = Nothing}
  where
    go [] acc = Right acc
    go ((AxisTag rawTag, AxisVal rawVal) : rest) acc =
      let tag = T.toCaseFold rawTag
       in if not (isFinite rawVal)
            then Left (InvalidCfg ("Axis value must be finite for tag " <> T.unpack rawTag))
            else case tag of
              "wght" ->
                case acc.wght of
                  Nothing -> go rest acc {wght = Just rawVal}
                  Just _ -> Left (InvalidCfg "Duplicate wght axis value in varfont source.")
              "opsz" ->
                case acc.opsz of
                  Nothing -> go rest acc {opsz = Just rawVal}
                  Just _ -> Left (InvalidCfg "Duplicate opsz axis value in varfont source.")
              _ -> Left (Unsupported ("Unsupported variation axis tag: " <> T.unpack rawTag))

firstExecFailed :: Either String a -> Either GenErr a
firstExecFailed result =
  case result of
    Left err -> Left (ExecFailed ("Native TrueType/OpenType outline load failed: " <> err))
    Right x -> Right x

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

renderPreparedBatchCpuIO :: Int -> GenCfg -> [Either GenErr PreparedGlyph] -> IO [Either GenErr GenOut]
renderPreparedBatchCpuIO jobs cfg prepared
  | jobs <= 1 || numCapabilities <= 1 = pure (fmap renderOne prepared)
  | otherwise = mapConcurrentlyBounded jobs (evaluate . renderOne) prepared
  where
    renderOne preparedResult =
      case preparedResult of
        Left err -> Left err
        Right glyph -> rasterPreparedCpu cfg glyph

renderPreparedBatchWithIO :: Int -> GenCfg -> RasterPreparedIO -> [Either GenErr PreparedGlyph] -> IO [Either GenErr GenOut]
renderPreparedBatchWithIO jobs cfg raster prepared
  | jobs <= 1 || numCapabilities <= 1 = traverse renderOne prepared
  | otherwise = mapConcurrentlyBounded jobs renderOne prepared
  where
    renderOne preparedResult =
      case preparedResult of
        Left err -> pure (Left err)
        Right glyph -> raster cfg glyph

mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded jobs action inputs = do
  sem <- newQSem (max 1 jobs)
  resultVars <- traverse (spawnOne sem) inputs
  results <- traverse takeMVar resultVars
  case firstFailure results of
    Just ex -> throwIO ex
    Nothing -> pure (collectRights results)
  where
    spawnOne sem input = do
      mask $ \restore -> do
        waitQSem sem
        resultVar <- newEmptyMVar
        let release = signalQSem sem
        _ <-
          forkFinally
            (restore (action input))
            (\result -> do
               putMVar resultVar result
               release
            )
            `onException` release
        pure resultVar

    firstFailure = foldr pickFirstFailure Nothing
    pickFirstFailure result acc =
      case result of
        Left ex -> Just ex
        Right _ -> acc
    collectRights = foldr collect []
    collect result acc =
      case result of
        Left _ -> acc
        Right value -> value : acc
