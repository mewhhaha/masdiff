{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native
  ( RasterPreparedIO,
    PreparedGlyph (..),
    PreparedLineSeg (..),
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import MSDF.Native.Raster (outlineMetrics, rasterizeOutline)
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
    col :: !Word8
  }
  deriving stock (Eq, Show)

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
  concatMap flattenContour edgeContours
  where
    edgeContours = buildEdgeContours cfg.seed prepared.ol.contours
    quadSteps =
      max
        4
        (min 24 (unDim cfg.dim `quot` 16))

    flattenContour edges = concatMap (flattenEdge quadSteps) edges

    flattenEdge steps edge =
      case edge.c of
        Nothing ->
          [mkSeg edge.a edge.b edge.col]
        Just _ ->
          [ mkSeg
              (edgePoint edge t0)
              (edgePoint edge t1)
              edge.col
            | i <- [0 .. steps - 1],
              let t0 = fromIntegral i / fromIntegral steps,
              let t1 = fromIntegral (i + 1) / fromIntegral steps
          ]

    mkSeg p0 p1 color =
      PreparedLineSeg
        { x0 = realToFrac p0.x,
          y0 = realToFrac p0.y,
          x1 = realToFrac p1.x,
          y1 = realToFrac p1.y,
          col = color
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
