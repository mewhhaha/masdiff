{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native
  ( generateGlyphNativeIO,
    generateGlyphBatchNativeIO,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (evaluate, mask, onException, throwIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Conc (numCapabilities)
import MSDF.Native.Raster (rasterizeOutline)
import MSDF.Native.TTF (VariationAxes (..), loadOutlineIO, loadOutlinesIO)
import MSDF.Native.Types (Outline)
import MSDF.Types
  ( AxisMap,
    AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut,
    GlyphCode,
    Mode (..),
  )
import System.Directory (doesFileExist)

generateGlyphNativeIO :: GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphNativeIO cfg src glyph =
  case validateCfg cfg of
    Left err -> pure (Left err)
    Right () ->
      case parseSource src of
        Left err -> pure (Left err)
        Right (fontPath, axes) -> do
          exists <- doesFileExist fontPath
          if not exists
            then pure (Left (MissingInput ("Font file not found: " <> fontPath)))
            else do
              outlineResult <- loadOutlineIO fontPath axes glyph
              case firstExecFailed outlineResult of
                Left err -> pure (Left err)
                Right outline -> pure (rasterizeOutline cfg outline)

generateGlyphBatchNativeIO :: Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
generateGlyphBatchNativeIO jobs cfg src glyphs =
  case validateCfg cfg of
    Left err -> pure (fmap (const (Left err)) glyphs)
    Right () ->
      case parseSource src of
        Left err -> pure (fmap (const (Left err)) glyphs)
        Right (fontPath, axes) -> do
          exists <- doesFileExist fontPath
          if not exists
            then pure (fmap (const (Left (MissingInput ("Font file not found: " <> fontPath)))) glyphs)
            else do
              outlines <- loadOutlinesIO fontPath axes glyphs
              renderOutlinesIO jobs cfg outlines

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

renderOutlinesIO :: Int -> GenCfg -> [Either String Outline] -> IO [Either GenErr GenOut]
renderOutlinesIO jobs cfg outlines
  | jobs <= 1 || numCapabilities <= 1 = pure (fmap renderOne outlines)
  | otherwise = mapConcurrentlyBounded jobs (evaluate . renderOne) outlines
  where
    renderOne outlineResult =
      case firstExecFailed outlineResult of
        Left err -> Left err
        Right outline -> rasterizeOutline cfg outline

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
