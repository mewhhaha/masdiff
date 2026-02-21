{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native
  ( generateGlyphNativeIO,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import MSDF.Native.Raster (rasterizeOutline)
import MSDF.Native.TTF (VariationAxes (..), loadOutlineIO)
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
