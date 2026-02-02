{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.Char (ord, toUpper)
import Data.List (nub)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (MSDFConfig(..), defaultMSDFConfig, renderGlyphMSDF)
import MSDF.TTF.Parser (Cmap(..), Head(..), ParseError(..), parseTTF, TTF(..))
import MSDF.Types (BitmapFormat(..), MSDFBitmap(..), GlyphMSDF(..), bitmapChannels)
import Paths_masdiff (getDataFileName)


data Options = Options
  { optFontPath :: Maybe FilePath
  , optText :: String
  , optPixelSize :: Int
  , optRange :: Int
  , optPadding :: Int
  , optFormat :: BitmapFormat
  , optOutDir :: FilePath
  }

usage :: String
usage =
  unlines
    [ "msdf-dump --font PATH --text TEXT --out-dir DIR"
    , "          [--pixel-size N] [--range N] [--padding N]"
    , "          [--format msdf|mtsdf]"
    ]

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  fontPath <- resolveFontPath opts
  parsed <- parseTTF fontPath
  ttf <- case parsed of
    Left (ParseError ctx msg) -> die (ctx ++ ": " ++ msg)
    Right val -> pure val
  createDirectoryIfMissing True opts.optOutDir
  let cps = nub (map ord opts.optText)
      cfg0 = defaultMSDFConfig
      cfg = cfg0
        { MSDF.pixelSize = opts.optPixelSize
        , MSDF.range = opts.optRange
        , MSDF.atlas = (cfg0.atlas { MSDF.atlasPadding = opts.optPadding, MSDF.packAtlas = False })
        , MSDF.outputFormat = opts.optFormat
        }
  mapM_ (dumpGlyph ttf cfg opts.optOutDir) cps

parseArgs :: [String] -> IO Options
parseArgs args = go args defaultOpts
  where
    defaultOpts = Options
      { optFontPath = Nothing
      , optText = "masdiff"
      , optPixelSize = 128
      , optRange = 12
      , optPadding = 16
      , optFormat = BitmapMSDF
      , optOutDir = "out/msdfgen_compare/masdiff"
      }

    go [] opts = pure opts
    go ("--font":path:rest) opts = go rest opts { optFontPath = Just path }
    go ("--text":txt:rest) opts = go rest opts { optText = txt }
    go ("--pixel-size":v:rest) opts = go rest opts { optPixelSize = read v }
    go ("--range":v:rest) opts = go rest opts { optRange = read v }
    go ("--padding":v:rest) opts = go rest opts { optPadding = read v }
    go ("--format":v:rest) opts = go rest opts { optFormat = parseFormat v }
    go ("--out-dir":dir:rest) opts = go rest opts { optOutDir = dir }
    go ("-h":_) _ = die usage
    go ("--help":_) _ = die usage
    go (_:rest) opts = go rest opts

parseFormat :: String -> BitmapFormat
parseFormat v =
  case map toUpper v of
    "MSDF" -> BitmapMSDF
    "MTSDF" -> BitmapMTSDF
    _ -> BitmapMSDF

resolveFontPath :: Options -> IO FilePath
resolveFontPath opts =
  case opts.optFontPath of
    Just path -> do
      exists <- doesFileExist path
      if exists then pure path else die ("font not found: " ++ path)
    Nothing -> getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"


dumpGlyph :: TTF -> MSDFConfig -> FilePath -> Int -> IO ()
dumpGlyph ttf cfg outDir cp =
  case lookupCodepoint cp (ttfMappings ttf) of
    Nothing -> putStrLn ("missing codepoint U+" ++ padHex cp)
    Just glyphIndex -> do
      let glyph = renderGlyphMSDF cfg ttf glyphIndex
          bmp = case glyph of
            GlyphMSDF { bitmap = b } -> b
          chans = bitmapChannels bmp.format
          rawPath = outDir </> (glyphName cp <.> "raw")
          metaPath = outDir </> (glyphName cp <.> "json")
          bytes = BS.pack (UA.elems bmp.pixels)
          unitsPerEm = case ttf of
            TTF { head = Head { unitsPerEm = upm } } -> upm
          scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
          translateX = if scale == 0 then 0 else (-bmp.offsetX / scale)
          translateY = if scale == 0 then 0 else (-bmp.offsetY / scale)
      BS.writeFile rawPath bytes
      writeFile metaPath (glyphMeta cp glyphIndex bmp cfg chans unitsPerEm scale translateX translateY)

ttfMappings :: TTF -> [(Int, Int)]
ttfMappings ttf =
  case ttf of
    TTF { cmap = Cmap ms } -> ms

lookupCodepoint :: Int -> [(Int, Int)] -> Maybe Int
lookupCodepoint cp mappings = lookup cp mappings

padHex :: Int -> String
padHex cp =
  let hex = map toUpper (showHex cp "")
      minWidth = if cp <= 0xFFFF then 4 else 6
      pad = replicate (max 0 (minWidth - length hex)) '0'
  in pad ++ hex

glyphName :: Int -> String
glyphName cp = "U+" ++ padHex cp

glyphMeta :: Int -> Int -> MSDFBitmap -> MSDFConfig -> Int -> Int -> Double -> Double -> Double -> String
glyphMeta cp glyphIndex bmp cfg chans unitsPerEm scale translateX translateY =
  let fmt = case bmp.format of
        BitmapMSDF -> "msdf"
        BitmapMTSDF -> "mtsdf"
  in unlines
      [ "{"
      , "  \"codepoint\": " ++ show cp ++ ","
      , "  \"glyphIndex\": " ++ show glyphIndex ++ ","
      , "  \"format\": \"" ++ fmt ++ "\","
      , "  \"width\": " ++ show bmp.width ++ ","
      , "  \"height\": " ++ show bmp.height ++ ","
      , "  \"channels\": " ++ show chans ++ ","
      , "  \"offsetX\": " ++ show bmp.offsetX ++ ","
      , "  \"offsetY\": " ++ show bmp.offsetY ++ ","
      , "  \"pixelSize\": " ++ show cfg.pixelSize ++ ","
      , "  \"range\": " ++ show cfg.range ++ ","
      , "  \"unitsPerEm\": " ++ show unitsPerEm ++ ","
      , "  \"scale\": " ++ show scale ++ ","
      , "  \"translateX\": " ++ show translateX ++ ","
      , "  \"translateY\": " ++ show translateY
      , "}"
      ]


die :: String -> IO a
die msg = do
  putStrLn msg
  exitFailure
