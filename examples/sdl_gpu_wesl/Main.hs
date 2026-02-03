{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Array ((!))
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, floatLE, word8, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord, toLower)
import Data.List (foldl', isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeBaseName, (<.>))
import Data.Word (Word8)

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (GlyphSet(..))
import MSDF.Generated (generateMSDFFromTTF)
import MSDF.Render (glyphQuad, glyphUV, pixelRangeForAtlas)
import MSDF.Types (AtlasImage(..), BitmapFormat(..), GlyphMSDF(..), MSDFAtlas(..), lookupCodepoint)
import MSDF.TTF.Parser (ParseError(..), parseTTF, TTF)
import Spirdo.Wesl (compile, renderCompileError, sourceFile, shaderSpirv)

shaderDirName :: FilePath
shaderDirName = "shaders"

outDirName :: FilePath
outDirName = "out"

screenW, screenH :: Double
screenW = 1280
screenH = 720

pangramText :: String
pangramText = "The quick brown fox jumps over the lazy dog"

defaultSampleText :: String
defaultSampleText = "f"

data CliOptions = CliOptions
  { fontPathOpt :: Maybe FilePath
  , pixelSizeOpt :: Maybe Int
  , rangeOpt :: Maybe Int
  , paddingOpt :: Maybe Int
  , correctionOpt :: Maybe Double
  , edgeOpt :: Maybe Double
  , hardOpt :: Maybe Double
  , conflictOpt :: Maybe Double
  , flatnessOpt :: Maybe Double
  , signModeOpt :: Maybe MSDF.SignMode
  , fillRuleOpt :: Maybe MSDF.FillRule
  , overlapOpt :: Maybe Bool
  , overlapEpsOpt :: Maybe Double
  , coloringOpt :: Maybe MSDF.ColoringStrategy
  , pseudoOpt :: Maybe Bool
  , packAtlasOpt :: Maybe Bool
  }

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions
    { fontPathOpt = Nothing
    , pixelSizeOpt = Nothing
    , rangeOpt = Nothing
    , paddingOpt = Nothing
    , correctionOpt = Nothing
    , edgeOpt = Nothing
    , hardOpt = Nothing
    , conflictOpt = Nothing
    , flatnessOpt = Nothing
    , signModeOpt = Nothing
    , fillRuleOpt = Nothing
    , overlapOpt = Nothing
    , overlapEpsOpt = Nothing
    , coloringOpt = Nothing
    , pseudoOpt = Nothing
    , packAtlasOpt = Nothing
    }

data ParseResult
  = ParseOk CliOptions
  | ParseHelp
  | CliParseError String

usage :: String
usage =
  unlines
    [ "msdf-sdl-gen [--font PATH] [--pixel-size N] [--range N] [--padding N]"
    , "             [--correction X] [--edge-threshold X] [--hard-threshold X]"
    , "             [--conflict X] [--flatness X]"
    , "             [--sign-mode winding|scanline] [--fill-rule nonzero|odd|positive|negative]"
    , "             [--overlap | --no-overlap] [--overlap-epsilon X]"
    , "             [--coloring simple|inktrap|distance] [--pseudo-distance | --no-pseudo-distance]"
    , "             [--no-pack]"
    , ""
    , "Examples:"
    , "  msdf-sdl-gen --font /path/to/font.ttf"
    , "  msdf-sdl-gen --range 16 --padding 24 --correction 0.08 --edge-threshold 1.0"
    ]

main :: IO ()
main = do
  baseDir <- resolveBaseDir
  let shaderDir = baseDir </> shaderDirName
      outDir = baseDir </> outDirName
  createDirectoryIfMissing True outDir
  compileWesl outDir (shaderDir </> "msdf.vert.wesl")
  compileWesl outDir (shaderDir </> "msdf.frag.wesl")

  sampleText <- resolveSampleText
  args <- getArgs
  opts <- case parseArgs args defaultCliOptions of
    ParseHelp -> do
      putStrLn usage
      exitSuccess
    CliParseError msg -> do
      putStrLn msg
      putStrLn usage
      exitFailure
    ParseOk parsed -> pure parsed

  fontPath <- resolveFontPath baseDir opts
  let cfg0 = MSDF.defaultMSDFConfig
      cfgBase = cfg0
        { MSDF.pixelSize = 256
        , MSDF.range = 12
        , MSDF.atlas = cfg0.atlas { MSDF.atlasPadding = 16, MSDF.packAtlas = True }
        , MSDF.correction = cfg0.correction { MSDF.channelThreshold = 0.1, MSDF.edgeThreshold = 1.0, MSDF.hardThreshold = 0.05 }
        , MSDF.outline = cfg0.outline { MSDF.windingFlatness = 0.02 }
        , MSDF.distance = cfg0.distance { MSDF.signMode = MSDF.SignScanline }
        , MSDF.coloring = cfg0.coloring { MSDF.conflictDistance = 1.0 }
        , MSDF.glyphSet = GlyphSetCodepoints (map ord sampleText)
        }
      cfg = applyCliOverrides opts cfgBase
  parsed <- parseTTF fontPath
  case parsed of
    Left err -> do
      let ParseError { context = ctx, message = msg } = err
      putStrLn ("parseTTF failed: " <> ctx <> ": " <> msg)
      exitFailure
    Right ttf -> do
      let outputs =
            [ ("mtsdf", BitmapMTSDF, screenW * 0.5, False)
            ]
      mapM_ (writeAtlas outDir sampleText cfg ttf) outputs

resolveSampleText :: IO String
resolveSampleText = do
  val <- lookupEnv "SDL_MSDF_SAMPLE_TEXT"
  case fmap (map toLower) val of
    Nothing -> pure defaultSampleText
    Just "pangram" -> pure pangramText
    Just "" -> pure defaultSampleText
    Just other -> pure other

resolveBaseDir :: IO FilePath
resolveBaseDir = do
  let local = "." </> shaderDirName </> "msdf.vert.wesl"
      fromRoot = "examples" </> "sdl_gpu_wesl" </> shaderDirName </> "msdf.vert.wesl"
  localOk <- doesFileExist local
  if localOk
    then pure "."
    else do
      rootOk <- doesFileExist fromRoot
      if rootOk
        then pure ("examples" </> "sdl_gpu_wesl")
        else do
          putStrLn "could not locate shaders; run from repo root or examples/sdl_gpu_wesl"
          exitFailure

resolveFontPath :: FilePath -> CliOptions -> IO FilePath
resolveFontPath baseDir opts = do
  let defaultPath = baseDir </> ".." </> ".." </> "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
      path = fromMaybe defaultPath opts.fontPathOpt
  exists <- doesFileExist path
  if exists
    then pure path
    else do
      putStrLn ("font not found: " <> path)
      putStrLn "pass a font path: msdf-sdl-gen --font /path/to/font.ttf"
      exitFailure

compileWesl :: FilePath -> FilePath -> IO ()
compileWesl outDir src = do
  result <- compile [] (sourceFile src)
  case result of
    Left err -> do
      putStrLn (renderCompileError err)
      exitFailure
    Right bundle -> do
      let outPath = outDir </> (takeBaseName src <.> "spv")
      BS.writeFile outPath (shaderSpirv bundle)
      putStrLn ("wrote " <> outPath)

writeAtlas :: FilePath -> String -> MSDF.MSDFConfig -> TTF -> (String, BitmapFormat, Double, Bool) -> IO ()
writeAtlas outDir sampleText cfgBase ttf (label, fmt, centerX, writeDefault) =
  let cfg = cfgBase { MSDF.outputFormat = fmt }
      atlas = generateMSDFFromTTF cfg ttf
  in
  case atlas of
    MSDFAtlas { atlas = Nothing } -> do
      putStrLn "atlas packing failed (packAtlas was true)"
      exitFailure
    MSDFAtlas { atlas = Just img, pixelSize = ps } -> do
      let AtlasImage { width = aw, height = ah, format = fmtImg, pixels = px } = img
          rgbaBytes = bitmapToRgbaBytes fmtImg px
          pxRange = pixelRangeForAtlas atlas (fromIntegral ps :: Double)
          bounds = textBounds atlas sampleText
          penStart' = snapPen (penForCenter (centerX, screenH * 0.5) bounds)
          texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
          verts = buildTextVertices atlas (screenW, screenH) penStart' texel sampleText
          verticesOut = outDir </> ("vertices_" <> label <.> "bin")
          atlasOut = outDir </> ("atlas_" <> label <.> "rgba")
          metaOut = outDir </> ("meta_" <> label <.> "txt")
      writeOutputs atlasOut verticesOut metaOut aw ah ps pxRange verts rgbaBytes
      when writeDefault $ do
        let verticesOut' = outDir </> "vertices.bin"
            atlasOut' = outDir </> "atlas.rgba"
            metaOut' = outDir </> "meta.txt"
        writeOutputs atlasOut' verticesOut' metaOut' aw ah ps pxRange verts rgbaBytes
      putStrLn ("wrote " <> atlasOut)
      putStrLn ("wrote " <> verticesOut)
      putStrLn ("wrote " <> metaOut)

applyCliOverrides :: CliOptions -> MSDF.MSDFConfig -> MSDF.MSDFConfig
applyCliOverrides opts cfg =
  let atlasCfg = cfg.atlas
      corrCfg = cfg.correction
      outlineCfg = cfg.outline
      coloringCfg = cfg.coloring
      distCfg = cfg.distance
  in cfg
    { MSDF.pixelSize = fromMaybe cfg.pixelSize opts.pixelSizeOpt
    , MSDF.range = fromMaybe cfg.range opts.rangeOpt
    , MSDF.atlas = atlasCfg
        { MSDF.atlasPadding = fromMaybe atlasCfg.atlasPadding opts.paddingOpt
        , MSDF.packAtlas = fromMaybe atlasCfg.packAtlas opts.packAtlasOpt
        }
    , MSDF.correction = corrCfg
        { MSDF.channelThreshold = fromMaybe corrCfg.channelThreshold opts.correctionOpt
        , MSDF.edgeThreshold = fromMaybe corrCfg.edgeThreshold opts.edgeOpt
        , MSDF.hardThreshold = fromMaybe corrCfg.hardThreshold opts.hardOpt
        }
    , MSDF.coloring = coloringCfg
        { MSDF.conflictDistance = fromMaybe coloringCfg.conflictDistance opts.conflictOpt
        , MSDF.strategy = fromMaybe coloringCfg.strategy opts.coloringOpt
        }
    , MSDF.outline = outlineCfg
        { MSDF.windingFlatness = fromMaybe outlineCfg.windingFlatness opts.flatnessOpt
        }
    , MSDF.distance = distCfg
        { MSDF.signMode = fromMaybe distCfg.signMode opts.signModeOpt
        , MSDF.fillRule = fromMaybe distCfg.fillRule opts.fillRuleOpt
        , MSDF.overlapSupport = fromMaybe distCfg.overlapSupport opts.overlapOpt
        , MSDF.overlapEpsilon = fromMaybe distCfg.overlapEpsilon opts.overlapEpsOpt
        , MSDF.pseudoDistance = fromMaybe distCfg.pseudoDistance opts.pseudoOpt
        }
    }

parseArgs :: [String] -> CliOptions -> ParseResult
parseArgs [] opts = ParseOk opts
parseArgs ("--help":_) _ = ParseHelp
parseArgs ("--font":p:rest) opts = parseArgs rest opts { fontPathOpt = Just p }
parseArgs ("--pixel-size":n:rest) opts =
  parseNumber "pixel size" n (\v -> opts { pixelSizeOpt = Just v }) rest
parseArgs ("--range":n:rest) opts =
  parseNumber "range" n (\v -> opts { rangeOpt = Just v }) rest
parseArgs ("--padding":n:rest) opts =
  parseNumber "padding" n (\v -> opts { paddingOpt = Just v }) rest
parseArgs ("--correction":x:rest) opts =
  parseDouble "correction" x (\v -> opts { correctionOpt = Just v }) rest
parseArgs ("--edge-threshold":x:rest) opts =
  parseDouble "edge-threshold" x (\v -> opts { edgeOpt = Just v }) rest
parseArgs ("--hard-threshold":x:rest) opts =
  parseDouble "hard-threshold" x (\v -> opts { hardOpt = Just v }) rest
parseArgs ("--conflict":x:rest) opts =
  parseDouble "conflict" x (\v -> opts { conflictOpt = Just v }) rest
parseArgs ("--flatness":x:rest) opts =
  parseDouble "flatness" x (\v -> opts { flatnessOpt = Just v }) rest
parseArgs ("--sign-mode":v:rest) opts =
  case parseSignMode v of
    Just mode -> parseArgs rest opts { signModeOpt = Just mode }
    Nothing -> CliParseError ("invalid sign-mode: " <> v)
parseArgs ("--fill-rule":v:rest) opts =
  case parseFillRule v of
    Just rule -> parseArgs rest opts { fillRuleOpt = Just rule }
    Nothing -> CliParseError ("invalid fill-rule: " <> v)
parseArgs ("--overlap":rest) opts = parseArgs rest opts { overlapOpt = Just True }
parseArgs ("--no-overlap":rest) opts = parseArgs rest opts { overlapOpt = Just False }
parseArgs ("--overlap-epsilon":x:rest) opts =
  parseDouble "overlap-epsilon" x (\v -> opts { overlapEpsOpt = Just v }) rest
parseArgs ("--coloring":v:rest) opts =
  case parseColoring v of
    Just mode -> parseArgs rest opts { coloringOpt = Just mode }
    Nothing -> CliParseError ("invalid coloring: " <> v)
parseArgs ("--pseudo-distance":rest) opts = parseArgs rest opts { pseudoOpt = Just True }
parseArgs ("--no-pseudo-distance":rest) opts = parseArgs rest opts { pseudoOpt = Just False }
parseArgs ("--no-pack":rest) opts = parseArgs rest opts { packAtlasOpt = Just False }
parseArgs (arg:rest) opts
  | "-" `isPrefixOf` arg = CliParseError ("unknown option: " <> arg)
  | otherwise =
      case opts.fontPathOpt of
        Nothing -> parseArgs rest opts { fontPathOpt = Just arg }
        Just _ -> CliParseError ("unexpected positional argument: " <> arg)

parseNumber :: String -> String -> (Int -> CliOptions) -> [String] -> ParseResult
parseNumber label raw apply rest =
  case readInt raw of
    Just v -> parseArgs rest (apply v)
    Nothing -> CliParseError ("invalid " <> label <> ": " <> raw)

parseDouble :: String -> String -> (Double -> CliOptions) -> [String] -> ParseResult
parseDouble label raw apply rest =
  case readDouble raw of
    Just v -> parseArgs rest (apply v)
    Nothing -> CliParseError ("invalid " <> label <> ": " <> raw)

readInt :: String -> Maybe Int
readInt s =
  case reads s of
    [(v, "")] -> Just v
    _ -> Nothing

readDouble :: String -> Maybe Double
readDouble s =
  case reads s of
    [(v, "")] -> Just v
    _ -> Nothing

parseSignMode :: String -> Maybe MSDF.SignMode
parseSignMode s =
  case map toLower s of
    "winding" -> Just MSDF.SignWinding
    "scanline" -> Just MSDF.SignScanline
    _ -> Nothing

parseFillRule :: String -> Maybe MSDF.FillRule
parseFillRule s =
  case map toLower s of
    "nonzero" -> Just MSDF.FillNonZero
    "odd" -> Just MSDF.FillOdd
    "positive" -> Just MSDF.FillPositive
    "negative" -> Just MSDF.FillNegative
    _ -> Nothing

parseColoring :: String -> Maybe MSDF.ColoringStrategy
parseColoring s =
  case map toLower s of
    "simple" -> Just MSDF.ColoringSimple
    "inktrap" -> Just MSDF.ColoringInktrap
    "distance" -> Just MSDF.ColoringDistance
    _ -> Nothing

bitmapToRgbaBytes :: BitmapFormat -> UA.UArray Int Word8 -> BS.ByteString
bitmapToRgbaBytes fmt arr =
  BL.toStrict (toLazyByteString (go (UA.elems arr)))
  where
    go xs =
      case fmt of
        BitmapMSDF ->
          case xs of
            (r:g:b:rest) -> word8 r <> word8 g <> word8 b <> word8 255 <> go rest
            [] -> mempty
            _ -> error "bitmapToRgbaBytes: invalid MSDF RGB length"
        BitmapMTSDF ->
          case xs of
            (r:g:b:a:rest) -> word8 r <> word8 g <> word8 b <> word8 a <> go rest
            [] -> mempty
            _ -> error "bitmapToRgbaBytes: invalid MTSDF RGBA length"

floatListBuilder :: [Float] -> Builder
floatListBuilder = foldMap floatLE

buildTextVertices :: MSDFAtlas -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> [Float]
buildTextVertices atlas screen (penX0, penY) texel text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      step (acc, penX) ch =
        case lookupCodepoint atlas (ord ch) of
          Nothing -> (acc, penX)
          Just gi ->
            let glyph = glyphsArr ! gi
                GlyphMSDF { advance = adv } = glyph
                quad = quadVerts screen (penX, penY) texel glyph
                penX' = penX + adv
            in (quad : acc, penX')
      (chunks, _) = foldl' step ([], penX0) text
  in concat (reverse chunks)

quadVerts :: (Double, Double) -> (Double, Double) -> (Double, Double) -> GlyphMSDF -> [Float]
quadVerts screen pen texel glyph =
  let (x0, y0, x1, y1) = glyphQuad glyph pen
      (u0, v0, u1, v1) = insetUV texel (glyphUV glyph)
      (cx0, cy0) = toClip screen (x0, y0)
      (cx1, cy1) = toClip screen (x1, y1)
  in [ cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy0, realToFrac u1, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy1, realToFrac u0, realToFrac v1
     ]

toClip :: (Double, Double) -> (Double, Double) -> (Float, Float)
toClip (w, h) (x, y) =
  ( realToFrac (x / (w * 0.5) - 1)
  , realToFrac (y / (h * 0.5) - 1)
  )

insetUV :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
insetUV (du, dv) (u0, v0, u1, v1) =
  let insetU = du * 0.5
      insetV = dv * 0.5
  in (u0 + insetU, v0 + insetV, u1 - insetU, v1 - insetV)

textBounds :: MSDFAtlas -> String -> (Double, Double, Double, Double)
textBounds atlas text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      step (penX, acc) ch =
        case lookupCodepoint atlas (ord ch) of
          Nothing -> (penX, acc)
          Just gi ->
            let glyph = glyphsArr ! gi
                GlyphMSDF { advance = adv } = glyph
                (x0, y0, x1, y1) = glyphQuad glyph (penX, 0)
                acc' = case acc of
                  Nothing -> Just (x0, y0, x1, y1)
                  Just (mnx, mny, mxx, mxy) ->
                    Just (min mnx x0, min mny y0, max mxx x1, max mxy y1)
            in (penX + adv, acc')
      (_, bounds) = foldl' step (0, Nothing) text
  in case bounds of
       Just b -> b
       Nothing -> (0, 0, 0, 0)

penForCenter :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double)
penForCenter (cx, cy) (minX, minY, maxX, maxY) =
  let midX = (minX + maxX) * 0.5
      midY = (minY + maxY) * 0.5
  in (cx - midX, cy - midY)

snapPen :: (Double, Double) -> (Double, Double)
snapPen (x, y) = (fromIntegral (round x :: Int), fromIntegral (round y :: Int))

writeOutputs :: FilePath -> FilePath -> FilePath -> Int -> Int -> Int -> Double -> [Float] -> BS.ByteString -> IO ()
writeOutputs atlasOut verticesOut metaOut aw ah ps pxRange verts rgbaBytes = do
  BS.writeFile atlasOut rgbaBytes
  BL.writeFile verticesOut (toLazyByteString (floatListBuilder verts))
  writeFile metaOut $ unlines
    [ "atlasWidth " <> show aw
    , "atlasHeight " <> show ah
    , "pixelSize " <> show ps
    , "pxRange " <> show pxRange
    , "screenWidth " <> show screenW
    , "screenHeight " <> show screenH
    , "vertexCount " <> show (length verts `div` 4)
    ]
