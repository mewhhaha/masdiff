{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Array ((!))
import Data.Array.IArray (bounds, elems)
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Builder (Builder, byteString, floatLE, word32LE, word8, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.))
import Data.Char (isSpace, ord, toLower)
import Data.List (foldl', isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Numeric (showHex)
import Control.Monad (foldM, forM_, when)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeBaseName, (<.>))
import Data.Word (Word8, Word16)
import Text.Read (readMaybe)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (BufferMode(..), hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (GlyphSet(..))
import MSDF.Generated (generateMSDFFromTTF)
import MSDF.Render (glyphQuad, glyphUV, pixelRangeForAtlas)
import MSDF.Binary (ByteBuffer(..), readU8)
import MSDF.Outline (Point(..))
import MSDF.Types (AtlasImage(..), BitmapFormat(..), GlyphMSDF(..), MSDFAtlas(..), MSDFBitmap(..), lookupCodepoint)
import MSDF.TTF.Parser (Cmap(..), ParseError(..), glyphBBoxRawAt, glyphOutlineAt, parseTTF, TTF(..))
import MSDF.TTF.Variations (Fvar(..), FvarAxis(..), Gvar(..), Variations(..), gvarDeltaSum, gvarTupleDeltaStats, gvarTupleHeaderStats, gvarTupleScalars, normalizeLocation)
import MSDF.LazyAtlas (LazyAtlas, LazyAtlasConfig(..), defaultLazyAtlasConfig, ensureGlyph, newLazyAtlas, snapshotAtlasImage)
import MSDF.Config (AtlasConfig(..))
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
  , lazyAtlasOpt :: Maybe Bool
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
  , emitBlobOpt :: Bool
  , cacheOpt :: Bool
  , batchOpt :: Bool
  }

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions
    { fontPathOpt = Nothing
    , pixelSizeOpt = Nothing
    , rangeOpt = Nothing
    , paddingOpt = Nothing
    , lazyAtlasOpt = Nothing
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
    , emitBlobOpt = False
    , cacheOpt = False
    , batchOpt = False
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
    , "             [--conflict X] [--flatness X] [--lazy-atlas]"
    , "             [--sign-mode winding|scanline] [--fill-rule nonzero|odd|positive|negative]"
    , "             [--overlap | --no-overlap] [--overlap-epsilon X]"
    , "             [--coloring simple|inktrap|distance] [--pseudo-distance | --no-pseudo-distance]"
    , "             [--no-pack] [--emit-blob] [--cache] [--batch]"
    , ""
    , "Examples:"
    , "  msdf-sdl-gen --font /path/to/font.ttf"
    , "  msdf-sdl-gen --range 16 --padding 24 --correction 0.08 --edge-threshold 1.0"
    ]

data MetaInfo = MetaInfo
  { metaAtlasW :: Int
  , metaAtlasH :: Int
  , metaPixelSize :: Int
  , metaScreenW :: Int
  , metaScreenH :: Int
  , metaPxRange :: Double
  , metaVertexCount :: Int
  }

data OutputData = OutputData
  { outLabel :: String
  , outFormatFlag :: Float
  , outMeta :: MetaInfo
  , outAtlasBytes :: BS.ByteString
  , outVertexBytes :: BS.ByteString
  }

data OutputSpec = OutputSpec
  { specLabel :: String
  , specFormat :: BitmapFormat
  , specCenterX :: Double
  , specCenterY :: Double
  , specWriteDefault :: Bool
  , specVars :: [(String, Double)]
  , specText :: String
  , specFontPath :: FilePath
  , specTTF :: TTF
  }

{-# NOINLINE emitBlobRef #-}
emitBlobRef :: IORef Bool
emitBlobRef = unsafePerformIO (newIORef False)

setEmitBlob :: Bool -> IO ()
setEmitBlob = writeIORef emitBlobRef

logMsg :: String -> IO ()
logMsg msg = do
  emit <- readIORef emitBlobRef
  if emit
    then hPutStrLn stderr msg
    else putStrLn msg

type AtlasCache = IORef (Map.Map String MSDFAtlas)

{-# NOINLINE atlasCacheRef #-}
atlasCacheRef :: AtlasCache
atlasCacheRef = unsafePerformIO (newIORef Map.empty)

getCachedAtlas :: Bool -> String -> MSDF.MSDFConfig -> TTF -> IO MSDFAtlas
getCachedAtlas useCache key cfg ttf =
  if not useCache
    then pure (generateMSDFFromTTF cfg ttf)
    else do
      cache <- readIORef atlasCacheRef
      case Map.lookup key cache of
        Just atlas -> pure atlas
        Nothing -> do
          let atlas = generateMSDFFromTTF cfg ttf
          writeIORef atlasCacheRef (Map.insert key atlas cache)
          pure atlas

atlasCacheKey :: FilePath -> MSDF.MSDFConfig -> String
atlasCacheKey fontKey cfg =
  let vars = normalizeVars cfg.variations
      glyphKey = glyphSetKey cfg.glyphSet
      atlasCfg = cfg.atlas
      outlineCfg = cfg.outline
      coloringCfg = cfg.coloring
      distCfg = cfg.distance
      corrCfg = cfg.correction
  in unlines
      [ "font=" <> fontKey
      , "pixelSize=" <> show cfg.pixelSize
      , "range=" <> show cfg.range
      , "output=" <> show cfg.outputFormat
      , "vars=" <> show vars
      , "glyphs=" <> glyphKey
      , "pack=" <> show atlasCfg.packAtlas
      , "pad=" <> show atlasCfg.atlasPadding
      , "minSize=" <> show atlasCfg.atlasMinSize
      , "maxSize=" <> show atlasCfg.atlasMaxSize
      , "pow2=" <> show atlasCfg.atlasPowerOfTwo
      , "buildImage=" <> show atlasCfg.buildAtlasImage
      , "flatness=" <> show outlineCfg.windingFlatness
      , "eps=" <> show outlineCfg.contourEpsilon
      , "normalize=" <> show outlineCfg.normalizeOrientation
      , "split=" <> show outlineCfg.splitIntersections
      , "cornerAngle=" <> show coloringCfg.cornerAngleDeg
      , "minSegments=" <> show coloringCfg.minSegments
      , "conflictDist=" <> show coloringCfg.conflictDistance
      , "colorSeed=" <> show coloringCfg.coloringSeed
      , "strategy=" <> show coloringCfg.strategy
      , "pseudo=" <> show distCfg.pseudoDistance
      , "grid=" <> show distCfg.gridCellSize
      , "signEps=" <> show distCfg.signEpsilon
      , "fill=" <> show distCfg.fillRule
      , "sign=" <> show distCfg.signMode
      , "overlap=" <> show distCfg.overlapSupport
      , "overlapEps=" <> show distCfg.overlapEpsilon
      , "corr=" <> show corrCfg.enableCorrection
      , "corrChan=" <> show corrCfg.channelThreshold
      , "corrEdge=" <> show corrCfg.edgeThreshold
      , "corrHard=" <> show corrCfg.hardThreshold
      ]

normalizeVars :: [(String, Double)] -> [(String, Double)]
normalizeVars = uniqueVars . sortOn fst
  where
    uniqueVars [] = []
    uniqueVars (x:xs) = x : uniqueVars (dropWhile (\y -> fst y == fst x) xs)

glyphSetKey :: GlyphSet -> String
glyphSetKey gs =
  case gs of
    GlyphSetNone -> "none"
    GlyphSetAll -> "all"
    GlyphSetCodepoints cps -> "codepoints:" <> show (uniqueSortedInts cps)

uniqueSortedInts :: [Int] -> [Int]
uniqueSortedInts xs =
  case sortOn id xs of
    [] -> []
    (y:ys) -> y : go y ys
  where
    go _ [] = []
    go prev (z:zs)
      | z == prev = go prev zs
      | otherwise = z : go z zs

main :: IO ()
main = do
  baseDir <- resolveBaseDir
  let shaderDir = baseDir </> shaderDirName
      outDir = baseDir </> outDirName

  sampleText <- resolveSampleText
  variations <- resolveVariations
  lazyAtlasEnv <- resolveLazyAtlas
  textDebug <- resolveTextDebug
  fastEnv <- resolveFastMode
  fastVariationsEnv <- resolveFastVariations
  pairWeights <- resolvePairWeights
  pairOpsz <- resolvePairOpsz
  pixelSizeEnv <- resolvePixelSizeOverride
  rangeEnv <- resolveRangeOverride
  cacheEnv <- resolveCacheMode
  batchEnv <- resolveBatchMode
  demoVariants <- resolveDemoVariants
  args <- getArgs
  opts <- case parseArgs args defaultCliOptions of
    ParseHelp -> do
      logMsg usage
      exitSuccess
    CliParseError msg -> do
      logMsg msg
      logMsg usage
      exitFailure
    ParseOk parsed -> pure parsed
  emitBlobEnv <- resolveEmitBlob
  let emitBlob = emitBlobEnv || opts.emitBlobOpt
  setEmitBlob emitBlob
  when emitBlob $ do
    hSetBinaryMode stdout True
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr LineBuffering
  when (not emitBlob) $
    createDirectoryIfMissing True outDir
  vertSpv <- compileWesl (not emitBlob) outDir (shaderDir </> "msdf.vert.wesl")
  fragSpv <- compileWesl (not emitBlob) outDir (shaderDir </> "msdf.frag.wesl")

  let cacheEnabled = cacheEnv || opts.cacheOpt
      batchEnabled = batchEnv || opts.batchOpt
      lazyAtlas = if cacheEnabled || batchEnabled
                  then False
                  else fromMaybe lazyAtlasEnv opts.lazyAtlasOpt
      hasVariations = (not (null variations) || isJust pairWeights)
      autoFast = False
      fastEnabled = fastEnv || (fastVariationsEnv && hasVariations) || autoFast
      basePixelSize = 256
      baseRange = 12
      pixelSizeBase =
        case opts.pixelSizeOpt of
          Just _ -> basePixelSize
          Nothing ->
            case pixelSizeEnv of
              Just v -> v
              Nothing -> if fastEnabled then min 128 basePixelSize else basePixelSize
      rangeBase =
        case opts.rangeOpt of
          Just _ -> baseRange
          Nothing ->
            case rangeEnv of
              Just v -> v
              Nothing -> if fastEnabled then min 8 baseRange else baseRange
      cfg0 = MSDF.defaultMSDFConfig
      cfgBase = cfg0
        { MSDF.pixelSize = pixelSizeBase
        , MSDF.range = rangeBase
        , MSDF.atlas = cfg0.atlas { MSDF.atlasPadding = 16, MSDF.packAtlas = True }
        , MSDF.correction = cfg0.correction { MSDF.channelThreshold = 0.1, MSDF.edgeThreshold = 1.0, MSDF.hardThreshold = 0.05 }
        , MSDF.outline = cfg0.outline { MSDF.windingFlatness = 0.02 }
        , MSDF.distance = cfg0.distance { MSDF.signMode = MSDF.SignScanline }
        , MSDF.coloring = cfg0.coloring { MSDF.conflictDistance = 1.0 }
        , MSDF.variations = variations
        , MSDF.glyphSet = GlyphSetCodepoints (map ord sampleText)
        }
      cfgFast =
        if fastEnabled
        then cfgBase
          { MSDF.outline = cfgBase.outline
              { MSDF.splitIntersections = False
              , MSDF.windingFlatness = max cfgBase.outline.windingFlatness 0.08
              }
          , MSDF.distance = cfgBase.distance
              { MSDF.overlapSupport = False
              }
          , MSDF.coloring = cfgBase.coloring
              { MSDF.strategy = MSDF.ColoringSimple
              , MSDF.conflictDistance = 0.5
              }
          }
        else cfgBase
      cfg = applyCliOverrides opts cfgFast
  when fastEnabled $
    logMsg "text debug: fast mode enabled (set SDL_MSDF_FAST=0 and SDL_MSDF_FAST_VARIATIONS=0 for full quality)"
  when batchEnabled $
    logMsg "text debug: batch mode enabled (lazy atlas disabled)"
  when cacheEnabled $
    logMsg "text debug: atlas cache enabled (keyed by font+config+glyph set)"
  when demoVariants $
    logMsg "text debug: demo variants enabled (regular/bold/italic/bolditalic)"
  fontPath <- resolveFontPath baseDir opts
  italicFontPath <- if demoVariants
    then resolveItalicFontPath baseDir fontPath
    else pure fontPath
  parsed <- parseTTF fontPath
  case parsed of
    Left err -> do
      let ParseError { context = ctx, message = msg } = err
      logMsg ("parseTTF failed: " <> ctx <> ": " <> msg)
      exitFailure
    Right ttf -> do
      italicTtf <-
        if italicFontPath == fontPath
        then pure ttf
        else do
          parsedItalic <- parseTTF italicFontPath
          case parsedItalic of
            Left err -> do
              let ParseError { context = ctx, message = msg } = err
              logMsg ("parseTTF failed: " <> ctx <> ": " <> msg)
              exitFailure
            Right ttfItalic -> pure ttfItalic
      let baseVars = maybe [] (\o -> [("opsz", o)]) pairOpsz
          (wRegular, wBold) = fromMaybe (400, 900) pairWeights
          outputs =
            if demoVariants
            then
              [ OutputSpec "regular" BitmapMTSDF (screenW * 0.5) (screenH * 0.78) False (("wght", wRegular) : baseVars)
                  "Regular: The quick brown fox jumps over the lazy dog."
                  fontPath ttf
              , OutputSpec "bold" BitmapMTSDF (screenW * 0.5) (screenH * 0.60) False (("wght", wBold) : baseVars)
                  "Bold: Sphinx of black quartz, judge my vow."
                  fontPath ttf
              , OutputSpec "italic" BitmapMTSDF (screenW * 0.5) (screenH * 0.42) False (("wght", wRegular) : baseVars)
                  "Italic: Pack my box with five dozen liquor jugs."
                  italicFontPath italicTtf
              , OutputSpec "bolditalic" BitmapMTSDF (screenW * 0.5) (screenH * 0.24) False (("wght", wBold) : baseVars)
                  "Bold Italic: How vexingly quick daft zebras jump."
                  italicFontPath italicTtf
              ]
            else
              case pairWeights of
                Just (w0, w1) ->
                  let vars0 = ("wght", w0) : baseVars
                      vars1 = ("wght", w1) : baseVars
                  in [ OutputSpec "w400" BitmapMTSDF (screenW * 0.33) (screenH * 0.5) False vars0 sampleText fontPath ttf
                     , OutputSpec "w900" BitmapMTSDF (screenW * 0.67) (screenH * 0.5) False vars1 sampleText fontPath ttf
                     ]
                Nothing ->
                  [ OutputSpec "mtsdf" BitmapMTSDF (screenW * 0.5) (screenH * 0.5) False variations sampleText fontPath ttf
                  ]
      if emitBlob
        then do
          outs <- mapM (buildAtlas cfg lazyAtlas textDebug cacheEnabled) outputs
          emitBlobOutputs vertSpv fragSpv outs
        else mapM_ (writeAtlas outDir cfg lazyAtlas textDebug cacheEnabled) outputs

resolveSampleText :: IO String
resolveSampleText = do
  val <- lookupEnv "SDL_MSDF_SAMPLE_TEXT"
  case fmap (map toLower) val of
    Nothing -> pure defaultSampleText
    Just "pangram" -> pure pangramText
    Just "" -> pure defaultSampleText
    Just other -> pure other

resolveVariations :: IO [(String, Double)]
resolveVariations = do
  val <- lookupEnv "SDL_MSDF_VARIATIONS"
  case val of
    Nothing -> pure []
    Just raw ->
      let parts = splitOn ',' raw
          parsed = mapMaybe parseAxis parts
      in pure parsed

resolveLazyAtlas :: IO Bool
resolveLazyAtlas = do
  val <- lookupEnv "SDL_MSDF_LAZY_ATLAS"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveFastMode :: IO Bool
resolveFastMode = do
  val <- lookupEnv "SDL_MSDF_FAST"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveFastVariations :: IO Bool
resolveFastVariations = do
  val <- lookupEnv "SDL_MSDF_FAST_VARIATIONS"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolvePixelSizeOverride :: IO (Maybe Int)
resolvePixelSizeOverride = do
  val <- lookupEnv "SDL_MSDF_PIXEL_SIZE"
  pure (val >>= readMaybe . trim)

resolveRangeOverride :: IO (Maybe Int)
resolveRangeOverride = do
  val <- lookupEnv "SDL_MSDF_RANGE"
  pure (val >>= readMaybe . trim)

resolveCacheMode :: IO Bool
resolveCacheMode = do
  val <- lookupEnv "SDL_MSDF_CACHE"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveBatchMode :: IO Bool
resolveBatchMode = do
  val <- lookupEnv "SDL_MSDF_BATCH"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveDemoVariants :: IO Bool
resolveDemoVariants = do
  val <- lookupEnv "SDL_MSDF_DEMO_VARIANTS"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveEmitBlob :: IO Bool
resolveEmitBlob = do
  val <- lookupEnv "SDL_MSDF_IN_MEMORY"
  val2 <- lookupEnv "SDL_MSDF_EMIT_BLOB"
  let norm v = fmap (map toLower) v
      on v = case v of
        Just "1" -> True
        Just "true" -> True
        Just "yes" -> True
        Just "on" -> True
        _ -> False
  pure (on (norm val) || on (norm val2))

resolveTextDebug :: IO Bool
resolveTextDebug = do
  val <- lookupEnv "SDL_MSDF_TEXT_DEBUG"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s =
  let (chunk, rest) = break (== delim) s
      rest' = case rest of
        [] -> []
        (_:xs) -> xs
  in chunk : splitOn delim rest'

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

showHex16 :: Word16 -> String
showHex16 w =
  let hex = showHex w ""
      pad = replicate (4 - length hex) '0'
  in pad <> hex

showHex8 :: Word8 -> String
showHex8 w =
  let hex = showHex w ""
      pad = replicate (2 - length hex) '0'
  in pad <> hex

parseAxis :: String -> Maybe (String, Double)
parseAxis s =
  case break (== '=') s of
    (lhs, '=':rhs) ->
      let key = trim lhs
          val = trim rhs
      in case readMaybe val of
           Just v | not (null key) -> Just (key, v)
           _ -> Nothing
    _ -> Nothing

resolvePairWeights :: IO (Maybe (Double, Double))
resolvePairWeights = do
  env <- lookupEnv "SDL_MSDF_VARIATION_PAIR"
  env2 <- lookupEnv "SDL_MSDF_PAIR_WEIGHTS"
  let raw = case env of
        Just s | not (null s) -> Just s
        _ -> case env2 of
          Just s | not (null s) -> Just s
          _ -> Nothing
  case raw of
    Nothing -> pure Nothing
    Just s ->
      let cleaned = case stripPrefix "wght=" (trim s) of
            Just rest -> rest
            Nothing -> s
          parts = map trim (splitOn ',' cleaned)
          nums = mapMaybe readMaybe parts
      in case nums of
           (a:b:_) -> pure (Just (a, b))
           _ -> pure (Just (400, 900))

resolvePairOpsz :: IO (Maybe Double)
resolvePairOpsz = do
  env <- lookupEnv "SDL_MSDF_PAIR_OPSZ"
  case env of
    Nothing -> pure Nothing
    Just s ->
      case readMaybe (trim s) of
        Just v -> pure (Just v)
        Nothing -> pure Nothing

stripPrefix :: String -> String -> Maybe String
stripPrefix [] xs = Just xs
stripPrefix _ [] = Nothing
stripPrefix (p:ps) (x:xs)
  | p == x = stripPrefix ps xs
  | otherwise = Nothing

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
          logMsg "could not locate shaders; run from repo root or examples/sdl_gpu_wesl"
          exitFailure

resolveFontPath :: FilePath -> CliOptions -> IO FilePath
resolveFontPath baseDir opts = do
  let defaultPath = baseDir </> ".." </> ".." </> "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
      path = fromMaybe defaultPath opts.fontPathOpt
  exists <- doesFileExist path
  if exists
    then pure path
    else do
      logMsg ("font not found: " <> path)
      logMsg "pass a font path: msdf-sdl-gen --font /path/to/font.ttf"
      exitFailure

resolveItalicFontPath :: FilePath -> FilePath -> IO FilePath
resolveItalicFontPath baseDir fontPath = do
  env <- lookupEnv "SDL_MSDF_ITALIC_FONT"
  let defaultPath = baseDir </> ".." </> ".." </> "assets/Inter/Inter-Italic-VariableFont_opsz,wght.ttf"
      path = case env of
        Just p | not (null p) -> p
        _ -> defaultPath
  exists <- doesFileExist path
  if exists
    then pure path
    else do
      logMsg ("italic font not found: " <> path <> " (using base font)")
      pure fontPath

compileWesl :: Bool -> FilePath -> FilePath -> IO BS.ByteString
compileWesl writeOut outDir src = do
  result <- compile [] (sourceFile src)
  case result of
    Left err -> do
      logMsg (renderCompileError err)
      exitFailure
    Right bundle -> do
      let outPath = outDir </> (takeBaseName src <.> "spv")
          bytes = shaderSpirv bundle
      when writeOut $ do
        BS.writeFile outPath bytes
        logMsg ("wrote " <> outPath)
      pure bytes

formatFlagFor :: BitmapFormat -> Float
formatFlagFor fmt =
  case fmt of
    BitmapMSDF -> 0
    BitmapMTSDF -> 1

buildAtlas :: MSDF.MSDFConfig -> Bool -> Bool -> Bool -> OutputSpec -> IO OutputData
buildAtlas cfgBase lazyAtlas textDebug cacheEnabled spec = do
  let label = spec.specLabel
      fmt = spec.specFormat
      centerX = spec.specCenterX
      centerY = spec.specCenterY
      vars = spec.specVars
      sampleText = spec.specText
      fontKey = spec.specFontPath
      ttf = spec.specTTF
  when textDebug $ do
    let glyphCount = IntMap.size (buildGlyphMap ttf sampleText)
    logMsg ("text debug: label=" <> label <> " lazy=" <> show lazyAtlas <> " glyphs=" <> show glyphCount <> " vars=" <> show vars)
  debugEnv <- lookupEnv "SDL_MSDF_VARIATION_DEBUG"
  when (isJust debugEnv) $ debugVariations label ttf vars sampleText
  let cfg = cfgBase
        { MSDF.outputFormat = fmt
        , MSDF.variations = vars
        , MSDF.glyphSet = GlyphSetCodepoints (map ord sampleText)
        }
  if lazyAtlas
    then buildAtlasLazy sampleText cfg ttf label centerX centerY textDebug
    else do
      let cacheKey = atlasCacheKey fontKey cfg
      atlas <- getCachedAtlas cacheEnabled cacheKey cfg ttf
      case atlas of
        MSDFAtlas { atlas = Nothing, glyphs = glyphArr } -> do
          let hasBitmap = any (\g -> g.bitmap.width > 0 && g.bitmap.height > 0) (elems glyphArr)
          if hasBitmap
            then do
              logMsg "atlas packing failed (packAtlas was true)"
              exitFailure
            else do
              logMsg ("atlas empty (no glyph bitmaps); skipping output " <> label)
              let meta = MetaInfo
                    { metaAtlasW = 0
                    , metaAtlasH = 0
                    , metaPixelSize = 0
                    , metaScreenW = round screenW
                    , metaScreenH = round screenH
                    , metaPxRange = 0
                    , metaVertexCount = 0
                    }
              pure OutputData
                { outLabel = label
                , outFormatFlag = formatFlagFor fmt
                , outMeta = meta
                , outAtlasBytes = BS.empty
                , outVertexBytes = BS.empty
                }
        MSDFAtlas { atlas = Just img, pixelSize = ps } -> do
          let AtlasImage { width = aw, height = ah, format = fmtImg, pixels = px } = img
              rgbaBytes = bitmapToRgbaBytes fmtImg px
              pxRange = pixelRangeForAtlas atlas (fromIntegral ps :: Double)
              bounds = textBounds atlas sampleText
              penStart' = snapPen (penForCenter (centerX, centerY) bounds)
              texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
              verts = buildTextVertices atlas (screenW, screenH) penStart' texel sampleText
              vtxBytes = BL.toStrict (toLazyByteString (floatListBuilder verts))
              meta = MetaInfo
                { metaAtlasW = aw
                , metaAtlasH = ah
                , metaPixelSize = ps
                , metaScreenW = round screenW
                , metaScreenH = round screenH
                , metaPxRange = pxRange
                , metaVertexCount = length verts `div` 4
                }
          pure OutputData
                { outLabel = label
                , outFormatFlag = formatFlagFor fmt
                , outMeta = meta
                , outAtlasBytes = rgbaBytes
                , outVertexBytes = vtxBytes
                }

buildAtlasLazy :: String -> MSDF.MSDFConfig -> TTF -> String -> Double -> Double -> Bool -> IO OutputData
buildAtlasLazy sampleText cfg ttf label centerX centerY textDebug = do
  let AtlasConfig { atlasMaxSize = maxSize, atlasPadding = pad } = cfg.atlas
      lazyCfg = (defaultLazyAtlasConfig cfg)
        { atlasWidth = max 1 maxSize
        , atlasHeight = max 1 maxSize
        , atlasPadding = pad
        , debugEnabled = textDebug
        }
  lazy <- newLazyAtlas cfg ttf lazyCfg
  let glyphMap = buildGlyphMap ttf sampleText
  glyphsMap <-
    foldM
      (ensureLazyGlyph cfg.outputFormat lazyCfg.atlasWidth lazyCfg.atlasHeight lazyCfg.atlasPadding lazy)
      IntMap.empty
      (IntMap.elems glyphMap)
  atlasImg <- snapshotAtlasImage lazy
  let AtlasImage { width = aw, height = ah, format = fmtImg, pixels = px } = atlasImg
      rgbaBytes = bitmapToRgbaBytes fmtImg px
      pxRange = fromIntegral cfg.range
      bounds = textBoundsLazy glyphsMap sampleText ttf
      penStart' = snapPen (penForCenter (centerX, centerY) bounds)
      texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
      verts = buildTextVerticesLazy glyphsMap (screenW, screenH) penStart' texel sampleText ttf
      vtxBytes = BL.toStrict (toLazyByteString (floatListBuilder verts))
      meta = MetaInfo
        { metaAtlasW = aw
        , metaAtlasH = ah
        , metaPixelSize = cfg.pixelSize
        , metaScreenW = round screenW
        , metaScreenH = round screenH
        , metaPxRange = pxRange
        , metaVertexCount = length verts `div` 4
        }
  pure OutputData
    { outLabel = label
    , outFormatFlag = formatFlagFor cfg.outputFormat
    , outMeta = meta
    , outAtlasBytes = rgbaBytes
    , outVertexBytes = vtxBytes
    }

writeAtlas :: FilePath -> MSDF.MSDFConfig -> Bool -> Bool -> Bool -> OutputSpec -> IO ()
writeAtlas outDir cfgBase lazyAtlas textDebug cacheEnabled spec = do
  output <- buildAtlas cfgBase lazyAtlas textDebug cacheEnabled spec
  let emptyOutput = BS.null output.outAtlasBytes
                || BS.null output.outVertexBytes
                || output.outMeta.metaAtlasW <= 0
                || output.outMeta.metaAtlasH <= 0
  if emptyOutput
    then logMsg ("skip write: empty output " <> output.outLabel)
    else do
      let verticesOut = outDir </> ("vertices_" <> output.outLabel <.> "bin")
          atlasOut = outDir </> ("atlas_" <> output.outLabel <.> "rgba")
          metaOut = outDir </> ("meta_" <> output.outLabel <.> "txt")
      writeOutputs atlasOut verticesOut metaOut output
      when spec.specWriteDefault $ do
        let verticesOut' = outDir </> "vertices.bin"
            atlasOut' = outDir </> "atlas.rgba"
            metaOut' = outDir </> "meta.txt"
        writeOutputs atlasOut' verticesOut' metaOut' output
      logMsg ("wrote " <> atlasOut)
      logMsg ("wrote " <> verticesOut)
      logMsg ("wrote " <> metaOut)

buildGlyphMap :: TTF -> String -> IntMap.IntMap Int
buildGlyphMap ttf text =
  let Cmap maps = ttf.cmap
      mapList = maps
      lookupGlyph cp = case lookup cp mapList of
        Just gi -> Just gi
        Nothing -> Nothing
      add acc ch = case lookupGlyph (ord ch) of
        Just gi -> IntMap.insert gi gi acc
        Nothing -> acc
  in foldl' add IntMap.empty text

ensureLazyGlyph :: BitmapFormat -> Int -> Int -> Int -> LazyAtlas -> IntMap.IntMap GlyphMSDF -> Int -> IO (IntMap.IntMap GlyphMSDF)
ensureLazyGlyph outFmt aw ah pad lazy m glyphIndex =
  case IntMap.lookup glyphIndex m of
    Just _ -> pure m
    Nothing -> do
      (glyph, _) <- ensureGlyph lazy glyphIndex
      case glyph.placement of
        Nothing -> do
          let MSDFBitmap { width = bw, height = bh, format = fmt } = glyph.bitmap
              reason
                | bw <= 0 || bh <= 0 = "empty bitmap"
                | fmt /= outFmt = "format mismatch (glyph=" <> show fmt <> ", atlas=" <> show outFmt <> ")"
                | bw + 2 * pad > aw || bh + 2 * pad > ah =
                    "too large (need " <> show (bw + 2 * pad) <> "x" <> show (bh + 2 * pad)
                      <> " atlas " <> show aw <> "x" <> show ah <> ")"
                | otherwise = "no space (atlas full?)"
          logMsg ("lazy atlas: skip glyph " <> show glyphIndex <> " (" <> reason <> ")")
          pure m
        Just _ -> pure (IntMap.insert glyphIndex glyph m)

buildTextVerticesLazy :: IntMap.IntMap GlyphMSDF -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> TTF -> [Float]
buildTextVerticesLazy glyphs screen (penX0, penY) texel text ttf =
  let step (acc, penX) ch =
        case lookupCodepointLazy ttf ch of
          Nothing -> (acc, penX)
          Just gi ->
            case IntMap.lookup gi glyphs of
              Nothing -> (acc, penX)
              Just glyph ->
                let GlyphMSDF { advance = adv } = glyph
                    quad = quadVerts screen (penX, penY) texel glyph
                    penX' = penX + adv
                in (quad : acc, penX')
      (chunks, _) = foldl' step ([], penX0) text
  in concat (reverse chunks)

textBoundsLazy :: IntMap.IntMap GlyphMSDF -> String -> TTF -> (Double, Double, Double, Double)
textBoundsLazy glyphs text ttf =
  let step (penX, acc) ch =
        case lookupCodepointLazy ttf ch of
          Nothing -> (penX, acc)
          Just gi ->
            case IntMap.lookup gi glyphs of
              Nothing -> (penX, acc)
              Just glyph ->
                let GlyphMSDF { advance = adv } = glyph
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

lookupCodepointLazy :: TTF -> Char -> Maybe Int
lookupCodepointLazy ttf ch =
  let Cmap maps = ttf.cmap
  in lookup (ord ch) maps

debugVariations :: String -> TTF -> [(String, Double)] -> String -> IO ()
debugVariations label ttf vars sampleText =
  case ttf of
    TTF { variations = Nothing } ->
      logMsg ("variations[" <> label <> "]: none")
    TTF { variations = Just v } -> do
      let Variations { fvar = fvar', avar = avar', gvar = gvar' } = v
          gvarStatus = case gvar' of
            Nothing -> "none"
            Just gv ->
              let Gvar { axisCount = axisCount', flags = flags', dataOffset = dataOff, buffer = ByteBuffer { len = bbLen }, sharedTuples = tuples } = gv
                  longOffsets = (flags' .&. 0x0001) /= 0
              in "axisCount=" <> show axisCount'
                 <> " flags=0x" <> showHex16 flags'
                 <> " longOffsets=" <> show longOffsets
                 <> " dataOffset=" <> show dataOff
                 <> " len=" <> show bbLen
                 <> " sharedTuples=" <> show (length tuples)
          Fvar { axes = axes } = fvar'
      logMsg ("variations[" <> label <> "]: " <> show vars)
      logMsg ("gvar[" <> label <> "]: " <> gvarStatus)
      logMsg ("axes[" <> label <> "]:")
      forM_ axes $ \ax ->
        let FvarAxis { tag = tag', minValue = minV, defaultValue = defV, maxValue = maxV } = ax
        in logMsg ("  " <> tag' <> " min=" <> show minV <> " def=" <> show defV <> " max=" <> show maxV)
      let loc = normalizeLocation fvar' avar' vars
      logMsg ("location[" <> label <> "]: " <> show loc)
      case sampleText of
        [] -> pure ()
        (ch:_) ->
          case ttf of
            TTF { cmap = Cmap maps } ->
              case lookup (ord ch) maps of
                Nothing -> logMsg ("glyph[" <> label <> "]: missing for " <> show ch)
                Just gi -> do
                  let (x0, y0, x1, y1) = glyphBBoxRawAt Nothing ttf gi
                      (vx0, vy0, vx1, vy1) = glyphBBoxRawAt (Just loc) ttf gi
                  logMsg ("bbox[" <> label <> "]: base=" <> show (x0, y0, x1, y1) <> " var=" <> show (vx0, vy0, vx1, vy1))
                  let baseContours = glyphOutlineAt Nothing ttf gi
                      varContours = glyphOutlineAt (Just loc) ttf gi
                      delta = contourDeltaSum baseContours varContours
                      pointCount = sum (map length baseContours) + 4
                  logMsg ("deltaSum[" <> label <> "]: " <> show delta)
                  case gvar' of
                    Nothing -> pure ()
                    Just gv ->
                      let Gvar { offsets = offs, dataOffset = dataOff, buffer = bb } = gv
                          (lo, hi) = bounds offs
                      in if gi < lo || gi + 1 > hi
                         then logMsg ("gvarOffsets[" <> label <> "]: out of range")
                         else
                           let o0 = offs ! gi
                               o1 = offs ! (gi + 1)
                               oFirst = if lo <= hi then offs ! lo else 0
                               oSecond = if lo + 1 <= hi then offs ! (lo + 1) else 0
                               dataStart = dataOff + o0
                               raw = [ readU8 bb (dataStart + i) | i <- [0 .. 47] ]
                               rawHex = unwords (map showHex8 raw)
                           in logMsg ("gvarOffsets[" <> label <> "]: " <> show o0 <> " -> " <> show o1
                                        <> " first=" <> show oFirst
                                        <> " second=" <> show oSecond)
                               >> logMsg ("gvarBytes[" <> label <> "]: " <> rawHex)
                  case gvar' of
                    Nothing -> pure ()
                    Just gv ->
                      logMsg ("gvarDeltaSum[" <> label <> "]: " <> show (gvarDeltaSum gv loc gi pointCount))
                  case gvar' of
                    Nothing -> pure ()
                    Just gv ->
                      case gvarTupleScalars gv loc gi of
                        Nothing -> logMsg ("gvarTuples[" <> label <> "]: none")
                        Just (tupleCount, dataLen, scalars) ->
                          let nonZero = length (filter (/= 0) scalars)
                              minS = if null scalars then 0 else minimum scalars
                              maxS = if null scalars then 0 else maximum scalars
                          in logMsg ("gvarTuples[" <> label <> "]: count=" <> show tupleCount
                                       <> " dataLen=" <> show dataLen
                                       <> " nonZero=" <> show nonZero
                                       <> " min=" <> show minS
                                       <> " max=" <> show maxS)
                  case gvar' of
                    Nothing -> pure ()
                    Just gv ->
                      case gvarTupleHeaderStats gv gi of
                        Nothing -> logMsg ("gvarHeaders[" <> label <> "]: none")
                        Just (dataLen, dataOff, pairs) ->
                          let offs = map fst pairs
                              sizes = map snd pairs
                              minOff = if null offs then 0 else minimum offs
                              maxOff = if null offs then 0 else maximum offs
                              minSize = if null sizes then 0 else minimum sizes
                              maxSize = if null sizes then 0 else maximum sizes
                              maxEnd = if null pairs then 0 else maximum (map (\(o, s) -> o + s) pairs)
                          in logMsg ("gvarHeaders[" <> label <> "]: dataLen=" <> show dataLen
                                       <> " dataOff=" <> show dataOff
                                       <> " minOff=" <> show minOff
                                       <> " maxOff=" <> show maxOff
                                       <> " minSize=" <> show minSize
                                       <> " maxSize=" <> show maxSize
                                       <> " maxEnd=" <> show maxEnd)
                  case gvar' of
                    Nothing -> pure ()
                    Just gv ->
                      case gvarTupleDeltaStats gv loc gi pointCount of
                        Nothing -> logMsg ("gvarDeltas[" <> label <> "]: none")
                        Just stats ->
                          let fmt (idx, (scalar, minDx, maxDx, minDy, maxDy)) =
                                "  #" <> show idx <> " scalar=" <> show scalar
                                  <> " dx=[" <> show minDx <> "," <> show maxDx <> "]"
                                  <> " dy=[" <> show minDy <> "," <> show maxDy <> "]"
                              lines' = zipWith (\i s -> fmt (i, s)) [0..] stats
                          in do
                            logMsg ("gvarDeltas[" <> label <> "]:")
                            mapM_ logMsg lines'

contourDeltaSum :: [[Point]] -> [[Point]] -> Double
contourDeltaSum a b =
  let pairs = zip a b
      sumPts acc (pa, pb) = acc + contourDelta pa pb
  in foldl' sumPts 0 pairs

contourDelta :: [Point] -> [Point] -> Double
contourDelta a b =
  let pairs = zip a b
      sumPt acc (p1, p2) =
        let Point { x = x1, y = y1 } = p1
            Point { x = x2, y = y2 } = p2
            dx = x1 - x2
            dy = y1 - y2
        in acc + sqrt (dx * dx + dy * dy)
  in foldl' sumPt 0 pairs

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
parseArgs ("--lazy-atlas":rest) opts =
  parseArgs rest opts { lazyAtlasOpt = Just True }
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
parseArgs ("--emit-blob":rest) opts = parseArgs rest opts { emitBlobOpt = True }
parseArgs ("--cache":rest) opts = parseArgs rest opts { cacheOpt = True }
parseArgs ("--batch":rest) opts = parseArgs rest opts { batchOpt = True }
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

writeOutputs :: FilePath -> FilePath -> FilePath -> OutputData -> IO ()
writeOutputs atlasOut verticesOut metaOut output = do
  let MetaInfo { metaAtlasW = aw
               , metaAtlasH = ah
               , metaPixelSize = ps
               , metaPxRange = pxRange
               , metaScreenW = sw
               , metaScreenH = sh
               , metaVertexCount = vcount
               } = output.outMeta
  BS.writeFile atlasOut output.outAtlasBytes
  BS.writeFile verticesOut output.outVertexBytes
  writeFile metaOut $ unlines
    [ "atlasWidth " <> show aw
    , "atlasHeight " <> show ah
    , "pixelSize " <> show ps
    , "pxRange " <> show pxRange
    , "screenWidth " <> show sw
    , "screenHeight " <> show sh
    , "vertexCount " <> show vcount
    ]

emitBlobOutputs :: BS.ByteString -> BS.ByteString -> [OutputData] -> IO ()
emitBlobOutputs vertSpv fragSpv outputs =
  BL.hPut stdout (toLazyByteString (blobBuilder vertSpv fragSpv outputs))

blobBuilder :: BS.ByteString -> BS.ByteString -> [OutputData] -> Builder
blobBuilder vertSpv fragSpv outputs =
  let magic = BSC.pack "MSDFBLB1"
      header =
        byteString magic
        <> word32LE 1
        <> word32LE (fromIntegral (BS.length vertSpv))
        <> word32LE (fromIntegral (BS.length fragSpv))
        <> byteString vertSpv
        <> byteString fragSpv
        <> word32LE (fromIntegral (length outputs))
  in header <> mconcat (map outputBuilder outputs)

outputBuilder :: OutputData -> Builder
outputBuilder output =
  let labelBs = BSC.pack output.outLabel
      MetaInfo { metaAtlasW = aw
               , metaAtlasH = ah
               , metaPixelSize = ps
               , metaPxRange = pxRange
               , metaScreenW = sw
               , metaScreenH = sh
               , metaVertexCount = vcount
               } = output.outMeta
      atlasBytes = output.outAtlasBytes
      vertBytes = output.outVertexBytes
  in word32LE (fromIntegral (BS.length labelBs))
     <> byteString labelBs
     <> floatLE output.outFormatFlag
     <> word32LE (fromIntegral aw)
     <> word32LE (fromIntegral ah)
     <> word32LE (fromIntegral ps)
     <> word32LE (fromIntegral sw)
     <> word32LE (fromIntegral sh)
     <> floatLE (realToFrac pxRange)
     <> word32LE (fromIntegral vcount)
     <> word32LE (fromIntegral (BS.length atlasBytes))
     <> word32LE (fromIntegral (BS.length vertBytes))
     <> byteString atlasBytes
     <> byteString vertBytes
