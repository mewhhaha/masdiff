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
import Data.Bits ((.&.), xor)
import Data.Char (isAlphaNum, isSpace, ord, toLower)
import Data.List (foldl', intercalate, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Numeric (showFFloat, showHex)
import Control.Monad (foldM, forM_, when)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeBaseName, (<.>))
import Data.Word (Word8, Word16, Word64)
import Text.Read (readMaybe)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import System.IO (BufferMode(..), hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (GlyphSet(..), glyphMetricsOnlyAt)
import MSDF.Generated (BuildTimings(..), generateMSDFFromTTF, generateMSDFFromTTFWithTimings)
import MSDF.Render (glyphQuad, glyphUV)
import MSDF.Binary (ByteBuffer(..), readU8)
import MSDF.Outline (Point(..))
import MSDF.Types (AtlasImage(..), BitmapFormat(..), GlyphMSDF(..), MSDFAtlas(..), MSDFBitmap(..), lookupCodepoint)
import MSDF.TTF.Parser (Cmap(..), Head(..), Hhea(..), ParseError(..), glyphBBoxRawAt, glyphOutlineAt, parseTTF, TTF(..))
import MSDF.TTF.Variations (Fvar(..), FvarAxis(..), Gvar(..), Variations(..), gvarDeltaSum, gvarTupleDeltaStats, gvarTupleHeaderStats, gvarTupleScalars, isDefaultLocation, normalizeLocation)
import MSDF.LazyAtlas (LazyAtlas, LazyAtlasConfig(..), defaultLazyAtlasConfig, ensureGlyph, newLazyAtlas, snapshotAtlasImage)
import MSDF.Config (AtlasConfig(..))
import Spirdo.Wesl (compile, renderCompileError, sourceFile, shaderSpirv)

shaderDirName :: FilePath
shaderDirName = "shaders"

outDirName :: FilePath
outDirName = "out"

screenW, screenH :: Double
screenW = 1920
screenH = 1080

pangramText :: String
pangramText = "The quick brown fox jumps over the lazy dog"

defaultSampleText :: String
defaultSampleText = pangramText <> " Pack my box with five dozen liquor jugs."

defaultDemoTexts :: [String]
defaultDemoTexts =
  [ "Regular:\nThe quick brown fox jumps over the lazy dog."
  , "Bold:\nSphinx of black quartz, judge my vow."
  , "Italic:\nCozy lummox gives smart squid who asks for job pen."
  , "Bold Italic:\nVexing wizards quickly jab the gnomes."
  ]
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
  , splitOpt :: Maybe Bool
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
    , splitOpt = Nothing
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
    , "             [--split-intersections | --no-split-intersections]"
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
  , specMaxWidth :: Maybe Double
  , specMaxHeight :: Maybe Double
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

type OutputCache = IORef (Map.Map String OutputData)

{-# NOINLINE outputCacheRef #-}
outputCacheRef :: OutputCache
outputCacheRef = unsafePerformIO (newIORef Map.empty)

getCachedAtlas :: Bool -> Bool -> String -> MSDF.MSDFConfig -> TTF -> IO (MSDFAtlas, Maybe BuildTimings, Bool)
getCachedAtlas useCache timingsEnabled key cfg ttf =
  if not useCache
    then do
      (atlas, timings) <- buildOnce
      pure (atlas, timings, False)
    else do
      cache <- readIORef atlasCacheRef
      case Map.lookup key cache of
        Just atlas -> pure (atlas, Nothing, True)
        Nothing -> do
          (atlas, timings) <- buildOnce
          writeIORef atlasCacheRef (Map.insert key atlas cache)
          pure (atlas, timings, False)
  where
    buildOnce =
      if timingsEnabled
        then do
          (atlas, timings) <- generateMSDFFromTTFWithTimings cfg ttf
          pure (atlas, Just timings)
        else pure (generateMSDFFromTTF cfg ttf, Nothing)

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

outputCacheKey :: FilePath -> MSDF.MSDFConfig -> Int -> String -> Double -> Double -> String
outputCacheKey fontKey cfg renderPixelSize sampleText centerX centerY =
  atlasCacheKey fontKey cfg
  <> "renderPixelSize=" <> show renderPixelSize <> "\n"
  <> "text=" <> show sampleText <> "\n"
  <> "centerX=" <> show centerX <> "\n"
  <> "centerY=" <> show centerY <> "\n"

fnv1a64 :: String -> Word64
fnv1a64 =
  let offset = 14695981039346656037 :: Word64
      prime = 1099511628211 :: Word64
      step h c = (h `xor` fromIntegral (ord c)) * prime
  in foldl' step offset

cacheKeyHash :: String -> String
cacheKeyHash key = showHex (fnv1a64 key) ""

sanitizeLabel :: String -> String
sanitizeLabel =
  let keep c
        | isAlphaNum c = c
        | c == '-' = c
        | c == '_' = c
        | otherwise = '_'
  in map keep

cachePaths :: FilePath -> String -> String -> (FilePath, FilePath, FilePath)
cachePaths dir label key =
  let base = sanitizeLabel label <> "_" <> cacheKeyHash key
  in ( dir </> (base <.> "rgba")
     , dir </> (base <.> "bin")
     , dir </> (base <.> "txt")
     )

readMetaFile :: FilePath -> IO (Maybe MetaInfo)
readMetaFile path = do
  ok <- doesFileExist path
  if not ok
    then pure Nothing
    else do
      raw <- readFile path
      let step acc line =
            case words line of
              ["atlasWidth", v] -> acc { metaAtlasW = readDef 0 v }
              ["atlasHeight", v] -> acc { metaAtlasH = readDef 0 v }
              ["pixelSize", v] -> acc { metaPixelSize = readDef 0 v }
              ["screenW", v] -> acc { metaScreenW = readDef 0 v }
              ["screenH", v] -> acc { metaScreenH = readDef 0 v }
              ["pxRange", v] -> acc { metaPxRange = readDefD 0 v }
              ["vertexCount", v] -> acc { metaVertexCount = readDef 0 v }
              _ -> acc
          readDef def v = fromMaybe def (readMaybe v)
          readDefD def v = fromMaybe def (readMaybe v)
          base = MetaInfo 0 0 0 0 0 0 0
          meta = foldl' step base (lines raw)
      if meta.metaAtlasW > 0 && meta.metaAtlasH > 0
        then pure (Just meta)
        else pure Nothing

readOutputCache :: FilePath -> String -> String -> BitmapFormat -> IO (Maybe OutputData)
readOutputCache dir label key fmt = do
  let (atlasPath, vertPath, metaPath) = cachePaths dir label key
  metaMb <- readMetaFile metaPath
  case metaMb of
    Nothing -> pure Nothing
    Just meta -> do
      atlasOk <- doesFileExist atlasPath
      vertOk <- doesFileExist vertPath
      if not (atlasOk && vertOk)
        then pure Nothing
        else do
          atlasBytes <- BS.readFile atlasPath
          vertBytes <- BS.readFile vertPath
          let expected = meta.metaAtlasW * meta.metaAtlasH * 4
              atlasSizeOk = expected <= 0 || BS.length atlasBytes == expected
              vertsOk = not (BS.null vertBytes)
          if not (atlasSizeOk && vertsOk)
            then pure Nothing
            else pure (Just OutputData
              { outLabel = label
              , outFormatFlag = formatFlagFor fmt
              , outMeta = meta
              , outAtlasBytes = atlasBytes
              , outVertexBytes = vertBytes
              })

writeOutputCache :: FilePath -> String -> String -> OutputData -> IO ()
writeOutputCache dir label key output = do
  let (atlasPath, vertPath, metaPath) = cachePaths dir label key
  writeOutputs atlasPath vertPath metaPath output

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
  textDebug <- resolveTextDebug
  fastEnv <- resolveFastMode
  fastVariationsEnv <- resolveFastVariations
  pairWeights <- resolvePairWeights
  pairOpsz <- resolvePairOpsz
  pixelSizeEnv <- resolvePixelSizeOverride
  rangeEnv <- resolveRangeOverride
  demoVariants <- resolveDemoVariants
  demoTextsRaw <- resolveDemoTexts
  timingsEnabled <- resolveTimings
  genPixelSizeEnv <- resolveGenPixelSizeOverride
  diskCacheEnabled <- resolveDiskCacheMode
  diskCacheDirEnv <- resolveDiskCacheDir
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
  let diskCacheDir =
        if diskCacheEnabled
        then Just (fromMaybe (outDir </> "cache") diskCacheDirEnv)
        else Nothing
  case diskCacheDir of
    Nothing -> pure ()
    Just dir -> createDirectoryIfMissing True dir
  vertSpv <- compileWesl (not emitBlob) outDir (shaderDir </> "msdf.vert.wesl")
  fragSpv <- compileWesl (not emitBlob) outDir (shaderDir </> "msdf.frag.wesl")

  let cacheEnabled = True
      lazyAtlas = True
      hasVariations = (not (null variations) || isJust pairWeights)
      autoFast = False
      fastEnabled = fastEnv || (fastVariationsEnv && hasVariations) || autoFast
      basePixelSize = 256
      baseRange = 16
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
        , MSDF.outline = cfg0.outline
            { MSDF.windingFlatness = 0.02
            -- Keep intersection splitting opt-in in demo defaults.
            -- Current split path can introduce small seam artifacts on some joins.
            , MSDF.splitIntersections = False
            }
        , MSDF.distance =
            cfg0.distance
              { MSDF.signMode = MSDF.SignScanline
              -- Keep overlap filtering opt-in for demo generation.
              -- Current overlap logic can introduce visible sliver artifacts.
              , MSDF.overlapSupport = False
              }
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
      renderPixelSize = max 1 cfg.pixelSize
      genPixelSize = max 1 (fromMaybe renderPixelSize genPixelSizeEnv)
      renderScale =
        if genPixelSize <= 0
        then 1
        else fromIntegral renderPixelSize / fromIntegral genPixelSize
      cfgGen =
        if genPixelSize == cfg.pixelSize
        then cfg
        else cfg { MSDF.pixelSize = genPixelSize }
  when fastEnabled $
    logMsg "text debug: fast mode enabled (set SDL_MSDF_FAST=0 and SDL_MSDF_FAST_VARIATIONS=0 for full quality)"
  when cacheEnabled $
    logMsg "text debug: lazy atlas + cache forced (quality mode)"
  when textDebug $
    when (renderPixelSize /= genPixelSize) $
      logMsg ("text debug: renderPixelSize=" <> show renderPixelSize <> " genPixelSize=" <> show genPixelSize <> " scale=" <> showFFloat (Just 3) renderScale "")
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
          demoTexts =
            case demoTextsRaw of
              [] -> replicate 4 (pangramText <> ".")
              xs -> xs
          demoTextList =
            if length demoTexts >= 4
            then take 4 demoTexts
            else take 4 (cycle demoTexts)
          [demoTextRegular, demoTextItalic, demoTextBold, demoTextBoldItalic] = demoTextList
          outputs =
            if demoVariants
            then
              let boxW = Just (screenW * 0.48)
                  boxH = Just (screenH * 0.44)
              in
              [ OutputSpec "regular" BitmapMTSDF (screenW * 0.26) (screenH * 0.72) boxW boxH False (("wght", wRegular) : baseVars)
                  demoTextRegular
                  fontPath ttf
              , OutputSpec "italic" BitmapMTSDF (screenW * 0.74) (screenH * 0.72) boxW boxH False (("wght", wRegular) : baseVars)
                  demoTextItalic
                  italicFontPath italicTtf
              , OutputSpec "bold" BitmapMTSDF (screenW * 0.26) (screenH * 0.28) boxW boxH False (("wght", wBold) : baseVars)
                  demoTextBold
                  fontPath ttf
              , OutputSpec "bolditalic" BitmapMTSDF (screenW * 0.74) (screenH * 0.28) boxW boxH False (("wght", wBold) : baseVars)
                  demoTextBoldItalic
                  italicFontPath italicTtf
              ]
            else
              case pairWeights of
                Just (w0, w1) ->
                  let vars0 = ("wght", w0) : baseVars
                      vars1 = ("wght", w1) : baseVars
                      boxW = Just (screenW * 0.44)
                      boxH = Just (screenH * 0.40)
                  in [ OutputSpec "w400" BitmapMTSDF (screenW * 0.28) (screenH * 0.72) boxW boxH False vars0 sampleText fontPath ttf
                     , OutputSpec "w900" BitmapMTSDF (screenW * 0.72) (screenH * 0.28) boxW boxH False vars1 sampleText fontPath ttf
                     ]
                Nothing ->
                  [ OutputSpec "mtsdf" BitmapMTSDF (screenW * 0.5) (screenH * 0.5) Nothing Nothing False variations sampleText fontPath ttf
                  ]
      if emitBlob
        then do
          outs <- mapM (buildAtlas cfgGen renderPixelSize diskCacheDir lazyAtlas textDebug cacheEnabled timingsEnabled) outputs
          emitBlobOutputs vertSpv fragSpv outs
        else mapM_ (writeAtlas outDir cfgGen renderPixelSize diskCacheDir lazyAtlas textDebug cacheEnabled timingsEnabled) outputs

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

resolveGenPixelSizeOverride :: IO (Maybe Int)
resolveGenPixelSizeOverride = do
  val <- lookupEnv "SDL_MSDF_GEN_PIXEL_SIZE"
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

resolveDiskCacheMode :: IO Bool
resolveDiskCacheMode = do
  val <- lookupEnv "SDL_MSDF_DISK_CACHE"
  pure (case fmap (map toLower) val of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "on" -> True
    _ -> False)

resolveDiskCacheDir :: IO (Maybe FilePath)
resolveDiskCacheDir = lookupEnv "SDL_MSDF_CACHE_DIR"

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
  case fmap (map toLower) val of
    Just "1" -> pure True
    Just "true" -> pure True
    Just "yes" -> pure True
    Just "on" -> pure True
    _ -> do
      pairA <- lookupEnv "SDL_MSDF_VARIATION_PAIR"
      pairB <- lookupEnv "SDL_MSDF_PAIR_WEIGHTS"
      let hasPair = maybe False (not . null . trim) pairA || maybe False (not . null . trim) pairB
      pure hasPair

resolveDemoTexts :: IO [String]
resolveDemoTexts = do
  val <- lookupEnv "SDL_MSDF_DEMO_TEXTS"
  case val of
    Nothing -> pure []
    Just raw ->
      let parts = map trim (splitOn '|' raw)
      in pure (filter (not . null) parts)

resolveTimings :: IO Bool
resolveTimings = do
  val <- lookupEnv "SDL_MSDF_TIMINGS"
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

buildAtlas :: MSDF.MSDFConfig -> Int -> Maybe FilePath -> Bool -> Bool -> Bool -> Bool -> OutputSpec -> IO OutputData
buildAtlas cfgBase renderPixelSize diskCacheDir lazyAtlas textDebug cacheEnabled timingsEnabled spec = do
  let label = spec.specLabel
      fmt = spec.specFormat
      centerX = spec.specCenterX
      centerY = spec.specCenterY
      maxHeight = spec.specMaxHeight
      vars = spec.specVars
      sampleText = spec.specText
      fontKey = spec.specFontPath
      ttf = spec.specTTF
      genPixelSize = max 1 cfgBase.pixelSize
      renderPx = max 1 renderPixelSize
      renderScale = fromIntegral renderPx / fromIntegral genPixelSize
  let cfgPre = cfgBase
        { MSDF.outputFormat = fmt
        , MSDF.variations = vars
        }
      wrapWidth = fromMaybe (screenW * 0.94) spec.specMaxWidth
      renderText = wrapTextForWidth wrapWidth renderScale cfgPre ttf sampleText
      cfg = cfgPre
        { MSDF.glyphSet = GlyphSetCodepoints (map ord renderText)
        }
  when textDebug $ do
    let glyphCount = IntMap.size (buildGlyphMap ttf renderText)
    logMsg ("text debug: label=" <> label <> " lazy=" <> show lazyAtlas <> " glyphs=" <> show glyphCount <> " vars=" <> show vars)
  debugEnv <- lookupEnv "SDL_MSDF_VARIATION_DEBUG"
  when (isJust debugEnv) $ debugVariations label ttf vars sampleText
  let outputKey = outputCacheKey fontKey cfg renderPx renderText centerX centerY
  cachedDisk <-
    case diskCacheDir of
      Nothing -> pure Nothing
      Just dir -> readOutputCache dir label outputKey fmt
  case cachedDisk of
    Just out -> do
      when cacheEnabled $
        modifyIORef' outputCacheRef (Map.insert outputKey out)
      pure out
    Nothing ->
      if lazyAtlas
        then do
          cachedMem <-
            if cacheEnabled
            then do
              cache <- readIORef outputCacheRef
              pure (Map.lookup outputKey cache)
            else pure Nothing
          case cachedMem of
            Just out -> pure out
            Nothing -> do
              out <- buildAtlasLazy renderScale renderText cfg ttf label centerX centerY spec.specMaxWidth maxHeight textDebug
              when cacheEnabled $
                modifyIORef' outputCacheRef (Map.insert outputKey out)
              forM_ diskCacheDir (\dir -> writeOutputCache dir label outputKey out)
              pure out
        else do
          let cacheKey = atlasCacheKey fontKey cfg
          (atlas, timings, cacheHit) <- getCachedAtlas cacheEnabled timingsEnabled cacheKey cfg ttf
          when timingsEnabled $ do
            case timings of
              Just t -> logTimings label t
              Nothing ->
                when cacheHit $
                  logMsg ("timing[" <> label <> "]: cache hit")
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
                  boundsRaw = scaleBounds renderScale (textBounds atlas renderText)
                  fitScale = fitScaleToBox wrapWidth maxHeight boundsRaw
                  bounds = scaleBounds fitScale boundsRaw
                  pxRange = fromIntegral atlas.range
                  penStart' = snapPen (penForCenter (centerX, centerY) bounds)
                  texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
                  verts = buildTextVerticesScaled (renderScale * fitScale) atlas (screenW, screenH) penStart' texel renderText
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
                  out = OutputData
                    { outLabel = label
                    , outFormatFlag = formatFlagFor fmt
                    , outMeta = meta
                    , outAtlasBytes = rgbaBytes
                    , outVertexBytes = vtxBytes
                    }
              forM_ diskCacheDir (\dir -> writeOutputCache dir label outputKey out)
              pure out

buildAtlasLazy :: Double -> String -> MSDF.MSDFConfig -> TTF -> String -> Double -> Double -> Maybe Double -> Maybe Double -> Bool -> IO OutputData
buildAtlasLazy renderScale renderText cfg ttf label centerX centerY maxWidth maxHeight textDebug = do
  let AtlasConfig { atlasMaxSize = maxSize, atlasPadding = pad } = cfg.atlas
      lazyCfg = (defaultLazyAtlasConfig cfg)
        { atlasWidth = max 1 maxSize
        , atlasHeight = max 1 maxSize
        , atlasPadding = pad
        , debugEnabled = textDebug
        }
  lazy <- newLazyAtlas cfg ttf lazyCfg
  let glyphMap = buildGlyphMap ttf renderText
  glyphsMap <-
    foldM
      (ensureLazyGlyph textDebug cfg.outputFormat lazyCfg.atlasWidth lazyCfg.atlasHeight lazyCfg.atlasPadding lazy)
      IntMap.empty
      (IntMap.elems glyphMap)
  atlasImg <- snapshotAtlasImage lazy
  let AtlasImage { width = aw, height = ah, format = fmtImg, pixels = px } = atlasImg
      rgbaBytes = bitmapToRgbaBytes fmtImg px
      boundsRaw = scaleBounds renderScale (textBoundsLazy glyphsMap renderText ttf cfg)
      wrapWidth = fromMaybe (screenW * 0.94) maxWidth
      fitScale = fitScaleToBox wrapWidth maxHeight boundsRaw
      bounds = scaleBounds fitScale boundsRaw
      pxRange = fromIntegral cfg.range
      penStart' = snapPen (penForCenter (centerX, centerY) bounds)
      texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
      verts = buildTextVerticesLazyScaled (renderScale * fitScale) glyphsMap (screenW, screenH) penStart' texel renderText ttf cfg
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

writeAtlas :: FilePath -> MSDF.MSDFConfig -> Int -> Maybe FilePath -> Bool -> Bool -> Bool -> Bool -> OutputSpec -> IO ()
writeAtlas outDir cfgBase renderPixelSize diskCacheDir lazyAtlas textDebug cacheEnabled timingsEnabled spec = do
  output <- buildAtlas cfgBase renderPixelSize diskCacheDir lazyAtlas textDebug cacheEnabled timingsEnabled spec
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

ensureLazyGlyph :: Bool -> BitmapFormat -> Int -> Int -> Int -> LazyAtlas -> IntMap.IntMap GlyphMSDF -> Int -> IO (IntMap.IntMap GlyphMSDF)
ensureLazyGlyph debugEnabled outFmt aw ah pad lazy m glyphIndex =
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
          let shouldLog = debugEnabled || reason /= "empty bitmap"
          when shouldLog $
            logMsg ("lazy atlas: skip glyph " <> show glyphIndex <> " (" <> reason <> ")")
          pure m
        Just _ -> pure (IntMap.insert glyphIndex glyph m)

buildTextVerticesLazy :: IntMap.IntMap GlyphMSDF -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> TTF -> MSDF.MSDFConfig -> [Float]
buildTextVerticesLazy glyphs screen (penX0, penY) texel text ttf cfg =
  let lineAdvance = lineAdvanceTTF cfg ttf
      spaceAdv = spaceAdvanceCfg cfg
      step (acc, penX, penY') ch
        | ch == '\n' = (acc, penX0, penY' - lineAdvance)
        | isSpace ch =
            case lookupCodepointLazy ttf ch of
              Nothing -> (acc, penX + spaceAdv, penY')
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (acc, penX + spaceAdv, penY')
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                    in (acc, penX + adv, penY')
        | otherwise =
            case lookupCodepointLazy ttf ch of
              Nothing -> (acc, penX, penY')
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (acc, penX, penY')
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                        quad = quadVerts screen (penX, penY') texel glyph
                        penX' = penX + adv
                    in (quad : acc, penX', penY')
      (chunks, _, _) = foldl' step ([], penX0, penY) text
  in concat (reverse chunks)

buildTextVerticesLazyScaled :: Double -> IntMap.IntMap GlyphMSDF -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> TTF -> MSDF.MSDFConfig -> [Float]
buildTextVerticesLazyScaled scale glyphs screen (penX0, penY) texel text ttf cfg =
  let lineAdvance = lineAdvanceTTF cfg ttf * scale
      spaceAdv = spaceAdvanceCfg cfg * scale
      step (acc, penX, penY') ch
        | ch == '\n' = (acc, penX0, penY' - lineAdvance)
        | isSpace ch =
            case lookupCodepointLazy ttf ch of
              Nothing -> (acc, penX + spaceAdv, penY')
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (acc, penX + spaceAdv, penY')
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                    in (acc, penX + adv * scale, penY')
        | otherwise =
            case lookupCodepointLazy ttf ch of
              Nothing -> (acc, penX, penY')
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (acc, penX, penY')
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                        quad = quadVertsScaled scale screen (penX, penY') texel glyph
                        penX' = penX + adv * scale
                    in (quad : acc, penX', penY')
      (chunks, _, _) = foldl' step ([], penX0, penY) text
  in concat (reverse chunks)

textBoundsLazy :: IntMap.IntMap GlyphMSDF -> String -> TTF -> MSDF.MSDFConfig -> (Double, Double, Double, Double)
textBoundsLazy glyphs text ttf cfg =
  let lineAdvance = lineAdvanceTTF cfg ttf
      spaceAdv = spaceAdvanceCfg cfg
      step (penX, penY', acc) ch
        | ch == '\n' = (0, penY' - lineAdvance, acc)
        | isSpace ch =
            case lookupCodepointLazy ttf ch of
              Nothing -> (penX + spaceAdv, penY', acc)
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (penX + spaceAdv, penY', acc)
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                    in (penX + adv, penY', acc)
        | otherwise =
            case lookupCodepointLazy ttf ch of
              Nothing -> (penX, penY', acc)
              Just gi ->
                case IntMap.lookup gi glyphs of
                  Nothing -> (penX, penY', acc)
                  Just glyph ->
                    let GlyphMSDF { advance = adv } = glyph
                        (x0, y0, x1, y1) = glyphQuad glyph (penX, penY')
                        acc' = case acc of
                          Nothing -> Just (x0, y0, x1, y1)
                          Just (mnx, mny, mxx, mxy) ->
                            Just (min mnx x0, min mny y0, max mxx x1, max mxy y1)
                    in (penX + adv, penY', acc')
      (_, _, bounds) = foldl' step (0, 0, Nothing) text
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
        , MSDF.splitIntersections = fromMaybe outlineCfg.splitIntersections opts.splitOpt
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
parseArgs ("--split-intersections":rest) opts = parseArgs rest opts { splitOpt = Just True }
parseArgs ("--no-split-intersections":rest) opts = parseArgs rest opts { splitOpt = Just False }
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

formatMs :: Double -> String
formatMs ms = showFFloat (Just 2) ms "ms"

logTimings :: String -> BuildTimings -> IO ()
logTimings label timings =
  let BuildTimings
        { totalMs = total
        , renderMs = render
        , packPlaceMs = place
        , packImageMs = image
        , kerningMs = kern
        , marksMs = marks
        , packRectCount = rects
        , atlasSize = atlas
        } = timings
      atlasStr = case atlas of
        Nothing -> "none"
        Just (w, h) -> show w <> "x" <> show h
  in logMsg
      ( "timing[" <> label <> "]: total=" <> formatMs total
        <> " render=" <> formatMs render
        <> " packPlace=" <> formatMs place
        <> " packImage=" <> formatMs image
        <> " kerning=" <> formatMs kern
        <> " marks=" <> formatMs marks
        <> " rects=" <> show rects
        <> " atlas=" <> atlasStr
      )

buildTextVertices :: MSDFAtlas -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> [Float]
buildTextVertices atlas screen (penX0, penY) texel text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      lineAdvance = lineAdvanceAtlas atlas
      spaceAdv = spaceAdvanceAtlas atlas
      step (acc, penX, penY') ch
        | ch == '\n' = (acc, penX0, penY' - lineAdvance)
        | isSpace ch =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (acc, penX + spaceAdv, penY')
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                in (acc, penX + adv, penY')
        | otherwise =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (acc, penX, penY')
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                    quad = quadVerts screen (penX, penY') texel glyph
                    penX' = penX + adv
                in (quad : acc, penX', penY')
      (chunks, _, _) = foldl' step ([], penX0, penY) text
  in concat (reverse chunks)

buildTextVerticesScaled :: Double -> MSDFAtlas -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> [Float]
buildTextVerticesScaled scale atlas screen (penX0, penY) texel text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      lineAdvance = lineAdvanceAtlas atlas * scale
      spaceAdv = spaceAdvanceAtlas atlas * scale
      step (acc, penX, penY') ch
        | ch == '\n' = (acc, penX0, penY' - lineAdvance)
        | isSpace ch =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (acc, penX + spaceAdv, penY')
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                in (acc, penX + adv * scale, penY')
        | otherwise =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (acc, penX, penY')
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                    quad = quadVertsScaled scale screen (penX, penY') texel glyph
                    penX' = penX + adv * scale
                in (quad : acc, penX', penY')
      (chunks, _, _) = foldl' step ([], penX0, penY) text
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

glyphQuadScaled :: Double -> GlyphMSDF -> (Double, Double) -> (Double, Double, Double, Double)
glyphQuadScaled scale glyph (penX, penY) =
  let bmp = glyph.bitmap
      x0 = penX + bmp.offsetX * scale
      y0 = penY + bmp.offsetY * scale
      x1 = x0 + fromIntegral bmp.width * scale
      y1 = y0 + fromIntegral bmp.height * scale
  in (x0, y0, x1, y1)

quadVertsScaled :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double) -> GlyphMSDF -> [Float]
quadVertsScaled scale screen pen texel glyph =
  let (x0, y0, x1, y1) = glyphQuadScaled scale glyph pen
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
      lineAdvance = lineAdvanceAtlas atlas
      spaceAdv = spaceAdvanceAtlas atlas
      step (penX, penY', acc) ch
        | ch == '\n' = (0, penY' - lineAdvance, acc)
        | isSpace ch =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (penX + spaceAdv, penY', acc)
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                in (penX + adv, penY', acc)
        | otherwise =
            case lookupCodepoint atlas (ord ch) of
              Nothing -> (penX, penY', acc)
              Just gi ->
                let glyph = glyphsArr ! gi
                    GlyphMSDF { advance = adv } = glyph
                    (x0, y0, x1, y1) = glyphQuad glyph (penX, penY')
                    acc' = case acc of
                      Nothing -> Just (x0, y0, x1, y1)
                      Just (mnx, mny, mxx, mxy) ->
                        Just (min mnx x0, min mny y0, max mxx x1, max mxy y1)
                in (penX + adv, penY', acc')
      (_, _, bounds) = foldl' step (0, 0, Nothing) text
  in case bounds of
       Just b -> b
       Nothing -> (0, 0, 0, 0)

scaleBounds :: Double -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
scaleBounds scale (x0, y0, x1, y1) =
  (x0 * scale, y0 * scale, x1 * scale, y1 * scale)

fitScaleToWidth :: Double -> (Double, Double, Double, Double) -> Double
fitScaleToWidth maxWidth (x0, _y0, x1, _y1) =
  let w = max 0 (x1 - x0)
  in if w <= 0
     then 1
     else min 1 (maxWidth / w)

fitScaleToBox :: Double -> Maybe Double -> (Double, Double, Double, Double) -> Double
fitScaleToBox maxWidth maxHeight bounds =
  let widthScale = fitScaleToWidth maxWidth bounds
      heightScale = case maxHeight of
        Nothing -> 1
        Just mh ->
          let h = max 0 (boundsHeight bounds)
          in if h <= 0 then 1 else min 1 (mh / h)
  in min widthScale heightScale

boundsHeight :: (Double, Double, Double, Double) -> Double
boundsHeight (_x0, y0, _x1, y1) = y1 - y0

lineAdvanceAtlas :: MSDFAtlas -> Double
lineAdvanceAtlas atlas =
  fromIntegral (atlas.ascent - atlas.descent + atlas.lineGap)

lineAdvanceTTF :: MSDF.MSDFConfig -> TTF -> Double
lineAdvanceTTF cfg ttf =
  let MSDF.TTF.Parser.Head { unitsPerEm = upem } = ttf.head
      MSDF.TTF.Parser.Hhea { ascent = asc, descent = desc, lineGap = gap0 } = ttf.hhea
      unitsPerEm = fromIntegral upem :: Double
      scale = fromIntegral cfg.pixelSize / unitsPerEm
      ascent = fromIntegral asc :: Double
      descent = fromIntegral desc :: Double
      gap = fromIntegral gap0 :: Double
  in (ascent - descent + gap) * scale

spaceAdvanceAtlas :: MSDFAtlas -> Double
spaceAdvanceAtlas atlas =
  fromIntegral atlas.pixelSize * 0.5

spaceAdvanceCfg :: MSDF.MSDFConfig -> Double
spaceAdvanceCfg cfg =
  fromIntegral cfg.pixelSize * 0.5

wrapTextForWidth :: Double -> Double -> MSDF.MSDFConfig -> TTF -> String -> String
wrapTextForWidth maxWidth scale cfg ttf text =
  let Cmap maps = ttf.cmap
      loc =
        case ttf.variations of
          Nothing -> Nothing
          Just vars ->
            let loc' = normalizeLocation vars.fvar vars.avar cfg.variations
            in if isDefaultLocation loc' then Nothing else Just loc'
      spaceAdv = spaceAdvanceCfg cfg
      advanceForGlyph =
        let go cache gi =
              case IntMap.lookup gi cache of
                Just v -> (v, cache)
                Nothing ->
                  let GlyphMSDF { advance = adv } = glyphMetricsOnlyAt loc cfg ttf gi
                      cache' = IntMap.insert gi adv cache
                  in (adv, cache')
        in go
      advanceForChar cache ch =
        case lookup (ord ch) maps of
          Just gi ->
            let (adv, cache') = advanceForGlyph cache gi
            in (adv, cache')
          Nothing ->
            if isSpace ch
            then (spaceAdv, cache)
            else (0, cache)
      tokenized = tokenizeWrap text
      spaceWidth = spaceAdv * scale
      wordWidth cache w =
        foldl' (\(acc, c) ch -> let (adv, c') = advanceForChar c ch in (acc + adv * scale, c')) (0, cache) w
      go [] cache lineWords lineW lines =
        let line = unwords (reverse lineWords)
            lines' = if null line then lines else line : lines
        in (reverse lines', cache)
      go (tok:toks) cache lineWords lineW lines =
        case tok of
          WrapNewline ->
            let line = unwords (reverse lineWords)
                lines' = if null line then lines else line : lines
            in go toks cache [] 0 lines'
          WrapWord w ->
            let (wWidth, cache') = wordWidth cache w
                extra = if null lineWords then 0 else spaceWidth
                newW = lineW + extra + wWidth
            in if lineW > 0 && newW > maxWidth
               then
                 let line = unwords (reverse lineWords)
                     lines' = if null line then lines else line : lines
                 in go toks cache' [w] wWidth lines'
               else
                 go toks cache' (w : lineWords) newW lines
      (linesOut, _) = go tokenized IntMap.empty [] 0 []
  in case linesOut of
       [] -> ""
       _ -> intercalate "\n" linesOut

data WrapToken = WrapWord String | WrapNewline

tokenizeWrap :: String -> [WrapToken]
tokenizeWrap =
  let flush acc toks =
        if null acc then toks else WrapWord (reverse acc) : toks
      go acc toks [] = reverse (flush acc toks)
      go acc toks (c:cs)
        | c == '\n' = go [] (WrapNewline : flush acc toks) cs
        | isSpace c = go [] (flush acc toks) cs
        | otherwise = go (c:acc) toks cs
  in go [] []

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
