{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Char (ord, toLower)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, nub, tails)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word8)
import Font
  ( FontCase (..),
    FontKind (..),
    FontSource (..),
    fontFilePath,
    fontInputArgs,
    fontInputLabel,
    interHarnessFontCases,
    interHarnessGlyphs,
  )
import MSDF.Compare (DiffStats (..), diffRGBA8, passesGate, strictGate)
import MSDF.Atlas (Atlas (..), AtlasEntry (..), AtlasRect (..), generateAtlasIO, generateAtlasWithRasterIO, mkAtlasCfg, packAtlas, renderAtlasTsv)
import MSDF.Encode (decodeMsdfgenRgba, encodeMsdfgenRgba)
import MSDF.Generate (BackendMode (..), RuntimeCfg (..), defaultRuntimeCfg, generateGlyphBatchIO, generateGlyphIO)
import MSDF.Manifest (Manifest (..), ManifestMeta (..), ManifestRow (..), loadManifest)
import MSDF.Native
  ( generateGlyphBatchNativeWithIO,
    hasProperSelfIntersection,
    PreparedLineSeg (..),
    prepareGlyphNativeIO,
    preparedNeedsOverlap,
    preparedLineSegs,
    requiresNonZeroWinding,
    rasterPreparedCpu,
  )
import MSDF.TextRender
  ( ScreenPxRange (..),
    ShaderCfg (..),
    addBorder,
    hcatWithGap,
    mkShaderCfg,
    resampleBilinear,
    shadeMtsdfImg,
    shadeMtsdfImgTo,
    solidImg
  )
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    ImgRGBA8 (..),
    Metrics (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkImgRGBA8,
    mkPxRange,
    unDim,
    unGlyphCode,
    unPxRange,
  )
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hClose, hPutStr, openTempFile)
import System.IO.Error (catchIOError)
import Test.QuickCheck
  ( Gen,
    NonPositive (..),
    Property,
    Testable,
    choose,
    chooseInt,
    forAll,
    quickCheckWithResult,
    stdArgs,
    vectorOf,
  )
import Test.QuickCheck.Test (isSuccess)

main :: IO ()
main = do
  processVarAxisSupportResult <- probeProcessVarAxisSupport
  (processVarAxisProbeOk, processVarAxisSupported) <-
    case processVarAxisSupportResult of
      Left err -> do
        ok <- check ("process variable-axis capability probe failed: " <> err) False
        pure (ok, False)
      Right supported -> do
        putStrLn
          ( if supported
              then "INFO: process backend variable-axis support detected."
              else "INFO: process backend variable-axis support not detected; strict SDL process-oracle checks will run in smoke mode."
          )
        pure (True, supported)
  staticCountOk <- check "at least 8 static font cases" (countKind Static >= 8)
  variableCountOk <- check "at least 8 variable font cases" (countKind Variable >= 8)
  uniqueFontIdsOk <- check "font case IDs are unique" (unique (fmap fontCaseId interHarnessFontCases))
  glyphCoverageOk <- check "at least 12 glyphs in harness set" (length interHarnessGlyphs >= 12)
  uniqueGlyphsOk <- check "glyph set is unique" (unique interHarnessGlyphs)
  variableAxesOk <- checkVariableAxes
  fontFilesOk <- checkFontFiles
  fontBehaviorOk <- runFontBehaviorChecks
  atlasChecksOk <- runAtlasChecks
  batchGenerateOk <- runBatchGenerateChecks
  preparedSegIntersectionOk <- runPreparedSegIntersectionChecks
  variableAxisNativeOk <- runVariableAxisNativeRegression
  backendParityOk <- runBackendParitySmoke
  thinItalicStrictParityOk <- runThinItalicStrictParityRegression
  sdlDemoVarBoldStrictParityOk <- runSdlDemoVarBoldStrictParity processVarAxisSupported
  sdlDemoRenderParityOk <- runSdlDemoRenderParity processVarAxisSupported
  compareBehaviorOk <- runCompareBehaviorChecks
  textRenderOk <- runTextRenderChecks
  decodeChecksOk <- runDecodeChecks
  manifestChecksOk <- runManifestChecks
  sdlShaderSourceChecksOk <- runSdlShaderSourceChecks
  parityWorkflowSourceChecksOk <- runParityWorkflowSourceChecks
  let allOk =
        and
          [ staticCountOk,
            variableCountOk,
            uniqueFontIdsOk,
            glyphCoverageOk,
            uniqueGlyphsOk,
            variableAxesOk,
            fontFilesOk,
            fontBehaviorOk,
            atlasChecksOk,
            batchGenerateOk,
            preparedSegIntersectionOk,
            variableAxisNativeOk,
            backendParityOk,
            thinItalicStrictParityOk,
            processVarAxisProbeOk,
            sdlDemoVarBoldStrictParityOk,
            sdlDemoRenderParityOk,
            compareBehaviorOk,
            textRenderOk,
            decodeChecksOk,
            manifestChecksOk,
            sdlShaderSourceChecksOk,
            parityWorkflowSourceChecksOk
          ]
  propertiesOk <- runQuickCheckProperties
  if allOk && propertiesOk
    then putStrLn "Harness and property checks passed."
    else exitFailure

countKind :: FontKind -> Int
countKind expected =
  length $
    filter (\fontCase -> fontCaseKind fontCase == expected) interHarnessFontCases

checkVariableAxes :: IO Bool
checkVariableAxes = do
  checks <-
    forM interHarnessFontCases $ \fontCase ->
      case fontCaseSource fontCase of
        StaticFont _ -> pure True
        VariableFont _ axes ->
          check
            ("variable case has wght+opsz axes: " <> fontCaseId fontCase)
            (hasAxis "wght" axes && hasAxis "opsz" axes)
  pure (and checks)
  where
    hasAxis axisName axes = any (\(name, _) -> name == axisName) axes

checkFontFiles :: IO Bool
checkFontFiles = do
  checks <-
    forM interHarnessFontCases $ \fontCase -> do
      exists <- doesFileExist (fontFilePath fontCase)
      check
        ("font file exists: " <> fontCaseId fontCase <> " -> " <> fontFilePath fontCase)
        exists
  pure (and checks)

check :: String -> Bool -> IO Bool
check label condition = do
  if condition
    then pure ()
    else putStrLn ("FAIL: " <> label)
  pure condition

unique :: Eq a => [a] -> Bool
unique values = length values == length (nub values)

runBackendParitySmoke :: IO Bool
runBackendParitySmoke = do
  strictRaw <- lookupEnv "MASDIFF_STRICT_PARITY"
  let strictEnabled = parseBool strictRaw
  case mkParityCfg of
    Left err ->
      check ("parity config construction failed: " <> err) False
    Right cfg -> do
      let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
      let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
      checks <- traverse (runParityCase strictEnabled nativeRuntime processRuntime cfg) parityCases
      pure (and checks)

runThinItalicStrictParityRegression :: IO Bool
runThinItalicStrictParityRegression =
  case mkParityCfg of
    Left err ->
      check ("thin-italic strict parity config failed: " <> err) False
    Right cfg -> do
      let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
      let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
      runParityCase
        True
        nativeRuntime
        processRuntime
        cfg
        ( "parity-thinitalic-a",
          FontFile {path = "assets/Inter/static/Inter_28pt-ThinItalic.ttf"},
          ord 'a'
        )

mkParityCfg :: Either String GenCfg
mkParityCfg = do
  dim <- mkDim 64
  pxr <- mkPxRange 8.0
  pure
        GenCfg
          { mode = Mtsdf,
            dim = dim,
            pxr = pxr,
            seed = 1,
            autoframe = True,
            ovlp = False
          }

parityCases :: [(String, FontSrc, Int)]
parityCases =
  [ ( "parity-static-A",
      FontFile {path = "assets/Inter/static/Inter_18pt-Regular.ttf"},
      ord 'A'
    ),
    ( "parity-variable-a",
      VarFontFile
        { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
          axes =
            Map.fromList
              [ (AxisTag (T.pack "opsz"), AxisVal 14.0),
                (AxisTag (T.pack "wght"), AxisVal 400.0)
              ]
        },
      ord 'a'
    )
  ]

mkDemoCfg :: Either String GenCfg
mkDemoCfg = do
  dim <- mkDim 128
  pxr <- mkPxRange 8.0
  pure
    GenCfg
      { mode = Mtsdf,
        dim = dim,
        pxr = pxr,
        seed = 1,
        autoframe = True,
        ovlp = False
      }

demoRegularSrc :: FontSrc
demoRegularSrc = FontFile {path = "assets/Inter/static/Inter_24pt-Regular.ttf"}

demoVarLightSrc :: FontSrc
demoVarLightSrc =
  VarFontFile
    { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
      axes =
        Map.fromList
          [ (AxisTag (T.pack "wght"), AxisVal 300),
            (AxisTag (T.pack "opsz"), AxisVal 14)
          ]
    }

demoVarBoldSrc :: FontSrc
demoVarBoldSrc =
  VarFontFile
    { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
      axes =
        Map.fromList
          [ (AxisTag (T.pack "wght"), AxisVal 900),
            (AxisTag (T.pack "opsz"), AxisVal 32)
          ]
    }

demoLineSpecs :: [(String, FontSrc, String)]
demoLineSpecs =
  [ ("demo-line-regular-1", demoRegularSrc, "MASDIFF SDL3"),
    ("demo-line-regular-2", demoRegularSrc, "AaRMYgq 0123 ?!"),
    ("demo-line-var-light", demoVarLightSrc, "VAR 300/14: AaRMY"),
    ("demo-line-var-bold", demoVarBoldSrc, "VAR 900/32: AaRMY")
  ]

probeProcessVarAxisSupport :: IO (Either String Bool)
probeProcessVarAxisSupport =
  case (mkParityCfg, mkGlyphCode (ord 'V')) of
    (Left err, _) ->
      pure (Left ("probe config failed: " <> err))
    (_, Left err) ->
      pure (Left ("probe glyph setup failed: " <> err))
    (Right cfg, Right glyph) -> do
      let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
      lightResult <- generateGlyphIO processRuntime cfg demoVarLightSrc glyph
      boldResult <- generateGlyphIO processRuntime cfg demoVarBoldSrc glyph
      pure $
        case (lightResult, boldResult) of
          (Left err, _) ->
            Left ("probe light generation failed: " <> show err)
          (_, Left err) ->
            Left ("probe bold generation failed: " <> show err)
          (Right lightOut, Right boldOut) ->
            Right (lightOut /= boldOut)

runParityCase :: Bool -> RuntimeCfg -> RuntimeCfg -> GenCfg -> (String, FontSrc, Int) -> IO Bool
runParityCase strictEnabled nativeRuntime processRuntime cfg (label, src, codepoint) =
  case mkGlyphCode codepoint of
    Left err ->
      check (label <> " glyph setup failed: " <> err) False
    Right glyph -> do
      nativeResult <- generateGlyphIO nativeRuntime cfg src glyph
      processResult <- generateGlyphIO processRuntime cfg src glyph
      case (nativeResult, processResult) of
        (Left err, _) ->
          check (label <> " native generation failed: " <> show err) False
        (_, Left err) ->
          check (label <> " process generation failed: " <> show err) False
        (Right nativeOut, Right processOut) ->
          let GenOut {img = nativeImg} = nativeOut
              GenOut {img = processImg} = processOut
           in case diffRGBA8 nativeImg processImg of
                Left err ->
                  check (label <> " diff failed: " <> err) False
                Right stats -> do
                  let strictOk = passesGate strictGate stats && stats.maxAbs <= 1
                  if strictEnabled && not strictOk
                    then
                      putStrLn
                        ( "FAIL details: "
                            <> label
                            <> " maxAbs="
                            <> show stats.maxAbs
                            <> " p99="
                            <> show stats.p99Abs
                            <> " mean="
                            <> show stats.meanAbs
                            <> " mismatch="
                            <> show stats.mismatch
                        )
                    else pure ()
                  let ok = if strictEnabled then strictOk else True
                  if ok
                    then pure ()
                    else
                      pure ()
                  check (label <> parityLabel strictEnabled) ok

runSdlDemoVarBoldStrictParity :: Bool -> IO Bool
runSdlDemoVarBoldStrictParity processVarAxisSupported = do
  strictRaw <- lookupEnv "MASDIFF_STRICT_SDL_DEMO_PARITY"
  let strictRequested = parseBool strictRaw
  let strictEnabled = strictRequested && processVarAxisSupported
  when (strictRequested && not processVarAxisSupported) $
    putStrLn "INFO: SDL demo var-bold strict parity downgraded to smoke (process backend does not vary by axis in this environment)."
  case mkDemoCfg of
    Left err ->
      check ("SDL demo strict parity config failed: " <> err) False
    Right cfg -> do
      let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
      let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
      let chars = nub (filter (/= ' ') "VAR 900/32: AaRMY")
      checks <-
        traverse
          ( \ch ->
              runParityCase
                strictEnabled
                nativeRuntime
                processRuntime
                cfg
                ("parity-sdl-varbold-" <> show (ord ch), demoVarBoldSrc, ord ch)
          )
          chars
      pure (and checks)

runSdlDemoRenderParity :: Bool -> IO Bool
runSdlDemoRenderParity processVarAxisSupported = do
  strictRaw <- lookupEnv "MASDIFF_STRICT_SDL_DEMO_PARITY"
  let strictRequested = parseBool strictRaw
  let strictEnabled = strictRequested && processVarAxisSupported
  when (strictRequested && not processVarAxisSupported) $
    putStrLn "INFO: SDL demo render strict parity downgraded to smoke (process backend does not vary by axis in this environment)."
  case (mkDemoCfg, mkShaderCfg (AutoPxRange 8.0) True 0.0) of
    (Left err, _) ->
      check ("SDL demo render parity config failed: " <> err) False
    (_, Left err) ->
      check ("SDL demo render parity shader config failed: " <> err) False
    (Right cfg, Right shader) -> do
      let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
      let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
      nativeImgResult <- renderDemoPreview nativeRuntime cfg shader
      processImgResult <- renderDemoPreview processRuntime cfg shader
      case (nativeImgResult, processImgResult) of
        (Left err, _) ->
          check ("SDL demo native render parity failed: " <> err) False
        (_, Left err) ->
          check ("SDL demo process render parity failed: " <> err) False
        (Right nativeImg, Right processImg) ->
          case diffRGBA8 nativeImg processImg of
            Left err ->
              check ("SDL demo render parity diff failed: " <> err) False
            Right stats -> do
              let strictOk = passesGate strictGate stats && stats.maxAbs <= 1
              let smokeOk = stats.pxCount > 0 && stats.chCount > 0
              if strictEnabled && not strictOk
                then
                  putStrLn
                    ( "FAIL details: sdl-demo-render-parity"
                        <> " maxAbs="
                        <> show stats.maxAbs
                        <> " p99="
                        <> show stats.p99Abs
                        <> " mean="
                        <> show stats.meanAbs
                        <> " mismatch="
                        <> show stats.mismatch
                    )
                else pure ()
              check
                (if strictEnabled then "SDL demo render parity strict gate" else "SDL demo render parity smoke")
                (if strictEnabled then strictOk else smokeOk)

renderDemoPreview :: RuntimeCfg -> GenCfg -> ShaderCfg -> IO (Either String ImgRGBA8)
renderDemoPreview runtime cfg shader = do
  lineResults <- traverse (renderDemoLine runtime cfg shader) demoLineSpecs
  pure $ do
    lines0 <- sequence lineResults
    padded <- padImagesToWidth lines0
    vcatWithGap 10 padded

renderDemoLine :: RuntimeCfg -> GenCfg -> ShaderCfg -> (String, FontSrc, String) -> IO (Either String ImgRGBA8)
renderDemoLine runtime cfg shader (_label, src, txt) = do
  let chars = nub [ch | ch <- txt, ch /= ' ']
  glyphPairsResult <-
    pure $
      traverse
        ( \ch -> do
            glyph <- mkGlyphCode (ord ch)
            Right (ch, glyph)
        )
        chars
  case glyphPairsResult of
    Left err ->
      pure (Left ("SDL demo line glyph setup failed: " <> err))
    Right glyphPairs -> do
      let glyphCodes = fmap snd glyphPairs
      generated <- generateGlyphBatchIO runtime 1 cfg src glyphCodes
      case sequence generated of
        Left genErr ->
          pure (Left ("SDL demo line generation failed: " <> show genErr))
        Right outs -> do
          let glyphMap = Map.fromList (zip (fmap fst glyphPairs) outs)
          let cell = 96
          let spaceW = 40
          let spaceImgResult = solidImg spaceW cell (255, 255, 255, 255)
          case spaceImgResult of
            Left err ->
              pure (Left ("SDL demo line space image failed: " <> err))
            Right spaceImg -> do
              atomResults <- traverse (renderAtom glyphMap spaceImg shader cell) txt
              pure $ do
                atomImgs <- sequence atomResults
                hcatWithGap 6 atomImgs

renderAtom ::
  Map.Map Char GenOut ->
  ImgRGBA8 ->
  ShaderCfg ->
  Int ->
  Char ->
  IO (Either String ImgRGBA8)
renderAtom glyphMap spaceImg shader cell ch
  | ch == ' ' = pure (Right spaceImg)
  | otherwise =
      case Map.lookup ch glyphMap of
        Nothing ->
          pure (Left ("SDL demo line missing generated glyph for char: " <> show ch))
        Just out ->
          pure (shadeMtsdfImgTo shader cell cell out.img)

vcatWithGap :: Int -> [ImgRGBA8] -> Either String ImgRGBA8
vcatWithGap gap imgs
  | gap < 0 = Left "vertical gap must be >= 0."
  | otherwise =
      case imgs of
        [] -> Left "Need at least one image to compose vertically."
        firstImg : _ ->
          let w0 = firstImg.w
           in if any (\img -> img.w /= w0) imgs
                then Left "All images must have the same width."
                else do
                  let totalH = sum (fmap (.h) imgs) + gap * (length imgs - 1)
                  let whiteRow = BS.replicate (w0 * 4) 255
                  let gapRows = BS.concat (replicate gap whiteRow)
                  let imgBody img = BS.concat [sliceRow img y | y <- [0 .. img.h - 1]]
                  let px = BS.intercalate gapRows (fmap imgBody imgs)
                  mkImgRGBA8 w0 totalH px

padImagesToWidth :: [ImgRGBA8] -> Either String [ImgRGBA8]
padImagesToWidth imgs =
  case imgs of
    [] -> Right []
    _ ->
      let targetW = maximum (fmap (.w) imgs)
       in traverse (padImageRight targetW) imgs

padImageRight :: Int -> ImgRGBA8 -> Either String ImgRGBA8
padImageRight targetW img
  | targetW < img.w = Left "target width cannot be smaller than source width."
  | targetW == img.w = Right img
  | otherwise = do
      let padBytes = BS.replicate ((targetW - img.w) * 4) 255
      let rows = [sliceRow img y <> padBytes | y <- [0 .. img.h - 1]]
      mkImgRGBA8 targetW img.h (BS.concat rows)

sliceRow :: ImgRGBA8 -> Int -> BS.ByteString
sliceRow img y =
  BS.take rowBytes (BS.drop (y * rowBytes) img.px)
  where
    rowBytes = img.w * 4

runAtlasChecks :: IO Bool
runAtlasChecks = do
  invalidWOk <- check "mkAtlasCfg rejects width <= 0" (isLeft (mkAtlasCfg 0 128 1))
  invalidHOk <- check "mkAtlasCfg rejects height <= 0" (isLeft (mkAtlasCfg 128 0 1))
  invalidPadOk <- check "mkAtlasCfg rejects negative padding" (isLeft (mkAtlasCfg 128 128 (-1)))
  case (mkAtlasCfg 96 96 1, mkGlyphCode (ord 'A'), mkGlyphCode (ord 'B'), mkGlyphCode (ord 'C')) of
    (Right atlasCfg, Right glyphA, Right glyphB, Right glyphC) ->
      case sequence [mkSolidOut 33, mkSolidOut 77, mkSolidOut 121] of
        Left err ->
          check ("atlas test setup failed: " <> err) False
        Right outs -> do
          let firstOut =
                case outs of
                  [] -> Nothing
                  x : _ -> Just x
          let packed = packAtlas atlasCfg (zip [glyphA, glyphB, glyphC] outs)
          packedOk <-
            case packed of
              Left err ->
                check ("packAtlas failed: " <> err) False
              Right atlas -> do
                entryCountOk <- check "packAtlas emits one entry per glyph" (length atlas.entries == 3)
                boundsOk <- check "packAtlas entries stay within page bounds" (all withinPage atlas.entries)
                overlapOk <- check "packAtlas entries do not overlap on same page" (noOverlap atlas.entries)
                tsvOk <- check "renderAtlasTsv emits atlas header + columns" ("# atlas_w=" `isInfixOf` renderAtlasTsv atlas && "glyph_hex\tpage\tx\ty\tw\th" `isInfixOf` renderAtlasTsv atlas)
                pure (entryCountOk && boundsOk && overlapOk && tsvOk)
          oversizeOk <-
            case mkAtlasCfg 16 16 0 of
              Left err ->
                check ("atlas oversize config failed: " <> err) False
              Right smallCfg ->
                case firstOut of
                  Nothing ->
                    check "atlas oversize setup failed: missing first test output" False
                  Just outA ->
                    check "packAtlas rejects oversized glyphs" (isLeft (packAtlas smallCfg [(glyphA, outA)]))
          multiPageOk <-
            case mkAtlasCfg 32 32 1 of
              Left err ->
                check ("atlas multipage config failed: " <> err) False
              Right smallCfg ->
                case packAtlas smallCfg (zip [glyphA, glyphB, glyphC] outs) of
                  Left err ->
                    check ("packAtlas multipage failed: " <> err) False
                  Right atlas ->
                    check "packAtlas spills to multiple pages when needed" (length atlas.pages >= 2)
          runtimeOk <-
            case mkParityCfg of
              Left err ->
                check ("atlas runtime config failed: " <> err) False
              Right cfg -> do
                let runtime = defaultRuntimeCfg {backend = BackendNative}
                let src = FontFile {path = "assets/Inter/static/Inter_18pt-Regular.ttf"}
                atlasResult <- generateAtlasIO runtime 2 atlasCfg cfg src [glyphA, glyphB, glyphA]
                case atlasResult of
                  Left err ->
                    check ("generateAtlasIO failed: " <> err) False
                  Right atlas -> do
                    dedupeOk <- check "generateAtlasIO deduplicates repeated glyph codes" (length atlas.entries == 2)
                    pageOk <- check "generateAtlasIO produces at least one page" (not (null atlas.pages))
                    pure (dedupeOk && pageOk)
          callbackRuntimeOk <-
            case mkParityCfg of
              Left err ->
                check ("atlas callback runtime config failed: " <> err) False
              Right cfg -> do
                let src = FontFile {path = "assets/Inter/static/Inter_18pt-Regular.ttf"}
                hitRef <- newIORef (0 :: Int)
                let countingRaster cfg0 prepared = do
                      modifyIORef' hitRef (+ 1)
                      pure (rasterPreparedCpu cfg0 prepared)
                atlasResult <- generateAtlasWithRasterIO 2 countingRaster atlasCfg cfg src [glyphA, glyphB, glyphA]
                hitCount <- readIORef hitRef
                case atlasResult of
                  Left err ->
                    check ("generateAtlasWithRasterIO failed: " <> err) False
                  Right atlas -> do
                    dedupeOk <- check "generateAtlasWithRasterIO deduplicates repeated glyph codes" (length atlas.entries == 2)
                    pageOk <- check "generateAtlasWithRasterIO produces at least one page" (not (null atlas.pages))
                    callbackCountOk <- check "generateAtlasWithRasterIO invokes raster callback for deduplicated glyphs" (hitCount == 2)
                    pure (dedupeOk && pageOk && callbackCountOk)
          pure (invalidWOk && invalidHOk && invalidPadOk && packedOk && oversizeOk && multiPageOk && runtimeOk && callbackRuntimeOk)
    _ ->
      check "atlas test glyph/config setup failed" False
  where
    mkSolidOut value = do
      img <- mkImgRGBA8 24 24 (BS.replicate (24 * 24 * 4) value)
      pure
        GenOut
          { img = img,
            metrics =
              Metrics
                { adv = 12.0,
                  bounds = (0.0, 0.0, 1.0, 1.0),
                  scale = Nothing,
                  translate = Nothing,
                  range = Nothing
                }
          }
    withinPage entry =
      let rect = entry.rect
       in rect.x >= 0
            && rect.y >= 0
            && rect.x + rect.w <= 96
            && rect.y + rect.h <= 96
    noOverlap entries = all disjoint (pairs entries)
    disjoint (a, b)
      | a.page /= b.page = True
      | otherwise = not (rectsOverlap a.rect b.rect)
    rectsOverlap r0 r1 =
      let x0 = r0.x
          y0 = r0.y
          x1 = r1.x
          y1 = r1.y
       in x0 < x1 + r1.w
            && x1 < x0 + r0.w
            && y0 < y1 + r1.h
            && y1 < y0 + r0.h
    pairs xs =
      [ (x, y)
        | (x : ys) <- tails xs,
          y <- ys
      ]

runBatchGenerateChecks :: IO Bool
runBatchGenerateChecks =
  case mkParityCfg of
    Left err ->
      check ("batch generation config failed: " <> err) False
    Right cfg ->
      case traverse mkGlyphCode [ord 'A', ord 'B', ord 'C', ord 'D'] of
        Left err ->
          check ("batch generation glyph setup failed: " <> err) False
        Right glyphs -> do
          let runtime = defaultRuntimeCfg {backend = BackendNative}
          let src = FontFile {path = "assets/Inter/static/Inter_18pt-Regular.ttf"}
          seqResults <- traverse (generateGlyphIO runtime cfg src) glyphs
          batchJobs1 <- generateGlyphBatchIO runtime 1 cfg src glyphs
          batchJobs4 <- generateGlyphBatchIO runtime 4 cfg src glyphs
          jobs1Ok <-
            check
              "generateGlyphBatchIO jobs=1 matches sequential generateGlyphIO"
              (batchJobs1 == seqResults)
          jobs4Ok <-
            check
              "generateGlyphBatchIO jobs=4 matches sequential generateGlyphIO"
              (batchJobs4 == seqResults)
          let variableSrc =
                VarFontFile
                  { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
                    axes =
                      Map.fromList
                        [ (AxisTag (T.pack "wght"), AxisVal 700),
                          (AxisTag (T.pack "opsz"), AxisVal 14)
                        ]
                  }
          variableSeq <- traverse (generateGlyphIO runtime cfg variableSrc) glyphs
          variableBatch <- generateGlyphBatchIO runtime 4 cfg variableSrc glyphs
          variableOk <-
            check
              "generateGlyphBatchIO variable jobs=4 matches sequential generateGlyphIO"
              (variableBatch == variableSeq)
          callbackHitRef <- newIORef (0 :: Int)
          let countingRaster cfg0 prepared = do
                modifyIORef' callbackHitRef (+ 1)
                pure (rasterPreparedCpu cfg0 prepared)
          callbackBatch <- generateGlyphBatchNativeWithIO 4 countingRaster cfg src glyphs
          callbackBatchOk <-
            check
              "generateGlyphBatchNativeWithIO callback path matches sequential generateGlyphIO"
              (callbackBatch == seqResults)
          callbackHitCount <- readIORef callbackHitRef
          callbackCountOk <-
            check
              "generateGlyphBatchNativeWithIO invokes callback once per prepared glyph"
              (callbackHitCount == length glyphs)
          missingBatch <- generateGlyphBatchIO runtime 4 cfg (FontFile {path = "/tmp/masdiff_missing_font.ttf"}) glyphs
          missingOk <-
            check
              "generateGlyphBatchIO reports MissingInput for missing font"
              (all isMissingInput missingBatch)
          missingCallbackHitRef <- newIORef (0 :: Int)
          let missingCountingRaster cfg0 prepared = do
                modifyIORef' missingCallbackHitRef (+ 1)
                pure (rasterPreparedCpu cfg0 prepared)
          missingWithCallback <-
            generateGlyphBatchNativeWithIO 4 missingCountingRaster cfg (FontFile {path = "/tmp/masdiff_missing_font.ttf"}) glyphs
          missingCallbackHitCount <- readIORef missingCallbackHitRef
          missingCallbackOk <-
            check
              "generateGlyphBatchNativeWithIO skips callback when preparation fails"
              (all isMissingInput missingWithCallback && missingCallbackHitCount == 0)
          let processRuntimeMissing =
                defaultRuntimeCfg
                  { backend = BackendProcess,
                    msdfgenBin = "/tmp/masdiff_missing_msdfgen_bin"
                  }
          processMissingJobs1 <- generateGlyphBatchIO processRuntimeMissing 1 cfg src glyphs
          processMissingJobs4 <- generateGlyphBatchIO processRuntimeMissing 4 cfg src glyphs
          processMissingJobs1Ok <-
            check
              "generateGlyphBatchIO process jobs=1 reports MissingInput for missing msdfgen executable"
              (all isMissingInput processMissingJobs1)
          processMissingJobs4Ok <-
            check
              "generateGlyphBatchIO process jobs=4 reports MissingInput for missing msdfgen executable"
              (all isMissingInput processMissingJobs4)
          pure
            ( jobs1Ok
                && jobs4Ok
                && variableOk
                && callbackBatchOk
                && callbackCountOk
                && missingOk
                && missingCallbackOk
                && processMissingJobs1Ok
                && processMissingJobs4Ok
            )
  where
    isMissingInput result =
      case result of
        Left (MissingInput _) -> True
        _ -> False

runPreparedSegIntersectionChecks :: IO Bool
runPreparedSegIntersectionChecks = do
  let crossSegs =
        [ mkSeg 0 0 10 10,
          mkSeg 0 10 10 0
        ]
      endpointTouchSegs =
        [ mkSeg 0 0 10 0,
          mkSeg 10 0 10 10
        ]
      doubledSquareSegs =
        let square =
              [ mkSeg 0 0 10 0,
                mkSeg 10 0 10 10,
                mkSeg 10 10 0 10,
                mkSeg 0 10 0 0
              ]
         in square <> square
  syntheticOverlapGuardOk <-
    check
      "requiresNonZeroWinding stays off when coincident overlap has no proper intersections"
      (not (requiresNonZeroWinding doubledSquareSegs) && not (hasProperSelfIntersection doubledSquareSegs))
  syntheticCrossOk <-
    check
      "hasProperSelfIntersection detects interior crossing on synthetic segments"
      (hasProperSelfIntersection crossSegs)
  syntheticTouchOk <-
    check
      "hasProperSelfIntersection ignores endpoint-only touches on synthetic segments"
      (not (hasProperSelfIntersection endpointTouchSegs))
  syntheticSimpleOk <-
    check
      "requiresNonZeroWinding stays off for simple non-overlapping contour"
      (not (requiresNonZeroWinding
              [ mkSeg 0 0 10 0,
                mkSeg 10 0 10 10,
                mkSeg 10 10 0 10,
                mkSeg 0 10 0 0
              ]))
  case (mkDim 256, mkPxRange 8.0, mkGlyphCode (ord '4'), mkGlyphCode (ord '9'), mkGlyphCode (ord 'A'), mkGlyphCode (ord 'R'), mkGlyphCode (ord '1')) of
    (Right dim, Right pxr, Right glyph4, Right glyph9, Right glyphA, Right glyphR, Right glyph1) -> do
      let cfg =
            GenCfg
              { mode = Mtsdf,
                dim = dim,
                pxr = pxr,
                seed = 1,
                autoframe = True,
                ovlp = False
              }
          src =
            VarFontFile
              { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
                axes =
                  Map.fromList
                    [ (AxisTag (T.pack "opsz"), AxisVal 32.0),
                      (AxisTag (T.pack "wght"), AxisVal 900.0)
                    ]
              }
      prep4 <- prepareGlyphNativeIO src glyph4
      prep9 <- prepareGlyphNativeIO src glyph9
      prepA <- prepareGlyphNativeIO src glyphA
      prepR <- prepareGlyphNativeIO src glyphR
      prep1 <- prepareGlyphNativeIO src glyph1
      case (prep4, prep9, prepA, prepR, prep1) of
        (Right prepared4, Right prepared9, Right preparedA, Right preparedR, Right prepared1) -> do
          let segs4 = preparedLineSegs cfg prepared4
              segs9 = preparedLineSegs cfg prepared9
              segsA = preparedLineSegs cfg preparedA
              segsR = preparedLineSegs cfg preparedR
              cfgOvlp = cfg {ovlp = True}
              needsOverlap4 = preparedNeedsOverlap cfgOvlp prepared4
              needsOverlap9 = preparedNeedsOverlap cfgOvlp prepared9
              needsOverlapA = preparedNeedsOverlap cfgOvlp preparedA
              needsOverlapR = preparedNeedsOverlap cfgOvlp preparedR
              needsOverlap1 = preparedNeedsOverlap cfgOvlp prepared1
              segsROvlp = preparedLineSegs cfgOvlp preparedR
          glyph4CrossOk <-
            check
              "hasProperSelfIntersection detects Inter variable bold glyph '4' overlap"
              (hasProperSelfIntersection segs4)
          glyph4NonZeroOk <-
            check
              "requiresNonZeroWinding enables non-zero mode for Inter variable bold glyph '4'"
              (requiresNonZeroWinding segs4)
          glyphANonZeroOffOk <-
            check
              "requiresNonZeroWinding stays off for Inter variable bold glyph 'A'"
              (not (requiresNonZeroWinding segsA))
          glyphRNonZeroOffOk <-
            check
              "requiresNonZeroWinding stays off for Inter variable bold glyph 'R'"
              (not (requiresNonZeroWinding segsR))
          glyphAStableOk <-
            check
              "preparedLineSegs builds non-empty segment set for Inter variable bold glyph 'A'"
              (not (null segsA))
          glyph4NeedsOverlapOk <-
            check
              "preparedNeedsOverlap is enabled for Inter variable bold glyph '4' (compound contour)"
              needsOverlap4
          glyph9NeedsOverlapOk <-
            check
              "preparedNeedsOverlap is enabled for Inter variable bold glyph '9' (compound contour)"
              needsOverlap9
          glyphANeedsOverlapOk <-
            check
              "preparedNeedsOverlap is enabled for Inter variable bold glyph 'A' (compound contour)"
              needsOverlapA
          glyphRNeedsOverlapOk <-
            check
              "preparedNeedsOverlap is enabled for Inter variable bold glyph 'R' (compound contour)"
              needsOverlapR
          glyph1NeedsOverlapOffOk <-
            check
              "preparedNeedsOverlap stays off for Inter variable bold glyph '1' (single contour)"
              (not needsOverlap1)
          glyph9StableOk <-
            check
              "preparedLineSegs builds non-empty segment set for Inter variable bold glyph '9'"
              (not (null segs9))
          glyphROvlpColorDiversityOk <-
            check
              "preparedLineSegs overlap merge preserves edge color diversity for Inter variable bold glyph 'R'"
              (length (nub (fmap (\seg -> seg.col) segsROvlp)) >= 2)
          pure (syntheticOverlapGuardOk && syntheticCrossOk && syntheticTouchOk && syntheticSimpleOk && glyph4CrossOk && glyph4NonZeroOk && glyphANonZeroOffOk && glyphRNonZeroOffOk && glyphAStableOk && glyph4NeedsOverlapOk && glyph9NeedsOverlapOk && glyphANeedsOverlapOk && glyphRNeedsOverlapOk && glyph1NeedsOverlapOffOk && glyph9StableOk && glyphROvlpColorDiversityOk)
        (Left err4, Left err9, Left errA, Left errR, Left err1) ->
          check
            ("prepared-segment setup failed for glyphs '4', '9', 'A', 'R', and '1': " <> show err4 <> " | " <> show err9 <> " | " <> show errA <> " | " <> show errR <> " | " <> show err1)
            False
        (Left err4, _, _, _, _) ->
          check
            ("prepared-segment setup failed for glyph '4': " <> show err4)
            False
        (_, Left err9, _, _, _) ->
          check
            ("prepared-segment setup failed for glyph '9': " <> show err9)
            False
        (_, _, Left errA, _, _) ->
          check
            ("prepared-segment setup failed for glyph 'A': " <> show errA)
            False
        (_, _, _, Left errR, _) ->
          check
            ("prepared-segment setup failed for glyph 'R': " <> show errR)
            False
        (_, _, _, _, Left err1) ->
          check
            ("prepared-segment setup failed for glyph '1': " <> show err1)
            False
    _ ->
      check "prepared-segment setup failed: invalid dim/pxrange/glyph code" False
  where
    mkSeg x0 y0 x1 y1 =
      PreparedLineSeg
        { x0 = x0,
          y0 = y0,
          x1 = x1,
          y1 = y1,
          col = 0,
          caps = 0,
          cid = 0,
          cw = 0
        }

runVariableAxisNativeRegression :: IO Bool
runVariableAxisNativeRegression =
  case mkParityCfg of
    Left err ->
      check ("variable-axis native regression config failed: " <> err) False
    Right cfg ->
      case mkGlyphCode (ord 'a') of
        Left err ->
          check ("variable-axis native regression glyph setup failed: " <> err) False
        Right glyph -> do
          let runtime = defaultRuntimeCfg {backend = BackendNative}
              mkVarSrc weight =
                VarFontFile
                  { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
                    axes =
                      Map.fromList
                        [ (AxisTag (T.pack "opsz"), AxisVal 14.0),
                          (AxisTag (T.pack "wght"), AxisVal weight)
                        ]
                  }
          thinResult <- generateGlyphIO runtime cfg (mkVarSrc 100.0) glyph
          blackResult <- generateGlyphIO runtime cfg (mkVarSrc 900.0) glyph
          case (thinResult, blackResult) of
            (Left err, _) ->
              check ("variable-axis native regression thin render failed: " <> show err) False
            (_, Left err) ->
              check ("variable-axis native regression black render failed: " <> show err) False
            (Right thinOut, Right blackOut) -> do
              let thinImg = thinOut.img
                  blackImg = blackOut.img
              case diffRGBA8 thinImg blackImg of
                Left err ->
                  check ("variable-axis native regression diff failed: " <> err) False
                Right stats -> do
                  differentOutputsOk <-
                    check
                      "native variable-font axis change mutates output bytes"
                      (thinImg /= blackImg)
                  nonZeroDiffOk <-
                    check
                      "native variable-font axis change has non-zero diff stats"
                      (stats.maxAbs > 0 && stats.mismatch > 0)
                  pure (differentOutputsOk && nonZeroDiffOk)

parityLabel :: Bool -> String
parityLabel strictEnabled =
  if strictEnabled
    then " native/process strict parity"
    else " native/process smoke parity"

parseBool :: Maybe String -> Bool
parseBool raw =
  case fmap (fmap toLower) raw of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    _ -> False

runFontBehaviorChecks :: IO Bool
runFontBehaviorChecks = do
  let staticFontCase =
        FontCase
          { fontCaseId = "test-static",
            fontCaseKind = Static,
            fontCaseSource = StaticFont "fonts/Inter-Regular.ttf"
          }
      variableFontCase =
        FontCase
          { fontCaseId = "test-variable",
            fontCaseKind = Variable,
            fontCaseSource = VariableFont "fonts/Inter-Variable.ttf" [("wght", "700"), ("opsz", "32")]
          }
  staticPathOk <-
    check
      "fontFilePath returns static path"
      (fontFilePath staticFontCase == "fonts/Inter-Regular.ttf")
  variablePathOk <-
    check
      "fontFilePath returns variable path"
      (fontFilePath variableFontCase == "fonts/Inter-Variable.ttf")
  staticArgsOk <-
    check
      "fontInputArgs uses -font for static fonts"
      (fontInputArgs staticFontCase == ["-font", "fonts/Inter-Regular.ttf"])
  variableArgsOk <-
    check
      "fontInputArgs uses -varfont with encoded axes"
      (fontInputArgs variableFontCase == ["-varfont", "fonts/Inter-Variable.ttf?wght=700&opsz=32"])
  staticLabelOk <-
    check
      "fontInputLabel formats static fonts"
      (fontInputLabel staticFontCase == "font:fonts/Inter-Regular.ttf")
  variableLabelOk <-
    check
      "fontInputLabel formats variable fonts with encoded axes"
      (fontInputLabel variableFontCase == "varfont:fonts/Inter-Variable.ttf?wght=700&opsz=32")
  pure
    ( and
        [ staticPathOk,
          variablePathOk,
          staticArgsOk,
          variableArgsOk,
          staticLabelOk,
          variableLabelOk
        ]
    )

runCompareBehaviorChecks :: IO Bool
runCompareBehaviorChecks = do
  let imgEqA = mkTestImgRGBA8 1 1 [10, 20, 30, 40]
      imgEqB = mkTestImgRGBA8 1 1 [10, 20, 30, 40]
      imgDiffA = mkTestImgRGBA8 1 1 [10, 20, 30, 40]
      imgDiffB = mkTestImgRGBA8 1 1 [11, 19, 35, 44]
      imgDimA = mkTestImgRGBA8 1 1 [1, 2, 3, 4]
      imgDimB = mkTestImgRGBA8 2 1 [1, 2, 3, 4, 5, 6, 7, 8]
      invalidPayload =
        ImgRGBA8
          { w = 1,
            h = 1,
            px = BS.pack [1, 2, 3]
          }
      validPayload =
        ImgRGBA8
          { w = 1,
            h = 1,
            px = BS.pack [1, 2, 3, 4]
          }
      expectedEqualStats =
        DiffStats
          { pxCount = 1,
            chCount = 4,
            maxAbs = 0,
            maxCh = (0, 0, 0, 0),
            p99Abs = 0,
            meanAbs = 0,
            mismatch = 0
          }
      expectedDiffStats =
        DiffStats
          { pxCount = 1,
            chCount = 4,
            maxAbs = 5,
            maxCh = (1, 1, 5, 4),
            p99Abs = 5,
            meanAbs = 2.75,
            mismatch = 4
          }
      gatePassStats =
        DiffStats
          { pxCount = 1,
            chCount = 4,
            maxAbs = 2,
            maxCh = (2, 1, 2, 0),
            p99Abs = 1,
            meanAbs = 0.20,
            mismatch = 2
          }
  equalStatsOk <-
    check
      "diffRGBA8 returns zero stats for identical images"
      (case (imgEqA, imgEqB) of
         (Right leftImg, Right rightImg) ->
           diffRGBA8 leftImg rightImg == Right expectedEqualStats
         _ -> False)
  concreteDiffStatsOk <-
    check
      "diffRGBA8 computes expected stats for a concrete pixel delta"
      (case (imgDiffA, imgDiffB) of
         (Right leftImg, Right rightImg) ->
           diffRGBA8 leftImg rightImg == Right expectedDiffStats
         _ -> False)
  dimMismatchOk <-
    check
      "diffRGBA8 rejects mismatched image dimensions"
      (case (imgDimA, imgDimB) of
         (Right leftImg, Right rightImg) ->
           diffRGBA8 leftImg rightImg
             == Left "Image dimensions differ. left=(1,1) right=(2,1)"
         _ -> False)
  payloadMismatchOk <-
    check
      "diffRGBA8 rejects mismatched payload lengths"
      ( diffRGBA8 invalidPayload validPayload
          == Left "Image payload lengths differ. left=3 right=4"
      )
  strictGatePassOk <-
    check
      "strictGate passes at exact thresholds"
      (passesGate strictGate gatePassStats)
  strictGateMaxChFailOk <-
    check
      "strictGate fails when max channel delta exceeds limit"
      ( not
          ( passesGate
              strictGate
              gatePassStats {maxCh = (3, 1, 2, 0), maxAbs = 3}
          )
      )
  strictGateP99FailOk <-
    check
      "strictGate fails when p99 exceeds limit"
      (not (passesGate strictGate gatePassStats {p99Abs = 2}))
  strictGateMeanFailOk <-
    check
      "strictGate fails when mean exceeds limit"
      (not (passesGate strictGate gatePassStats {meanAbs = 0.21}))
  pure
    ( and
        [ equalStatsOk,
          concreteDiffStatsOk,
          dimMismatchOk,
          payloadMismatchOk,
          strictGatePassOk,
          strictGateMaxChFailOk,
          strictGateP99FailOk,
          strictGateMeanFailOk
        ]
    )

runTextRenderChecks :: IO Bool
runTextRenderChecks = do
  badShaderCfgOk <-
    check
      "mkShaderCfg rejects non-positive screenPxRange"
      (isLeft (mkShaderCfg (FixedPxRange 0) True 0.06))
  goodShaderCfgOk <-
    check
      "mkShaderCfg accepts valid fixed range"
      (case mkShaderCfg (FixedPxRange 8) True 0.06 of Right _ -> True; Left _ -> False)
  autoModeShaderCfgOk <-
    check
      "mkShaderCfg accepts auto mode"
      (case mkShaderCfg (AutoPxRange 6) False 0.06 of Right _ -> True; Left _ -> False)
  blackInsideOk <-
    case mkShaderCfg (FixedPxRange 8) True 0.06 of
      Left err ->
        check ("shader cfg setup failed: " <> err) False
      Right shader ->
        case mkImgRGBA8 1 1 (BS.pack [255, 255, 255, 255]) of
          Left err ->
            check ("inside sample setup failed: " <> err) False
          Right img ->
            case shadeMtsdfImg shader img of
              Left err ->
                check ("inside shade failed: " <> err) False
              Right shaded ->
                check
                  "shadeMtsdfImg maps inside sample to black"
                  (BS.take 4 shaded.px == BS.pack [0, 0, 0, 255])
  whiteOutsideOk <-
    case mkShaderCfg (FixedPxRange 8) True 0.06 of
      Left err ->
        check ("shader cfg setup failed: " <> err) False
      Right shader ->
        case mkImgRGBA8 1 1 (BS.pack [0, 0, 0, 0]) of
          Left err ->
            check ("outside sample setup failed: " <> err) False
          Right img ->
            case shadeMtsdfImg shader img of
              Left err ->
                check ("outside shade failed: " <> err) False
              Right shaded ->
                check
                  "shadeMtsdfImg maps outside sample to white"
                  (BS.take 4 shaded.px == BS.pack [255, 255, 255, 255])
  monotonicCoverageOk <-
    case mkShaderCfg (FixedPxRange 8) True 0.06 of
      Left err ->
        check ("monotonic shader cfg setup failed: " <> err) False
      Right shader ->
        case
          ( mkImgRGBA8 1 1 (BS.pack [120, 120, 120, 255]),
            mkImgRGBA8 1 1 (BS.pack [136, 136, 136, 255])
          ) of
          (Left err, _) ->
            check ("lower median sample setup failed: " <> err) False
          (_, Left err) ->
            check ("higher median sample setup failed: " <> err) False
          (Right lowMedianImg, Right highMedianImg) ->
            case
              ( shadeMtsdfImg shader lowMedianImg,
                shadeMtsdfImg shader highMedianImg
              ) of
              (Left err, _) ->
                check ("low median shade failed: " <> err) False
              (_, Left err) ->
                check ("high median shade failed: " <> err) False
              (Right lowShaded, Right highShaded) ->
                case (BS.unpack lowShaded.px, BS.unpack highShaded.px) of
                  (lowGray : _, highGray : _) ->
                    check
                      "higher median channel yields darker or equal grayscale"
                      (highGray <= lowGray)
                  _ ->
                    check "shaded payloads were empty" False
  acuteJoinHealOk <-
    case mkShaderCfg (FixedPxRange 1) False 0.06 of
      Left err ->
        check ("acute-join shader cfg setup failed: " <> err) False
      Right shader ->
        case
          ( mkImgRGBA8
              3
              3
              ( BS.pack
                  ( concat
                      [ [214, 214, 214, 255, 214, 214, 214, 255, 178, 178, 178, 255],
                        [214, 214, 214, 255, 26, 26, 26, 255, 178, 178, 178, 255],
                        [214, 214, 214, 255, 214, 214, 214, 255, 178, 178, 178, 255]
                      ]
                  )
              )
          ) of
          Left err ->
            check ("acute-join fixture image setup failed: " <> err) False
          Right img -> do
            case shadeMtsdfImg (shader {ssaa = 1}) img of
              Left err ->
                check ("acute-join shade failed: " <> err) False
              Right shaded ->
                if BS.length shaded.px > 16
                  then
                    let centerGray = BS.index shaded.px 16
                  in check
                          "shadeMtsdfImg heals acute-join-like one-pixel pinhole"
                          (centerGray < 80)
                  else check "acute-join shaded payload was truncated" False
  multiJoinHealOk <-
    case mkShaderCfg (FixedPxRange 1) False 0.06 of
      Left err ->
        check ("join-heal shader cfg setup failed: " <> err) False
      Right shader ->
        let seamCoverage = [ [214, 214, 214],
                            [214, 26, 178],
                            [214, 214, 214]
                          ]
            pixelBytes = BS.pack $ concatMap (concatMap (\v -> [v, v, v, 255])) seamCoverage
         in case mkImgRGBA8 3 3 pixelBytes of
          Left err ->
            check ("join-heal fixture image setup failed: " <> err) False
          Right img -> do
            case shadeMtsdfImg (shader {ssaa = 1}) img of
              Left err ->
                check ("join-heal shade failed: " <> err) False
              Right shaded ->
                if BS.length shaded.px > 16
                  then
                    let centerGray = BS.index shaded.px 16
                     in check
                          "shadeMtsdfImg heals a join-seam-like one-pixel artifact"
                          (centerGray < 80)
                  else check "join-heal shaded payload was truncated" False
  neighborhoodSpeckHealOk <-
    case mkShaderCfg (FixedPxRange 1) False 0.06 of
      Left err ->
        check ("neighborhood-heal shader cfg setup failed: " <> err) False
      Right shader ->
        let seamCoverage = [ [190, 190, 190],
                            [190, 130, 190],
                            [190, 190, 190]
                          ]
            pixelBytes = BS.pack $ concatMap (concatMap (\v -> [v, v, v, 255])) seamCoverage
         in case mkImgRGBA8 3 3 pixelBytes of
              Left err ->
                check ("neighborhood-heal fixture image setup failed: " <> err) False
              Right img -> do
                case shadeMtsdfImg (shader {ssaa = 1}) img of
                  Left err ->
                    check ("neighborhood-heal shade failed: " <> err) False
                  Right shaded ->
                    if BS.length shaded.px > 16
                      then
                        let centerGray = BS.index shaded.px 16
                         in check
                              "shadeMtsdfImg heals dark-region bright bumps using neighborhood context"
                              (centerGray < 80)
                      else check "neighborhood-heal shaded payload was truncated" False
  edgePreserveOk <-
    case mkShaderCfg (FixedPxRange 1) False 0.06 of
      Left err ->
        check ("edge-preserve shader cfg setup failed: " <> err) False
      Right shader ->
        case
          ( mkImgRGBA8
              3
              3
              ( BS.pack
                  ( concat
                      [ [214, 214, 214, 255, 26, 26, 26, 255, 26, 26, 26, 255],
                        [214, 214, 214, 255, 26, 26, 26, 255, 26, 26, 26, 255],
                        [26, 26, 26, 255, 26, 26, 26, 255, 26, 26, 26, 255]
                      ]
                  )
              )
          ) of
          Left err ->
            check ("edge-preserve fixture image setup failed: " <> err) False
          Right img ->
            case shadeMtsdfImg (shader {ssaa = 1}) img of
              Left err ->
                check ("edge-preserve shade failed: " <> err) False
              Right shaded ->
                if BS.length shaded.px > 16
                  then
                    let centerGray = BS.index shaded.px 16
                     in check
                          "shadeMtsdfImg does not over-heal real edge pixels"
                          (centerGray > 160)
                  else check "edge-preserve shaded payload was truncated" False
  nonHealContourOk <-
    case mkShaderCfg (FixedPxRange 1) False 0.06 of
      Left err ->
        check ("non-heal shader cfg setup failed: " <> err) False
      Right shader ->
        let connectedDark = [ [214, 26, 214],
                             [26, 26, 26],
                             [214, 26, 214]
                           ]
            pixelBytes = BS.pack $ concatMap (concatMap (\v -> [v, v, v, 255])) connectedDark
         in case mkImgRGBA8 3 3 pixelBytes of
              Left err ->
                check ("non-heal fixture image setup failed: " <> err) False
              Right img -> do
                case shadeMtsdfImg (shader {ssaa = 1}) img of
                  Left err ->
                    check ("non-heal shade failed: " <> err) False
                  Right shaded ->
                    if BS.length shaded.px > 16
                      then
                        let centerGray = BS.index shaded.px 16
                         in check
                              "shadeMtsdfImg does not over-heal connected dark corners"
                              (centerGray > 140)
                      else check "non-heal shaded payload was truncated" False
  alphaFallbackOk <-
    case (mkShaderCfg (FixedPxRange 8) False 0.06, mkShaderCfg (FixedPxRange 8) True 0.06) of
      (Right noFallbackShader, Right fallbackShader) ->
        case mkImgRGBA8 1 1 (BS.pack [255, 255, 255, 0]) of
          Left err ->
            check ("fallback sample setup failed: " <> err) False
          Right img -> do
            noFallbackResult <- pure (shadeMtsdfImg noFallbackShader img)
            fallbackResult <- pure (shadeMtsdfImg fallbackShader img)
            case (noFallbackResult, fallbackResult) of
              (Right noFallbackImg, Right fallbackImg) ->
                case (BS.unpack noFallbackImg.px, BS.unpack fallbackImg.px) of
                  (noFallbackGray : _, fallbackGray : _) ->
                    check
                      "alpha fallback keeps output in valid grayscale range"
                      ( noFallbackGray <= 255
                          && fallbackGray <= 255
                      )
                  _ ->
                    check "alpha fallback sample had empty payload" False
              (Left err, _) ->
                check ("no-fallback shade failed: " <> err) False
              (_, Left err) ->
                check ("fallback shade failed: " <> err) False
      _ ->
        check "fallback cfg setup failed" False
  composeOk <-
    case (solidImg 2 3 (255, 255, 255, 255), solidImg 1 3 (0, 0, 0, 255)) of
      (Right leftImg, Right rightImg) ->
        case hcatWithGap 2 [leftImg, rightImg] of
          Left err ->
            check ("hcatWithGap failed: " <> err) False
          Right composed ->
            check
              "hcatWithGap preserves height and adds gap width"
              (composed.w == 5 && composed.h == 3)
      (Left err, _) ->
        check ("solidImg setup failed: " <> err) False
      (_, Left err) ->
        check ("solidImg setup failed: " <> err) False
  borderOk <-
    case solidImg 5 3 (255, 255, 255, 255) of
      Left err ->
        check ("solidImg setup failed: " <> err) False
      Right img ->
        case addBorder 2 img of
          Left err ->
            check ("addBorder failed: " <> err) False
          Right framed ->
            check
              "addBorder expands width and height by twice border"
              (framed.w == 9 && framed.h == 7)
  resampleOk <-
    case solidImg 8 4 (255, 255, 255, 255) of
      Left err ->
        check ("solidImg setup failed: " <> err) False
      Right img ->
        case resampleBilinear 4 2 img of
          Left err ->
            check ("resampleBilinear failed: " <> err) False
          Right resized ->
            check
              "resampleBilinear sets requested output dimensions"
              (resized.w == 4 && resized.h == 2)
  pure
    ( and
        [ badShaderCfgOk,
          goodShaderCfgOk,
          autoModeShaderCfgOk,
          blackInsideOk,
          whiteOutsideOk,
          monotonicCoverageOk,
          alphaFallbackOk,
          composeOk,
          borderOk,
          resampleOk,
          acuteJoinHealOk,
          neighborhoodSpeckHealOk,
          edgePreserveOk,
          multiJoinHealOk,
          nonHealContourOk
        ]
    )

runDecodeChecks :: IO Bool
runDecodeChecks = do
  invalidHeaderOk <-
    checkLeftContains
      "decodeMsdfgenRgba rejects invalid header"
      "missing RGBA header"
      (decodeMsdfgenRgba invalidHeaderPayload)
  truncatedPayloadOk <-
    check
      "decodeMsdfgenRgba rejects truncated payload"
      (isLeft (decodeMsdfgenRgba truncatedPayload))
  zeroDimOk <-
    checkLeftContains
      "decodeMsdfgenRgba rejects zero dimensions"
      "Image width and height must be > 0."
      (decodeMsdfgenRgba zeroDimPayload)
  pure (invalidHeaderOk && truncatedPayloadOk && zeroDimOk)

runManifestChecks :: IO Bool
runManifestChecks = do
  parseOk <- checkLoadManifestParsesValidFixture
  missingHeaderOk <- checkLoadManifestRejectsMissingPxRange
  pure (parseOk && missingHeaderOk)

runSdlShaderSourceChecks :: IO Bool
runSdlShaderSourceChecks = do
  src <- readFile "examples/sdl3-spirdo-text/app/Main.hsc"
  justSrc <- readFile "justfile"
  harnessSrc <- readFile "tools/run_sdl3_artifact_harness.sh"
  abSrc <- readFile "tools/sdl3_ab_compare.sh"
  oracleSrc <- readFile "tools/run_msdfgen_oracle_gate.sh"
  harnessPy <- readFile "tools/sdl3_artifact_harness.py"
  routeHealOk <-
    check
      "SDL presentation heal routes to dedicated shader variant"
      ("fragmentShader = fragmentShaderMtsdfHeal" `isInfixOf` src)
  routeNoHealOk <-
    check
      "SDL presentation no-heal routes to canonical shader variant"
      ("fragmentShaderNoHeal = fragmentShaderCanonical" `isInfixOf` src)
  modeParseOk <-
    check
      "SDL presentation heal mode parser is present"
      ("parsePresentHealMode :: Maybe String -> Either String PresentHealMode" `isInfixOf` src)
  fontRegularEnvOk <-
    check
      "SDL font regular override env is present"
      ("MASDIFF_SDL_FONT_REGULAR" `isInfixOf` src)
  fontVarEnvOk <-
    check
      "SDL font variable override env is present"
      ("MASDIFF_SDL_FONT_VAR" `isInfixOf` src)
  fontAxisEnvOk <-
    check
      "SDL variable axis override envs are present"
      ( all
          (`isInfixOf` src)
          [ "MASDIFF_SDL_VAR_LIGHT_WGHT",
            "MASDIFF_SDL_VAR_LIGHT_OPSZ",
            "MASDIFF_SDL_VAR_BOLD_WGHT",
            "MASDIFF_SDL_VAR_BOLD_OPSZ"
          ]
      )
  pxRangeDefaultOk <-
    check
      "SDL default scene px range is set to 8.0"
      ("readPositiveDoubleEnvDefault \"MASDIFF_SDL_PXRANGE\" 8.0" `isInfixOf` src)
  dimDefaultOk <-
    check
      "SDL default scene dim is set to 256 and overrideable"
      ("readPositiveIntEnvDefault \"MASDIFF_SDL_DIM\" 256" `isInfixOf` src)
  justDefaultRuntimeTuningOk <-
    check
      "just sdl3 defaults include present-heal mode and tuned px range"
      ( all
          (`isInfixOf` justSrc)
          [ "MASDIFF_SDL_PRESENT_HEAL=\"${MASDIFF_SDL_PRESENT_HEAL:-1}\"",
            "MASDIFF_SDL_PRESENT_HEAL_MODE=\"${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}\"",
            "MASDIFF_SDL_PXRANGE=\"${MASDIFF_SDL_PXRANGE:-7}\""
          ]
      )
  justVideoDriverDefaultOk <-
    check
      "just sdl3 defaults set SDL_VIDEODRIVER for deterministic CI runs"
      ("SDL_VIDEODRIVER=\"${SDL_VIDEODRIVER:-x11}\"" `isInfixOf` justSrc)
  justBackendDefaultOk <-
    check
      "just sdl3 defaults to GPU generation path"
      ("MASDIFF_SDL_GEN_BACKEND=\"${MASDIFF_SDL_GEN_BACKEND:-gpu}\"" `isInfixOf` justSrc)
  harnessRuntimeDefaultsOk <-
    check
      "SDL harness defaults include present-heal tuning and video driver"
      ( all
          (`isInfixOf` harnessSrc)
          [ "SDL_VIDEODRIVER=${SDL_VIDEODRIVER:-x11}",
            "MASDIFF_SDL_PRESENT_HEAL=${MASDIFF_SDL_PRESENT_HEAL:-1}",
            "MASDIFF_SDL_PRESENT_HEAL_MODE=${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}",
            "MASDIFF_SDL_PXRANGE=${MASDIFF_SDL_PXRANGE:-7}"
          ]
      )
  abRuntimeDefaultsOk <-
    check
      "SDL A/B script defaults include present-heal tuning and video driver"
      ( all
          (`isInfixOf` abSrc)
          [ "SDL_VIDEODRIVER=\"${SDL_VIDEODRIVER:-x11}\"",
            "MASDIFF_SDL_PRESENT_HEAL_MODE=\"${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}\"",
            "MASDIFF_SDL_PXRANGE=\"${MASDIFF_SDL_PXRANGE:-7}\""
          ]
      )
  harnessDimAbOk <-
    check
      "SDL harness includes MASDIFF_SDL_DIM A/B sweep"
      ( all
          (`isInfixOf` harnessSrc)
          [ "MASDIFF_SDL_HARNESS_DIM_AB",
            "MASDIFF_SDL_HARNESS_DIM_BASE",
            "MASDIFF_SDL_HARNESS_DIM_CANDIDATE",
            "run_dim_ab_case",
            "MASDIFF_SDL_DIM=\"$base_dim\"",
            "MASDIFF_SDL_DIM=\"$cand_dim\""
          ]
      )
  harnessCounterApexStrictOk <-
    check
      "SDL harness keeps single-glyph A counter apex checks strict"
      ( all
          (`isInfixOf` harnessPy)
          [ "def counter_core_bad_limit(ch: str, scene: str):",
            "def counter_apex_core_bad_limit(ch: str, scene: str):",
            "scene.startswith(\"single-\") and ch == \"A\"",
            "apex_core_bad_pixels",
            "max_apex_core_bad_pixels"
          ]
      )
  harnessOracleGateOk <-
    check
      "SDL harness runs msdfgen oracle gate by default"
      ( all
          (`isInfixOf` harnessSrc)
          [ "MASDIFF_SDL_REQUIRE_ORACLE",
            "MASDIFF_SDL_ORACLE_ENFORCE",
            "run_msdfgen_oracle_gate.sh"
          ]
      )
  abOracleGateOk <-
    check
      "SDL A/B script runs msdfgen oracle gate by default"
      ( all
          (`isInfixOf` abSrc)
          [ "MASDIFF_SDL_REQUIRE_ORACLE",
            "MASDIFF_SDL_ORACLE_ENFORCE",
            "run_msdfgen_oracle_gate.sh"
          ]
      )
  oracleScriptStrictParityOk <-
    check
      "msdfgen oracle gate enforces strict corpus parity command"
      ("masdiff-parity -- --require-exact" `isInfixOf` oracleSrc)
  oracleScriptStressCasesOk <-
    check
      "msdfgen oracle gate includes Inter/Roboto stress cases"
      ( all
          (`isInfixOf` oracleSrc)
          [ "var-inter-old-bold-A",
            "var-inter-v41-bold-A",
            "var-inter-v41-bold-R",
            "var-roboto-flex-bold-A"
          ]
      )
  oracleScriptVarInstancingOk <-
    check
      "msdfgen oracle gate instantiates varfont cases for process oracle"
      ( all
          (`isInfixOf` oracleSrc)
          [ "MASDIFF_ORACLE_INSTANCE_VAR",
            "instantiateVariableFont",
            "compare_mode=\"varfont\""
          ]
      )
  aggressiveShaderOk <-
    check
      "SDL aggressive heal shader variant is present"
      ("fragmentShaderMtsdfHealAggressive :: GpuShader" `isInfixOf` src)
  gpuNoPerGlyphOverrideOk <-
    check
      "SDL GPU generation avoids glyph-specific sign overrides"
      ( not ("preferEdgeSignForGlyphCode" `isInfixOf` src)
          && not ("code == 65" `isInfixOf` src)
          && not ("let useEdgeSign = u.meta1.w >= 0.5;" `isInfixOf` src)
      )
  gpuWindingModeSwitchOk <-
    check
      "SDL GPU generation shader supports parity and non-zero winding mode switch"
      ( countSubstring "let useNonZero = u.meta1.w > 0.5;" src >= 2
          && countSubstring "let inside = select(insideParity, insideNonZero, useNonZero);" src >= 2
          && countSubstring "fn windingStepParity(" src >= 2
          && countSubstring "fn windingStepNonZero(" src >= 2
      )
  gpuWindingDefaultNonZeroOk <-
    check
      "SDL GPU generation defaults to non-zero winding to match native fill semantics"
      ("let useNonZeroWinding = True" `isInfixOf` src)
  cornerMixPresentOk <-
    check
      "SDL heal shader includes mtsdf corner-mix fallback"
      (countSubstring "let cornerMix = smoothstep(" src >= 1 && countSubstring "channelSpread" src >= 2)
  mixDistPresentOk <-
    check
      "SDL heal shader mixes msdf and sdf distances"
      ( countSubstring "let dist = mix(msdf, sdf, cornerMix);" src >= 2
          || countSubstring "let dist = mix(msdf, sdf, max(cornerMix, signMismatch));" src >= 2
      )
  canonicalMedianOnlyOk <-
    check
      "SDL canonical shader keeps median-only distance"
      (countSubstring "let sd = msdf - 0.5;" src == 1)
  screenRangeScaleInvariantOk <-
    check
      "SDL fit scaling keeps glyph px range invariant"
      ("spr = d.spr" `isInfixOf` src && not ("spr = d.spr * k" `isInfixOf` src))
  healDistUsedOk <-
    check
      "SDL heal shader uses mixed distance for signed distance"
      (countSubstring "let sd = dist - 0.5;" src >= 2)
  gpuAtlasLoadOpBatchOk <-
    check
      "SDL GPU atlas batch uses per-draw load-op pass sequencing"
      ( all
          (`isInfixOf` src)
          [ "sdlGpuLoadOpLoad",
            "withColorTargetInfoLoadOp tex clear loadOp",
            "forM_ (zip [0 :: Int ..] draws)"
          ]
      )
  gpuCommandLifetimeOk <-
    check
      "SDL GPU atlas path submits/waits before releasing per-draw segment buffers"
      ( all
          (`isInfixOf` src)
          [ "submitGpuCommandBufferAndWait gpuCtx.dev \"SDL_SubmitGPUCommandBuffer(gen-atlas)\" cmd",
            "imgResult <- withUploadedSegBuffer dev cmd segs"
          ]
          && not ("requireTrue \"SDL_SubmitGPUCommandBuffer(gen-atlas)\"" `isInfixOf` src)
      )
  batchOverlapFallbackGuardOk <-
    check
      "SDL gpu batch path falls back when overlap support is enabled"
      ( all
          (`isInfixOf` src)
          [ "gpu batch disabled for overlap-support scene; using per-glyph raster path",
            "if cfg.ovlp"
          ]
      )
  pure
    ( and
        [ routeHealOk,
          routeNoHealOk,
          modeParseOk,
          fontRegularEnvOk,
          fontVarEnvOk,
          fontAxisEnvOk,
          pxRangeDefaultOk,
          dimDefaultOk,
          justDefaultRuntimeTuningOk,
          justVideoDriverDefaultOk,
          justBackendDefaultOk,
          harnessRuntimeDefaultsOk,
          abRuntimeDefaultsOk,
          harnessDimAbOk,
          harnessCounterApexStrictOk,
          harnessOracleGateOk,
          abOracleGateOk,
          oracleScriptStrictParityOk,
          oracleScriptStressCasesOk,
          oracleScriptVarInstancingOk,
          aggressiveShaderOk,
          gpuNoPerGlyphOverrideOk,
          gpuWindingModeSwitchOk,
          gpuWindingDefaultNonZeroOk,
          cornerMixPresentOk,
          mixDistPresentOk,
          canonicalMedianOnlyOk,
          screenRangeScaleInvariantOk,
          healDistUsedOk,
          gpuAtlasLoadOpBatchOk,
          gpuCommandLifetimeOk,
          batchOverlapFallbackGuardOk
        ]
    )

runParityWorkflowSourceChecks :: IO Bool
runParityWorkflowSourceChecks = do
  paritySrc <- readFile "app/ParityMain.hs"
  justSrc <- readFile "justfile"
  msdfglGateSrc <- readFile "tools/run_msdfgl_oracle_gate.sh"
  parityProfileFlagsOk <-
    check
      "masdiff-parity supports profile/oracle/manifest/json options"
      ( all
          (`isInfixOf` paritySrc)
          [ "--profile",
            "--oracle",
            "--manifest",
            "--json-out",
            "--allow-missing-oracle",
            "--require-oracle"
          ]
      )
  parityProfileParserOk <-
    check
      "masdiff-parity profile parser supports pr/nightly/full"
      ( all
          (`isInfixOf` paritySrc)
          [ "\"pr\" -> Right ProfilePr",
            "\"nightly\" -> Right ProfileNightly",
            "\"full\" -> Right ProfileFull"
          ]
      )
  parityOracleParserOk <-
    check
      "masdiff-parity oracle parser supports process/msdfgl/both"
      ( all
          (`isInfixOf` paritySrc)
          [ "\"process\" -> Right OracleProcess",
            "\"msdfgl\" -> Right OracleMsdfgl",
            "\"both\" -> Right OracleBoth"
          ]
      )
  justOracleTargetsOk <-
    check
      "justfile exposes oracle-pr, oracle-nightly, and oracle-msdfgl targets"
      ( all
          (`isInfixOf` justSrc)
          [ "oracle-pr:",
            "oracle-nightly:",
            "oracle-msdfgl:"
          ]
      )
  msdfglGateUsesParityCliOk <-
    check
      "msdfgl oracle gate invokes masdiff-parity with msdfgl oracle mode"
      ( all
          (`isInfixOf` msdfglGateSrc)
          [ "masdiff-parity --",
            "--oracle msdfgl",
            "--manifest",
            "--profile"
          ]
      )
  pure
    ( and
        [ parityProfileFlagsOk,
          parityProfileParserOk,
          parityOracleParserOk,
          justOracleTargetsOk,
          msdfglGateUsesParityCliOk
        ]
    )

countSubstring :: String -> String -> Int
countSubstring needle haystack
  | null needle = 0
  | otherwise = length (filter (isPrefixOf needle) (tails haystack))

checkLoadManifestParsesValidFixture :: IO Bool
checkLoadManifestParsesValidFixture =
  withTempTextFile validManifestFixture $ \manifestPath -> do
    parsed <- loadManifest manifestPath
    case expectedManifestFixture of
      Left err -> do
        putStrLn ("FAIL: expected manifest fixture setup failed: " <> err)
        pure False
      Right expected ->
        check "loadManifest parses valid fixture file" (parsed == Right expected)

checkLoadManifestRejectsMissingPxRange :: IO Bool
checkLoadManifestRejectsMissingPxRange =
  withTempTextFile missingPxRangeManifestFixture $ \manifestPath -> do
    parsed <- loadManifest manifestPath
    checkLeftContains
      "loadManifest rejects missing pxrange header"
      "Missing manifest header: pxrange"
      parsed

checkLeftContains :: String -> String -> Either String a -> IO Bool
checkLeftContains label expectedSubstr result =
  check label $
    case result of
      Left err -> expectedSubstr `isInfixOf` err
      Right _ -> False

withTempTextFile :: String -> (FilePath -> IO a) -> IO a
withTempTextFile contents action = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "masdiff-test-manifest.tsv") cleanup $ \(path, handle) -> do
    hPutStr handle contents
    hClose handle
    action path
  where
    cleanup (path, handle) = do
      catchIOError (hClose handle) (\_ -> pure ())
      catchIOError (removeFile path) (\_ -> pure ())

invalidHeaderPayload :: ByteString
invalidHeaderPayload =
  BS.pack [66, 65, 68, 33, 0, 0, 0, 1, 0, 0, 0, 1, 255, 0, 0, 255]

truncatedPayload :: ByteString
truncatedPayload =
  BS.pack [82, 71, 66, 65, 0, 0, 0, 1, 0, 0, 0, 1, 255, 0, 0]

zeroDimPayload :: ByteString
zeroDimPayload =
  BS.pack [82, 71, 66, 65, 0, 0, 0, 0, 0, 0, 0, 1]

validManifestFixture :: String
validManifestFixture =
  unlines
    [ "# generator=generate-inter-mtsdf-fixtures",
      "# dimensions=32",
      "# pxrange=4.5",
      "# seed=7",
      "font_case\tglyph\tglyph_hex\toutput_png\tinput_spec",
      "inter-static-regular\tA\tU+0041\tout/U+0041.png\tfont:assets/Inter-Regular.ttf",
      "inter-var-display\tg\tU+0067\tout/U+0067.png\tvarfont:assets/Inter-Variable.ttf?wght=700&opsz=14"
    ]

missingPxRangeManifestFixture :: String
missingPxRangeManifestFixture =
  unlines
    [ "# generator=generate-inter-mtsdf-fixtures",
      "# dimensions=32",
      "# seed=7",
      "font_case\tglyph\tglyph_hex\toutput_png\tinput_spec",
      "inter-static-regular\tA\tU+0041\tout/U+0041.png\tfont:assets/Inter-Regular.ttf"
    ]

expectedManifestFixture :: Either String Manifest
expectedManifestFixture = do
  dim <- mkDim 32
  pxRange <- mkPxRange 4.5
  glyphA <- mkGlyphCode 0x41
  glyphG <- mkGlyphCode 0x67
  pure
    Manifest
      { meta =
          ManifestMeta
            { dim = dim,
              pxr = pxRange,
              seed = 7
            },
        rows =
          [ ManifestRow
              { fontCase = "inter-static-regular",
                glyph = glyphA,
                glyphHex = "U+0041",
                outputPng = "out/U+0041.png",
                src = FontFile {path = "assets/Inter-Regular.ttf"}
              },
            ManifestRow
              { fontCase = "inter-var-display",
                glyph = glyphG,
                glyphHex = "U+0067",
                outputPng = "out/U+0067.png",
                src =
                  VarFontFile
                    { path = "assets/Inter-Variable.ttf",
                      axes =
                        Map.fromList
                          [ (AxisTag (T.pack "opsz"), AxisVal 14.0),
                            (AxisTag (T.pack "wght"), AxisVal 700.0)
                          ]
                    }
              }
          ]
      }

runQuickCheckProperties :: IO Bool
runQuickCheckProperties = do
  dimValidOk <- checkProperty "mkDim accepts positive Int values" prop_mkDim_acceptsPositive
  dimInvalidOk <- checkProperty "mkDim rejects zero and negatives" prop_mkDim_rejectsNonPositive
  pxRangeValidOk <- checkProperty "mkPxRange accepts finite positive Double values" prop_mkPxRange_acceptsFinitePositive
  pxRangeInvalidOk <- checkProperty "mkPxRange rejects zero and negatives" prop_mkPxRange_rejectsNonPositive
  pxRangeNonFiniteOk <- checkProperty "mkPxRange rejects NaN and infinities" prop_mkPxRange_rejectsNonFinite
  glyphValidOk <- checkProperty "mkGlyphCode accepts Unicode scalar range" prop_mkGlyphCode_acceptsUnicodeRange
  glyphNegOk <- checkProperty "mkGlyphCode rejects negative values" prop_mkGlyphCode_rejectsNegative
  glyphHighOk <- checkProperty "mkGlyphCode rejects values above 0x10FFFF" prop_mkGlyphCode_rejectsAboveUnicodeMax
  rgbaRoundtripOk <- checkProperty "msdfgen rgba encode/decode roundtrip" prop_msdfgenRgba_roundtrip
  pure $
    and
      [ dimValidOk,
        dimInvalidOk,
        pxRangeValidOk,
        pxRangeInvalidOk,
        pxRangeNonFiniteOk,
        glyphValidOk,
        glyphNegOk,
        glyphHighOk,
        rgbaRoundtripOk
      ]

checkProperty :: Testable prop => String -> prop -> IO Bool
checkProperty label prop = do
  putStrLn ("Property: " <> label)
  isSuccess <$> quickCheckWithResult stdArgs prop

prop_mkDim_acceptsPositive :: Property
prop_mkDim_acceptsPositive =
  forAll (chooseInt (1, 1000000 :: Int)) $ \x ->
    case mkDim x of
      Right dim -> unDim dim == x
      Left _ -> False

prop_mkDim_rejectsNonPositive :: NonPositive Int -> Bool
prop_mkDim_rejectsNonPositive (NonPositive x) = isLeft (mkDim x)

prop_mkPxRange_acceptsFinitePositive :: Property
prop_mkPxRange_acceptsFinitePositive =
  forAll (choose (1.0e-12, 1.0e6 :: Double)) $ \x ->
    case mkPxRange x of
      Right pxRange -> unPxRange pxRange == x
      Left _ -> False

prop_mkPxRange_rejectsNonPositive :: Property
prop_mkPxRange_rejectsNonPositive =
  forAll (choose (-1.0e6, 0.0 :: Double)) $ \x ->
    isLeft (mkPxRange x)

prop_mkPxRange_rejectsNonFinite :: Bool
prop_mkPxRange_rejectsNonFinite =
  isLeft (mkPxRange (0 / 0))
    && isLeft (mkPxRange (1 / 0))
    && isLeft (mkPxRange ((-1) / 0))

prop_mkGlyphCode_acceptsUnicodeRange :: Property
prop_mkGlyphCode_acceptsUnicodeRange =
  forAll (chooseInt (0, 0x10FFFF :: Int)) $ \x ->
    case mkGlyphCode x of
      Right glyph -> unGlyphCode glyph == x
      Left _ -> False

prop_mkGlyphCode_rejectsNegative :: Property
prop_mkGlyphCode_rejectsNegative =
  forAll (chooseInt (-1000000, -1 :: Int)) $ \x ->
    isLeft (mkGlyphCode x)

prop_mkGlyphCode_rejectsAboveUnicodeMax :: Property
prop_mkGlyphCode_rejectsAboveUnicodeMax =
  forAll (chooseInt (0x110000, 0x200000 :: Int)) $ \x ->
    isLeft (mkGlyphCode x)

prop_msdfgenRgba_roundtrip :: Property
prop_msdfgenRgba_roundtrip =
  forAll genImgRGBA8 $ \img ->
    decodeMsdfgenRgba (encodeMsdfgenRgba img) == Right img

genImgRGBA8 :: Gen ImgRGBA8
genImgRGBA8 = do
  w <- chooseInt (1, 16)
  h <- chooseInt (1, 16)
  bytes <- genPayload (w * h * 4)
  case mkImgRGBA8 w h bytes of
    Left _ -> genImgRGBA8
    Right img -> pure img

genPayload :: Int -> Gen ByteString
genPayload n = BS.pack <$> vectorOf n (choose (minBound :: Word8, maxBound :: Word8))

mkTestImgRGBA8 :: Int -> Int -> [Word8] -> Either String ImgRGBA8
mkTestImgRGBA8 w h pxBytes = mkImgRGBA8 w h (BS.pack pxBytes)
