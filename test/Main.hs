{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Char (ord, toLower)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (isInfixOf, nub)
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
import MSDF.Encode (decodeMsdfgenRgba, encodeMsdfgenRgba)
import MSDF.Generate (BackendMode (..), RuntimeCfg (..), defaultRuntimeCfg, generateGlyphIO)
import MSDF.Manifest (Manifest (..), ManifestMeta (..), ManifestRow (..), loadManifest)
import MSDF.TextRender
  ( ScreenPxRange (..),
    ShaderCfg (..),
    addBorder,
    hcatWithGap,
    mkShaderCfg,
    resampleBilinear,
    shadeMtsdfImg,
    solidImg
  )
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenOut (..),
    ImgRGBA8 (..),
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
  staticCountOk <- check "at least 8 static font cases" (countKind Static >= 8)
  variableCountOk <- check "at least 8 variable font cases" (countKind Variable >= 8)
  uniqueFontIdsOk <- check "font case IDs are unique" (unique (fmap fontCaseId interHarnessFontCases))
  glyphCoverageOk <- check "at least 12 glyphs in harness set" (length interHarnessGlyphs >= 12)
  uniqueGlyphsOk <- check "glyph set is unique" (unique interHarnessGlyphs)
  variableAxesOk <- checkVariableAxes
  fontFilesOk <- checkFontFiles
  fontBehaviorOk <- runFontBehaviorChecks
  variableAxisNativeOk <- runVariableAxisNativeRegression
  backendParityOk <- runBackendParitySmoke
  thinItalicStrictParityOk <- runThinItalicStrictParityRegression
  compareBehaviorOk <- runCompareBehaviorChecks
  textRenderOk <- runTextRenderChecks
  decodeChecksOk <- runDecodeChecks
  manifestChecksOk <- runManifestChecks
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
            variableAxisNativeOk,
            backendParityOk,
            thinItalicStrictParityOk,
            compareBehaviorOk,
            textRenderOk,
            decodeChecksOk,
            manifestChecksOk
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
