{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Font (FontCase (..), FontKind (..), FontSource (..), interHarnessFontCases, interHarnessGlyphs, interOracleFontCases)
import MSDF.Compare (DiffStats (..), diffRGBA8, passesGate, strictGate)
import MSDF.Encode (readPngRGBA8File)
import MSDF.Generate (BackendMode (..), RuntimeCfg (..), defaultRuntimeCfg, generateGlyphIO)
import MSDF.Manifest (Manifest (..), ManifestRow (..), loadManifest, manifestCfg)
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    GlyphCode,
    ImgRGBA8 (..),
    Metrics (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkPxRange,
    unGlyphCode,
  )
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (isRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Profile = ProfilePr | ProfileNightly | ProfileFull
  deriving stock (Eq, Show)

data OracleMode = OracleProcess | OracleMsdfgl | OracleBoth
  deriving stock (Eq, Show)

data OracleProvider = ProviderProcess | ProviderMsdfgl
  deriving stock (Eq, Show)

data Gate = GateStrict | GateCoverage
  deriving stock (Eq, Show)

data CaseStatus = CasePass | CaseFail | CaseSkip
  deriving stock (Eq, Show)

data SourceClass = SourceStatic | SourceVariable | SourceManifest
  deriving stock (Eq, Show)

data CliCfg = CliCfg
  { maxCases :: Maybe Int,
    requireExact :: Bool,
    verbose :: Bool,
    profile :: Profile,
    oracle :: OracleMode,
    manifestPath :: Maybe FilePath,
    jsonOut :: Maybe FilePath,
    allowMissingOracle :: Bool
  }
  deriving stock (Eq, Show)

data CaseRow = CaseRow
  { caseId :: String,
    glyphHex :: String,
    src :: FontSrc,
    glyph :: GlyphCode,
    cfg :: GenCfg,
    gate :: Gate,
    sourceClass :: SourceClass,
    referencePng :: Maybe FilePath
  }
  deriving stock (Eq, Show)

data Failure = Failure
  { provider :: OracleProvider,
    caseId :: String,
    glyph :: String,
    gate :: Gate,
    reason :: String
  }
  deriving stock (Eq, Show)

data CaseResult = CaseResult
  { provider :: OracleProvider,
    caseId :: String,
    glyph :: String,
    gate :: Gate,
    status :: CaseStatus,
    sourceClass :: SourceClass,
    exactMatch :: Maybe Bool,
    maxAbs :: Maybe Int,
    shapeDiffRatio :: Maybe Double,
    metricsMaxDelta :: Maybe Double,
    alphaMedianDelta :: Maybe Double,
    reason :: Maybe String
  }
  deriving stock (Eq, Show)

data ManifestBundle = ManifestBundle
  { path :: FilePath,
    manifest :: Manifest
  }
  deriving stock (Eq, Show)

data Summary = Summary
  { checked :: Int,
    strictFailed :: Int,
    coverageFailed :: Int,
    exactMismatch :: Int,
    exactMismatchStrict :: Int,
    worstMaxAbs :: Int,
    skipped :: Int,
    failures :: [Failure],
    results :: [CaseResult]
  }
  deriving stock (Eq, Show)

data CoverageStats = CoverageStats
  { shapeDiffRatio :: Double,
    alphaMedianDelta :: Double
  }
  deriving stock (Eq, Show)

data OracleFetch
  = OracleReady ImgRGBA8 (Maybe Metrics)
  | OracleSkip String
  | OracleFailure String
  deriving stock (Eq, Show)

main :: IO ()
main = do
  cli <- either failWith pure . parseArgs =<< getArgs
  manifestBundle <- loadManifestBundle cli.manifestPath
  processBin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
  let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = processBin}
  processVarAxisSupportResult <- probeProcessVarAxisSupport processRuntime
  processVarAxisSupported <-
    case processVarAxisSupportResult of
      Left err -> do
        putStrLn ("INFO: process variable-axis capability probe failed; treating as unsupported for coverage checks: " <> err)
        pure False
      Right supported -> do
        putStrLn
          ( if supported
              then "INFO: process variable-axis support detected."
              else "INFO: process variable-axis support not detected; variable coverage rows will be skipped."
          )
        pure supported
  plans <- either failWith pure (buildProviderPlans cli manifestBundle)
  summaries <- traverse (runProviderPlan cli nativeRuntime processRuntime processVarAxisSupported) plans
  let summary = foldl' mergeSummary emptySummary summaries
  maybe (pure ()) (\path -> writeFile path (renderSummaryJson summary)) cli.jsonOut
  report summary
  if shouldFail cli summary
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess

shouldFail :: CliCfg -> Summary -> Bool
shouldFail cli summary =
  summary.strictFailed > 0
    || summary.coverageFailed > 0
    || (cli.requireExact && summary.exactMismatchStrict > 0)

loadManifestBundle :: Maybe FilePath -> IO (Maybe ManifestBundle)
loadManifestBundle pathOpt =
  case pathOpt of
    Nothing -> pure Nothing
    Just path -> do
      exists <- doesFileExist path
      if not exists
        then failWith ("Manifest file does not exist: " <> path)
        else do
          parsed <- loadManifest path
          case parsed of
            Left err ->
              failWith ("Failed to parse manifest: " <> err)
            Right manifest ->
              pure
                ( Just
                    ManifestBundle
                      { path = path,
                        manifest = manifest
                      }
                )

buildProviderPlans :: CliCfg -> Maybe ManifestBundle -> Either String [(OracleProvider, Either String [CaseRow])]
buildProviderPlans cli manifestBundle =
  case cli.oracle of
    OracleProcess -> do
      processRows <- buildProcessRows cli.profile manifestBundle
      let selectedProcessRows = maybe processRows (`take` processRows) cli.maxCases
      pure [(ProviderProcess, Right selectedProcessRows)]
    OracleMsdfgl -> do
      msdfglPlan <- buildMsdfglPlan cli manifestBundle
      pure [(ProviderMsdfgl, msdfglPlan)]
    OracleBoth -> do
      processRows <- buildProcessRows cli.profile manifestBundle
      let selectedProcessRows = maybe processRows (`take` processRows) cli.maxCases
      msdfglPlan <- buildMsdfglPlan cli manifestBundle
      pure [(ProviderProcess, Right selectedProcessRows), (ProviderMsdfgl, msdfglPlan)]

buildMsdfglPlan :: CliCfg -> Maybe ManifestBundle -> Either String (Either String [CaseRow])
buildMsdfglPlan cli manifestBundle =
  case manifestBundle of
    Nothing ->
      let msg = "msdfgl oracle skipped: no manifest supplied via --manifest."
       in if cli.allowMissingOracle
            then Right (Left msg)
            else Left msg
    Just bundle ->
      let rows = mkManifestRows GateCoverage bundle
          selectedRows = maybe rows (`take` rows) cli.maxCases
       in if null selectedRows
            then
              let msg = "msdfgl oracle skipped: manifest has no rows."
               in if cli.allowMissingOracle
                    then Right (Left msg)
                    else Left msg
            else Right (Right selectedRows)

buildProcessRows :: Profile -> Maybe ManifestBundle -> Either String [CaseRow]
buildProcessRows profile manifestBundle = do
  profileRows <- mkProfileRows profile
  let manifestRows =
        case (profile, manifestBundle) of
          (ProfileFull, Just bundle) -> mkManifestRows GateCoverage bundle
          _ -> []
  pure (profileRows <> manifestRows)

mkProfileRows :: Profile -> Either String [CaseRow]
mkProfileRows profile = do
  let (fontCases, glyphs, cfgSpecs) =
        case profile of
          ProfilePr ->
            (interOracleFontCases, interHarnessGlyphs, [(64, 8.0)])
          ProfileNightly ->
            (nightlyFontCases, nightlyGlyphs, nightlyCfgSpecs)
          ProfileFull ->
            (nightlyFontCases, nightlyGlyphs, nightlyCfgSpecs)
  rows <- traverse (rowsForFontCase cfgSpecs profile glyphs) fontCases
  pure (concat rows)

rowsForFontCase :: [(Int, Double)] -> Profile -> [Char] -> FontCase -> Either String [CaseRow]
rowsForFontCase cfgSpecs profile glyphs fontCase = do
  src <- fontCaseToFontSrc fontCase
  let mkGlyphRow glyphChar = do
        glyph <- mkGlyphCode (ord glyphChar)
        traverse (mkCfgRow glyph) cfgSpecs
      mkCfgRow glyphCode (dimPx, pxrVal) = do
        cfg <- mkCfg dimPx pxrVal
        let gate =
              case profile of
                ProfilePr -> GateStrict
                ProfileNightly -> classifyNightlyGate fontCase dimPx pxrVal
                ProfileFull -> classifyNightlyGate fontCase dimPx pxrVal
            sourceClass =
              case fontCase.fontCaseKind of
                Static -> SourceStatic
                Variable -> SourceVariable
        pure
          CaseRow
            { caseId = fontCase.fontCaseId <> "@d" <> show dimPx <> "-p" <> pxRangeTag pxrVal,
              glyphHex = printf "U+%04X" (unGlyphCode glyphCode),
              src = src,
              glyph = glyphCode,
              cfg = cfg,
              gate = gate,
              sourceClass = sourceClass,
              referencePng = Nothing
            }
  glyphRows <- traverse mkGlyphRow glyphs
  pure (concat glyphRows)

classifyNightlyGate :: FontCase -> Int -> Double -> Gate
classifyNightlyGate fontCase dimPx pxrVal
  | fontCase.fontCaseKind == Static
      && isStableStrictCaseId fontCase.fontCaseId
      && dimPx == 64
      && approxEq pxrVal 8.0 =
      GateStrict
  | otherwise = GateCoverage

isStableStrictCaseId :: String -> Bool
isStableStrictCaseId caseId = caseId `elem` strictStableCaseIds

strictStableCaseIds :: [String]
strictStableCaseIds =
  fmap fontCaseId $
    filter
      (\fontCase -> fontCase.fontCaseKind == Static)
      interOracleFontCases

mkManifestRows :: Gate -> ManifestBundle -> [CaseRow]
mkManifestRows gate bundle =
  fmap mkRow bundle.manifest.rows
  where
    cfg = manifestCfg bundle.manifest.meta
    mkRow manifestRow =
      CaseRow
        { caseId = manifestRow.fontCase <> "@manifest",
          glyphHex = manifestRow.glyphHex,
          src = manifestRow.src,
          glyph = manifestRow.glyph,
          cfg = cfg,
          gate = gate,
          sourceClass = SourceManifest,
          referencePng = Just (resolveManifestOutputPath bundle.path manifestRow.outputPng)
        }

resolveManifestOutputPath :: FilePath -> FilePath -> FilePath
resolveManifestOutputPath manifestPath outputPath =
  if isRelative outputPath
    then takeDirectory manifestPath </> outputPath
    else outputPath

fontCaseToFontSrc :: FontCase -> Either String FontSrc
fontCaseToFontSrc fontCase =
  case fontCase.fontCaseSource of
    StaticFont path ->
      Right FontFile {path = path}
    VariableFont path axes -> do
      axisPairs <- traverse parseAxis axes
      pure
        VarFontFile
          { path = path,
            axes = Map.fromList axisPairs
          }
  where
    parseAxis (name, rawValue) =
      case readMaybe rawValue of
        Just value | isFinite value ->
          Right (AxisTag (T.pack name), AxisVal value)
        _ ->
          Left
            ( "Invalid variable axis value for case "
                <> fontCase.fontCaseId
                <> ": "
                <> name
                <> "="
                <> rawValue
            )

runProviderPlan :: CliCfg -> RuntimeCfg -> RuntimeCfg -> Bool -> (OracleProvider, Either String [CaseRow]) -> IO Summary
runProviderPlan cli nativeRuntime processRuntime processVarAxisSupported (provider, plan) =
  case plan of
    Left skipReason ->
      pure (addCaseResult emptySummary (mkSkipResult provider skipReason))
    Right rows ->
      foldl'
        (\ioSummary row -> ioSummary >>= runCase cli nativeRuntime processRuntime provider processVarAxisSupported row)
        (pure emptySummary)
        rows

runCase :: CliCfg -> RuntimeCfg -> RuntimeCfg -> OracleProvider -> Bool -> CaseRow -> Summary -> IO Summary
runCase cli nativeRuntime processRuntime provider processVarAxisSupported row summary = do
  if shouldSkipVariableCoverage provider processVarAxisSupported row
    then
      pure $
        addCaseResult
          summary
          CaseResult
            { provider = provider,
              caseId = row.caseId,
              glyph = row.glyphHex,
              gate = row.gate,
              status = CaseSkip,
              sourceClass = row.sourceClass,
              exactMatch = Nothing,
              maxAbs = Nothing,
              shapeDiffRatio = Nothing,
              metricsMaxDelta = Nothing,
              alphaMedianDelta = Nothing,
              reason = Just "Skipped variable coverage row because process var-axis behavior is unavailable."
            }
    else pure summary
    >>= \summary0 -> runCaseInner cli nativeRuntime processRuntime provider row summary0

runCaseInner :: CliCfg -> RuntimeCfg -> RuntimeCfg -> OracleProvider -> CaseRow -> Summary -> IO Summary
runCaseInner cli nativeRuntime processRuntime provider row summary = do
  whenVerbose cli.verbose $
    putStrLn
      ( "["
          <> providerTag provider
          <> "] "
          <> row.caseId
          <> " "
          <> row.glyphHex
          <> " gate="
          <> gateTag row.gate
      )
  nativeResult <- generateGlyphIO nativeRuntime row.cfg row.src row.glyph
  case nativeResult of
    Left err ->
      pure $
        addFailureResult
          summary
          CaseResult
            { provider = provider,
              caseId = row.caseId,
              glyph = row.glyphHex,
              gate = row.gate,
              status = CaseFail,
              sourceClass = row.sourceClass,
              exactMatch = Nothing,
              maxAbs = Nothing,
              shapeDiffRatio = Nothing,
              metricsMaxDelta = Nothing,
              alphaMedianDelta = Nothing,
              reason = Just ("Native generation failed: " <> renderGenErr err)
            }
    Right nativeOut -> do
      oracleFetch <- fetchOracle cli processRuntime provider row
      case oracleFetch of
        OracleSkip skipReason ->
          pure $
            addCaseResult
              summary
              CaseResult
                { provider = provider,
                  caseId = row.caseId,
                  glyph = row.glyphHex,
                  gate = row.gate,
                  status = CaseSkip,
                  sourceClass = row.sourceClass,
                  exactMatch = Nothing,
                  maxAbs = Nothing,
                  shapeDiffRatio = Nothing,
                  metricsMaxDelta = Nothing,
                  alphaMedianDelta = Nothing,
                  reason = Just skipReason
                }
        OracleFailure err ->
          pure $
            addFailureResult
              summary
              CaseResult
                { provider = provider,
                  caseId = row.caseId,
                  glyph = row.glyphHex,
                  gate = row.gate,
                  status = CaseFail,
                  sourceClass = row.sourceClass,
                  exactMatch = Nothing,
                  maxAbs = Nothing,
                  shapeDiffRatio = Nothing,
                  metricsMaxDelta = Nothing,
                  alphaMedianDelta = Nothing,
                  reason = Just err
                }
        OracleReady oracleImg oracleMetrics ->
          pure $
            addCaseResult
              summary
              (evaluateCase provider row nativeOut oracleImg oracleMetrics)

fetchOracle :: CliCfg -> RuntimeCfg -> OracleProvider -> CaseRow -> IO OracleFetch
fetchOracle cli processRuntime provider row =
  case provider of
    ProviderProcess -> do
      processResult <- generateGlyphIO processRuntime row.cfg row.src row.glyph
      pure $
        case processResult of
          Left err ->
            OracleFailure ("Process generation failed: " <> renderGenErr err)
          Right processOut ->
            OracleReady processOut.img (Just processOut.metrics)
    ProviderMsdfgl ->
      case row.referencePng of
        Nothing ->
          pure $
            if cli.allowMissingOracle
              then OracleSkip "Missing reference PNG path for msdfgl oracle case."
              else OracleFailure "Missing reference PNG path for msdfgl oracle case."
        Just referencePath -> do
          exists <- doesFileExist referencePath
          if not exists
            then
              pure $
                if cli.allowMissingOracle
                  then OracleSkip ("Missing msdfgl oracle image: " <> referencePath)
                  else OracleFailure ("Missing msdfgl oracle image: " <> referencePath)
            else do
              decoded <- readPngRGBA8File referencePath
              pure $
                case decoded of
                  Left err ->
                    OracleFailure ("Failed to decode msdfgl oracle PNG: " <> err)
                  Right img ->
                    OracleReady img Nothing

evaluateCase :: OracleProvider -> CaseRow -> GenOut -> ImgRGBA8 -> Maybe Metrics -> CaseResult
evaluateCase provider row nativeOut oracleImg oracleMetrics =
  case diffRGBA8 nativeOut.img oracleImg of
    Left err ->
      CaseResult
        { provider = provider,
          caseId = row.caseId,
          glyph = row.glyphHex,
          gate = row.gate,
          status = CaseFail,
          sourceClass = row.sourceClass,
          exactMatch = Nothing,
          maxAbs = Nothing,
          shapeDiffRatio = Nothing,
          metricsMaxDelta = Nothing,
          alphaMedianDelta = Nothing,
          reason = Just ("Diff failed: " <> err)
        }
    Right stats ->
      let exactMatch = nativeOut.img == oracleImg
          metricsDelta = maybe 0.0 (metricsDeltaMax nativeOut.metrics) oracleMetrics
       in case row.gate of
            GateStrict ->
              let strictOk = passesGate strictGate stats && metricsDelta <= strictMetricsDeltaLimit
                  strictReason =
                    if strictOk
                      then Nothing
                      else Just (renderStrictFailure stats metricsDelta)
               in CaseResult
                    { provider = provider,
                      caseId = row.caseId,
                      glyph = row.glyphHex,
                      gate = row.gate,
                      status = if strictOk then CasePass else CaseFail,
                      sourceClass = row.sourceClass,
                      exactMatch = Just exactMatch,
                      maxAbs = Just stats.maxAbs,
                      shapeDiffRatio = Nothing,
                      metricsMaxDelta = Just metricsDelta,
                      alphaMedianDelta = Nothing,
                      reason = strictReason
                    }
            GateCoverage ->
              case coverageStats nativeOut.img oracleImg of
                Left err ->
                  CaseResult
                    { provider = provider,
                      caseId = row.caseId,
                      glyph = row.glyphHex,
                      gate = row.gate,
                      status = CaseFail,
                      sourceClass = row.sourceClass,
                      exactMatch = Just exactMatch,
                      maxAbs = Just stats.maxAbs,
                      shapeDiffRatio = Nothing,
                      metricsMaxDelta = Just metricsDelta,
                      alphaMedianDelta = Nothing,
                      reason = Just ("Coverage stats failed: " <> err)
                    }
                Right cov ->
                  let coverageOk =
                        cov.shapeDiffRatio <= coverageShapeDiffLimit
                          && metricsDelta <= coverageMetricsDeltaLimit
                          && cov.alphaMedianDelta <= coverageAlphaMedianLimit
                      coverageReason =
                        if coverageOk
                          then Nothing
                          else Just (renderCoverageFailure stats cov metricsDelta)
                   in CaseResult
                        { provider = provider,
                          caseId = row.caseId,
                          glyph = row.glyphHex,
                          gate = row.gate,
                          status = if coverageOk then CasePass else CaseFail,
                          sourceClass = row.sourceClass,
                          exactMatch = Just exactMatch,
                          maxAbs = Just stats.maxAbs,
                          shapeDiffRatio = Just cov.shapeDiffRatio,
                          metricsMaxDelta = Just metricsDelta,
                          alphaMedianDelta = Just cov.alphaMedianDelta,
                          reason = coverageReason
                        }

addCaseResult :: Summary -> CaseResult -> Summary
addCaseResult summary result =
  let checked' =
        case result.status of
          CasePass -> summary.checked + 1
          CaseFail -> summary.checked + 1
          CaseSkip -> summary.checked
      skipped' =
        case result.status of
          CaseSkip -> summary.skipped + 1
          _ -> summary.skipped
      strictFailed' =
        case (result.status, result.gate) of
          (CaseFail, GateStrict) -> summary.strictFailed + 1
          _ -> summary.strictFailed
      coverageFailed' =
        case (result.status, result.gate) of
          (CaseFail, GateCoverage) -> summary.coverageFailed + 1
          _ -> summary.coverageFailed
      exactMismatch' =
        case result.exactMatch of
          Just False -> summary.exactMismatch + 1
          _ -> summary.exactMismatch
      exactMismatchStrict' =
        case (result.gate, result.exactMatch) of
          (GateStrict, Just False) -> summary.exactMismatchStrict + 1
          _ -> summary.exactMismatchStrict
      worstMaxAbs' = max summary.worstMaxAbs (fromMaybe 0 result.maxAbs)
      failures' =
        case (result.status, result.reason) of
          (CaseFail, Just reason) ->
            summary.failures
              <> [ Failure
                     { provider = result.provider,
                       caseId = result.caseId,
                       glyph = result.glyph,
                       gate = result.gate,
                       reason = reason
                     }
                 ]
          _ -> summary.failures
   in summary
        { checked = checked',
          strictFailed = strictFailed',
          coverageFailed = coverageFailed',
          exactMismatch = exactMismatch',
          exactMismatchStrict = exactMismatchStrict',
          worstMaxAbs = worstMaxAbs',
          skipped = skipped',
          failures = failures',
          results = summary.results <> [result]
        }

addFailureResult :: Summary -> CaseResult -> Summary
addFailureResult summary result = addCaseResult summary result {status = CaseFail}

mkSkipResult :: OracleProvider -> String -> CaseResult
mkSkipResult provider reason =
  CaseResult
    { provider = provider,
      caseId = providerTag provider,
      glyph = "-",
      gate = GateCoverage,
      status = CaseSkip,
      sourceClass = SourceManifest,
      exactMatch = Nothing,
      maxAbs = Nothing,
      shapeDiffRatio = Nothing,
      metricsMaxDelta = Nothing,
      alphaMedianDelta = Nothing,
      reason = Just reason
    }

shouldSkipVariableCoverage :: OracleProvider -> Bool -> CaseRow -> Bool
shouldSkipVariableCoverage provider processVarAxisSupported row =
  provider == ProviderProcess
    && row.sourceClass == SourceVariable
    && row.gate == GateCoverage
    && not processVarAxisSupported

emptySummary :: Summary
emptySummary =
  Summary
    { checked = 0,
      strictFailed = 0,
      coverageFailed = 0,
      exactMismatch = 0,
      exactMismatchStrict = 0,
      worstMaxAbs = 0,
      skipped = 0,
      failures = [],
      results = []
    }

mergeSummary :: Summary -> Summary -> Summary
mergeSummary a b =
  Summary
    { checked = a.checked + b.checked,
      strictFailed = a.strictFailed + b.strictFailed,
      coverageFailed = a.coverageFailed + b.coverageFailed,
      exactMismatch = a.exactMismatch + b.exactMismatch,
      exactMismatchStrict = a.exactMismatchStrict + b.exactMismatchStrict,
      worstMaxAbs = max a.worstMaxAbs b.worstMaxAbs,
      skipped = a.skipped + b.skipped,
      failures = a.failures <> b.failures,
      results = a.results <> b.results
    }

renderStrictFailure :: DiffStats -> Double -> String
renderStrictFailure stats metricsDelta =
  "strict gate failed: "
    <> "maxCh="
    <> show stats.maxCh
    <> " p99="
    <> show stats.p99Abs
    <> " mean="
    <> show stats.meanAbs
    <> " metrics_max_delta="
    <> printf "%.6f" metricsDelta
    <> " mismatch="
    <> show stats.mismatch

renderCoverageFailure :: DiffStats -> CoverageStats -> Double -> String
renderCoverageFailure stats cov metricsDelta =
  "coverage gate failed: "
    <> "shape_diff_ratio="
    <> printf "%.6f" cov.shapeDiffRatio
    <> " alpha_median_delta="
    <> printf "%.6f" cov.alphaMedianDelta
    <> " metrics_max_delta="
    <> printf "%.6f" metricsDelta
    <> " max_abs="
    <> show stats.maxAbs

report :: Summary -> IO ()
report summary = do
  putStrLn ("Checked cases: " <> show summary.checked)
  putStrLn ("Strict failures: " <> show summary.strictFailed)
  putStrLn ("Coverage failures: " <> show summary.coverageFailed)
  putStrLn ("Exact mismatches: " <> show summary.exactMismatch)
  putStrLn ("Exact mismatches (strict rows): " <> show summary.exactMismatchStrict)
  putStrLn ("Worst max abs diff: " <> show summary.worstMaxAbs)
  putStrLn ("Skipped cases: " <> show summary.skipped)
  reportProvider summary ProviderProcess
  reportProvider summary ProviderMsdfgl
  if null summary.failures
    then pure ()
    else mapM_ renderFailure summary.failures
  where
    reportProvider summary0 provider = do
      let providerResults = filter (\result -> result.provider == provider) summary0.results
      let checkedCount =
            length
              ( filter
                  (\result -> result.status == CasePass || result.status == CaseFail)
                  providerResults
              )
      let failedCount = length (filter (\result -> result.status == CaseFail) providerResults)
      let skippedCount = length (filter (\result -> result.status == CaseSkip) providerResults)
      putStrLn
        ( "Provider "
            <> providerTag provider
            <> ": checked="
            <> show checkedCount
            <> " failed="
            <> show failedCount
            <> " skipped="
            <> show skippedCount
        )
    renderFailure failure =
      putStrLn
        ( "[FAIL] "
            <> providerTag failure.provider
            <> " "
            <> failure.caseId
            <> " "
            <> failure.glyph
            <> " -> "
            <> failure.reason
        )

renderGenErr :: GenErr -> String
renderGenErr err =
  case err of
    InvalidCfg msg -> "invalid cfg: " <> msg
    MissingInput msg -> "missing input: " <> msg
    Unsupported msg -> "unsupported: " <> msg
    ExecFailed msg -> "execution failed: " <> msg
    ParseFailed msg -> "parse failed: " <> msg

parseArgs :: [String] -> Either String CliCfg
parseArgs args = go defaultCfg args
  where
    go cfg rest =
      case rest of
        [] -> Right cfg
        "--max-cases" : raw : xs ->
          case readMaybe raw of
            Just n | n > 0 -> go cfg {maxCases = Just n} xs
            _ -> Left ("Expected positive integer for --max-cases, got: " <> raw)
        "--require-exact" : xs -> go cfg {requireExact = True} xs
        "--verbose" : xs -> go cfg {verbose = True} xs
        "--profile" : raw : xs -> do
          parsedProfile <- parseProfile raw
          go cfg {profile = parsedProfile} xs
        "--oracle" : raw : xs -> do
          parsedOracle <- parseOracle raw
          go cfg {oracle = parsedOracle} xs
        "--manifest" : path : xs -> go cfg {manifestPath = Just path} xs
        "--json-out" : path : xs -> go cfg {jsonOut = Just path} xs
        "--allow-missing-oracle" : xs -> go cfg {allowMissingOracle = True} xs
        "--require-oracle" : xs -> go cfg {allowMissingOracle = False} xs
        "-h" : _ -> Left usage
        "--help" : _ -> Left usage
        flag : _ -> Left ("Unknown argument: " <> flag <> "\n" <> usage)

defaultCfg :: CliCfg
defaultCfg =
  CliCfg
    { maxCases = Nothing,
      requireExact = False,
      verbose = False,
      profile = ProfilePr,
      oracle = OracleProcess,
      manifestPath = Nothing,
      jsonOut = Nothing,
      allowMissingOracle = True
    }

usage :: String
usage =
  unlines
    [ "masdiff-parity",
      "Options:",
      "  --max-cases <n>    Limit case count for faster runs",
      "  --require-exact    Exit non-zero when any exact mismatch exists",
      "  --verbose          Print per-case diff stats",
      "  --profile <name>   pr | nightly | full (default: pr)",
      "  --oracle <name>    process | msdfgl | both (default: process)",
      "  --manifest <path>  Manifest for manifest-backed oracle comparisons",
      "  --json-out <path>  Write machine-readable JSON summary",
      "  --allow-missing-oracle",
      "                     Skip missing manifest/oracle files (default)",
      "  --require-oracle   Fail when manifest/oracle files are missing"
    ]

whenVerbose :: Bool -> IO () -> IO ()
whenVerbose verbose action =
  if verbose
    then action
    else pure ()

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

parseProfile :: String -> Either String Profile
parseProfile raw =
  case raw of
    "pr" -> Right ProfilePr
    "nightly" -> Right ProfileNightly
    "full" -> Right ProfileFull
    _ -> Left ("Unknown profile: " <> raw)

parseOracle :: String -> Either String OracleMode
parseOracle raw =
  case raw of
    "process" -> Right OracleProcess
    "msdfgl" -> Right OracleMsdfgl
    "both" -> Right OracleBoth
    _ -> Left ("Unknown oracle mode: " <> raw)

probeProcessVarAxisSupport :: RuntimeCfg -> IO (Either String Bool)
probeProcessVarAxisSupport processRuntime =
  case (mkCfg 64 8.0, mkGlyphCode (ord 'V')) of
    (Left err, _) ->
      pure (Left ("probe config failed: " <> err))
    (_, Left err) ->
      pure (Left ("probe glyph setup failed: " <> err))
    (Right cfg, Right glyph) -> do
      lightResult <- generateGlyphIO processRuntime cfg probeVarLightSrc glyph
      boldResult <- generateGlyphIO processRuntime cfg probeVarBoldSrc glyph
      pure $
        case (lightResult, boldResult) of
          (Left err, _) ->
            Left ("probe light generation failed: " <> renderGenErr err)
          (_, Left err) ->
            Left ("probe bold generation failed: " <> renderGenErr err)
          (Right lightOut, Right boldOut) ->
            Right (lightOut /= boldOut)

probeVarLightSrc :: FontSrc
probeVarLightSrc =
  VarFontFile
    { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
      axes =
        Map.fromList
          [ (AxisTag (T.pack "opsz"), AxisVal 14.0),
            (AxisTag (T.pack "wght"), AxisVal 100.0)
          ]
    }

probeVarBoldSrc :: FontSrc
probeVarBoldSrc =
  VarFontFile
    { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
      axes =
        Map.fromList
          [ (AxisTag (T.pack "opsz"), AxisVal 32.0),
            (AxisTag (T.pack "wght"), AxisVal 900.0)
          ]
    }

mkCfg :: Int -> Double -> Either String GenCfg
mkCfg dimPx pxrVal = do
  dim <- mkDim dimPx
  pxr <- mkPxRange pxrVal
  pure
    GenCfg
      { mode = Mtsdf,
        dim = dim,
        pxr = pxr,
        seed = 1,
        autoframe = True,
        ovlp = False
      }

metricsDeltaMax :: Metrics -> Metrics -> Double
metricsDeltaMax a b =
  maximum
    [ abs (a.adv - b.adv),
      tuple4Delta a.bounds b.bounds,
      maybeDoubleDelta a.scale b.scale,
      maybeTuple2Delta a.translate b.translate,
      maybeTuple2Delta a.range b.range
    ]

tuple4Delta :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> Double
tuple4Delta (a0, a1, a2, a3) (b0, b1, b2, b3) =
  maximum [abs (a0 - b0), abs (a1 - b1), abs (a2 - b2), abs (a3 - b3)]

tuple2Delta :: (Double, Double) -> (Double, Double) -> Double
tuple2Delta (a0, a1) (b0, b1) = max (abs (a0 - b0)) (abs (a1 - b1))

maybeDoubleDelta :: Maybe Double -> Maybe Double -> Double
maybeDoubleDelta a b =
  case (a, b) of
    (Nothing, Nothing) -> 0.0
    (Just x, Just y) -> abs (x - y)
    _ -> 1.0

maybeTuple2Delta :: Maybe (Double, Double) -> Maybe (Double, Double) -> Double
maybeTuple2Delta a b =
  case (a, b) of
    (Nothing, Nothing) -> 0.0
    (Just x, Just y) -> tuple2Delta x y
    _ -> 1.0

coverageStats :: ImgRGBA8 -> ImgRGBA8 -> Either String CoverageStats
coverageStats left right
  | left.w /= right.w || left.h /= right.h =
      Left
        ( "Image dimensions differ. left="
            <> show (left.w, left.h)
            <> " right="
            <> show (right.w, right.h)
        )
  | BS.length left.px /= BS.length right.px =
      Left
        ( "Image payload lengths differ. left="
            <> show (BS.length left.px)
            <> " right="
            <> show (BS.length right.px)
        )
  | otherwise =
      let pxCount = left.w * left.h
          (shapeDiff, leftAlphaMismatch, rightAlphaMismatch) = go 0 (0 :: Int) (0 :: Int) (0 :: Int)
          shapeRatio = fromIntegral shapeDiff / fromIntegral (max 1 pxCount)
          leftRatio = fromIntegral leftAlphaMismatch / fromIntegral (max 1 pxCount)
          rightRatio = fromIntegral rightAlphaMismatch / fromIntegral (max 1 pxCount)
       in Right
            CoverageStats
              { shapeDiffRatio = shapeRatio,
                alphaMedianDelta = abs (leftRatio - rightRatio)
              }
  where
    total = BS.length left.px
    go idx shapeDiff leftMismatch rightMismatch
      | idx >= total = (shapeDiff, leftMismatch, rightMismatch)
      | otherwise =
          let lr = BS.index left.px idx
              lg = BS.index left.px (idx + 1)
              lb = BS.index left.px (idx + 2)
              la = BS.index left.px (idx + 3)
              rr = BS.index right.px idx
              rg = BS.index right.px (idx + 1)
              rb = BS.index right.px (idx + 2)
              ra = BS.index right.px (idx + 3)
              leftInside = medianWord8 lr lg lb >= 128
              rightInside = medianWord8 rr rg rb >= 128
              shapeDiff' =
                if leftInside /= rightInside
                  then shapeDiff + 1
                  else shapeDiff
              leftMismatch' =
                if (la >= 128) /= leftInside
                  then leftMismatch + 1
                  else leftMismatch
              rightMismatch' =
                if (ra >= 128) /= rightInside
                  then rightMismatch + 1
                  else rightMismatch
           in go (idx + 4) shapeDiff' leftMismatch' rightMismatch'

medianWord8 :: Word8 -> Word8 -> Word8 -> Word8
medianWord8 x y z = max (min x y) (min (max x y) z)

nightlyFontCases :: [FontCase]
nightlyFontCases = nub (interHarnessFontCases <> extraCoverageFontCases)

extraCoverageFontCases :: [FontCase]
extraCoverageFontCases =
  [ FontCase
      { fontCaseId = "inter-v41-regular",
        fontCaseKind = Static,
        fontCaseSource = StaticFont "assets/inter-v4.1-source/extras/ttf/Inter-Regular.ttf"
      },
    FontCase
      { fontCaseId = "inter-v41-bold",
        fontCaseKind = Static,
        fontCaseSource = StaticFont "assets/inter-v4.1-source/extras/ttf/Inter-Bold.ttf"
      },
    FontCase
      { fontCaseId = "inter-v41-italic",
        fontCaseKind = Static,
        fontCaseSource = StaticFont "assets/inter-v4.1-source/extras/ttf/Inter-Italic.ttf"
      },
    FontCase
      { fontCaseId = "inter-v41-black",
        fontCaseKind = Static,
        fontCaseSource = StaticFont "assets/inter-v4.1-source/extras/ttf/Inter-Black.ttf"
      },
    FontCase
      { fontCaseId = "roboto-flex-w100-o14",
        fontCaseKind = Variable,
        fontCaseSource = VariableFont "assets/roboto-flex-source/RobotoFlex-VF.ttf" [("wght", "100"), ("opsz", "14")]
      },
    FontCase
      { fontCaseId = "roboto-flex-w400-o14",
        fontCaseKind = Variable,
        fontCaseSource = VariableFont "assets/roboto-flex-source/RobotoFlex-VF.ttf" [("wght", "400"), ("opsz", "14")]
      },
    FontCase
      { fontCaseId = "roboto-flex-w700-o32",
        fontCaseKind = Variable,
        fontCaseSource = VariableFont "assets/roboto-flex-source/RobotoFlex-VF.ttf" [("wght", "700"), ("opsz", "32")]
      },
    FontCase
      { fontCaseId = "roboto-flex-w900-o32",
        fontCaseKind = Variable,
        fontCaseSource = VariableFont "assets/roboto-flex-source/RobotoFlex-VF.ttf" [("wght", "900"), ("opsz", "32")]
      }
  ]

nightlyGlyphs :: [Char]
nightlyGlyphs = nub (interHarnessGlyphs <> ['R', 'S', '#', '!'])

nightlyCfgSpecs :: [(Int, Double)]
nightlyCfgSpecs =
  [ (32, 4.0),
    (32, 8.0),
    (32, 12.0),
    (64, 4.0),
    (64, 8.0),
    (64, 12.0),
    (128, 4.0),
    (128, 8.0),
    (128, 12.0)
  ]

pxRangeTag :: Double -> String
pxRangeTag pxrVal = map normalize (printf "%.2f" pxrVal)
  where
    normalize '.' = '_'
    normalize c = c

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) <= 1.0e-6

strictMetricsDeltaLimit :: Double
strictMetricsDeltaLimit = 1.0e-6

coverageShapeDiffLimit :: Double
coverageShapeDiffLimit = 0.002

coverageMetricsDeltaLimit :: Double
coverageMetricsDeltaLimit = 2.0e-2

coverageAlphaMedianLimit :: Double
coverageAlphaMedianLimit = 0.005

providerTag :: OracleProvider -> String
providerTag provider =
  case provider of
    ProviderProcess -> "process"
    ProviderMsdfgl -> "msdfgl"

gateTag :: Gate -> String
gateTag gate =
  case gate of
    GateStrict -> "strict"
    GateCoverage -> "coverage"

statusTag :: CaseStatus -> String
statusTag status =
  case status of
    CasePass -> "pass"
    CaseFail -> "fail"
    CaseSkip -> "skip"

sourceClassTag :: SourceClass -> String
sourceClassTag sourceClass =
  case sourceClass of
    SourceStatic -> "static"
    SourceVariable -> "variable"
    SourceManifest -> "manifest"

renderSummaryJson :: Summary -> String
renderSummaryJson summary =
  "{\n"
    <> "  \"summary\": {\n"
    <> "    \"checked\": "
    <> show summary.checked
    <> ",\n"
    <> "    \"strict_failed\": "
    <> show summary.strictFailed
    <> ",\n"
    <> "    \"coverage_failed\": "
    <> show summary.coverageFailed
    <> ",\n"
    <> "    \"exact_mismatch\": "
    <> show summary.exactMismatch
    <> ",\n"
    <> "    \"exact_mismatch_strict\": "
    <> show summary.exactMismatchStrict
    <> ",\n"
    <> "    \"worst_max_abs\": "
    <> show summary.worstMaxAbs
    <> ",\n"
    <> "    \"skipped\": "
    <> show summary.skipped
    <> "\n"
    <> "  },\n"
    <> "  \"results\": [\n"
    <> intercalate ",\n" (fmap (("    " <>) . renderCaseResultJson) summary.results)
    <> "\n"
    <> "  ]\n"
    <> "}\n"

renderCaseResultJson :: CaseResult -> String
renderCaseResultJson result =
  "{"
    <> intercalate
      ","
      [ "\"provider\":" <> jsonString (providerTag result.provider),
        "\"case_id\":" <> jsonString result.caseId,
        "\"glyph\":" <> jsonString result.glyph,
        "\"gate\":" <> jsonString (gateTag result.gate),
        "\"status\":" <> jsonString (statusTag result.status),
        "\"source_class\":" <> jsonString (sourceClassTag result.sourceClass),
        "\"exact_match\":" <> jsonMaybeBool result.exactMatch,
        "\"max_abs\":" <> jsonMaybeInt result.maxAbs,
        "\"shape_diff_ratio\":" <> jsonMaybeDouble result.shapeDiffRatio,
        "\"metrics_max_delta\":" <> jsonMaybeDouble result.metricsMaxDelta,
        "\"alpha_median_delta\":" <> jsonMaybeDouble result.alphaMedianDelta,
        "\"reason\":" <> jsonMaybeString result.reason
      ]
    <> "}"

jsonString :: String -> String
jsonString raw = "\"" <> concatMap escapeChar raw <> "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

jsonMaybeString :: Maybe String -> String
jsonMaybeString value =
  case value of
    Nothing -> "null"
    Just x -> jsonString x

jsonMaybeInt :: Maybe Int -> String
jsonMaybeInt value =
  case value of
    Nothing -> "null"
    Just x -> show x

jsonMaybeDouble :: Maybe Double -> String
jsonMaybeDouble value =
  case value of
    Nothing -> "null"
    Just x -> show x

jsonMaybeBool :: Maybe Bool -> String
jsonMaybeBool value =
  case value of
    Nothing -> "null"
    Just x -> if x then "true" else "false"

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 2)
