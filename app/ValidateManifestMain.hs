{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import MSDF.Compare (DiffStats (..), diffRGBA8, passesGate, strictGate)
import MSDF.Encode (readPngRGBA8File)
import MSDF.Generate
  ( RuntimeCfg (..),
    defaultRuntimeCfg,
    generateGlyphIO,
    parseBackendModeEnv,
  )
import MSDF.Manifest (Manifest (..), ManifestRow (..), loadManifest, manifestCfg)
import MSDF.Types (GenCfg, GenErr (..), GenOut (..))
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

data CliCfg = CliCfg
  { manifest :: FilePath,
    maxCases :: Maybe Int,
    verbose :: Bool
  }
  deriving stock (Eq, Show)

data Failure = Failure
  { caseId :: String,
    glyph :: String,
    reason :: String
  }
  deriving stock (Eq, Show)

data Summary = Summary
  { checked :: Int,
    failed :: [Failure],
    worstMaxAbs :: Int
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  cli <- case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitWith (ExitFailure 2)
    Right cfg -> pure cfg
  runtimeResult <- readRuntime
  runtime <- case runtimeResult of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 2)
    Right cfg -> pure cfg
  manifestResult <- loadManifest cli.manifest
  manifest <- case manifestResult of
    Left err -> do
      hPutStrLn stderr ("Failed to parse manifest: " <> err)
      exitWith (ExitFailure 2)
    Right parsed -> pure parsed
  let cfg = manifestCfg manifest.meta
  let rows = maybe manifest.rows (`take` manifest.rows) cli.maxCases
  summary <- foldl' (\ioSummary row -> ioSummary >>= checkOne runtime cfg cli.verbose row) (pure emptySummary) rows
  report summary
  if null summary.failed
    then exitWith ExitSuccess
    else exitWith (ExitFailure 1)

readRuntime :: IO (Either String RuntimeCfg)
readRuntime = do
  backendRaw <- lookupEnv "MASDIFF_BACKEND"
  bin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  pure $ do
    backend <- parseBackendModeEnv backendRaw
    pure RuntimeCfg {backend = backend, msdfgenBin = bin}

checkOne :: RuntimeCfg -> GenCfg -> Bool -> ManifestRow -> Summary -> IO Summary
checkOne runtime cfg verbose row summary = do
  whenVerbose verbose $
    putStrLn ("Validating " <> row.fontCase <> " " <> row.glyphHex)
  generated <- generateGlyphIO runtime cfg row.src row.glyph
  case generated of
    Left err ->
      pure $
        summary
          { checked = summary.checked + 1,
            failed =
              summary.failed
                <> [ Failure
                       { caseId = row.fontCase,
                         glyph = row.glyphHex,
                         reason = renderGenErr err
                       }
                   ]
          }
    Right out -> do
      refResult <- readPngRGBA8File row.outputPng
      case refResult of
        Left err ->
          pure $
            summary
              { checked = summary.checked + 1,
                failed =
                  summary.failed
                    <> [ Failure
                           { caseId = row.fontCase,
                             glyph = row.glyphHex,
                             reason = "Reference decode failed: " <> err
                           }
                       ]
              }
        Right ref -> do
          case diffRGBA8 ref out.img of
            Left err ->
              pure $
                summary
                  { checked = summary.checked + 1,
                    failed =
                      summary.failed
                        <> [ Failure
                               { caseId = row.fontCase,
                                 glyph = row.glyphHex,
                                 reason = "Diff failed: " <> err
                               }
                           ]
                  }
            Right stats ->
              if passesGate strictGate stats
                then
                  pure $
                    summary
                      { checked = summary.checked + 1,
                        worstMaxAbs = max summary.worstMaxAbs stats.maxAbs
                      }
                else
                  pure $
                    summary
                      { checked = summary.checked + 1,
                        worstMaxAbs = max summary.worstMaxAbs stats.maxAbs,
                        failed =
                          summary.failed
                            <> [ Failure
                                   { caseId = row.fontCase,
                                     glyph = row.glyphHex,
                                     reason = renderGateFailure stats
                                   }
                               ]
                      }

emptySummary :: Summary
emptySummary =
  Summary
    { checked = 0,
      failed = [],
      worstMaxAbs = 0
    }

renderGateFailure :: DiffStats -> String
renderGateFailure stats =
  "Strict gate failed: "
    <> "maxCh="
    <> show stats.maxCh
    <> " p99="
    <> show stats.p99Abs
    <> " mean="
    <> show stats.meanAbs
    <> " mismatch="
    <> show stats.mismatch

report :: Summary -> IO ()
report summary = do
  putStrLn ("Validated cases: " <> show summary.checked)
  putStrLn ("Worst max abs diff: " <> show summary.worstMaxAbs)
  if null summary.failed
    then putStrLn "All manifest cases passed strict gate."
    else do
      putStrLn ("Failed cases: " <> show (length summary.failed))
      mapM_ renderFailure summary.failed
  where
    renderFailure failure =
      putStrLn
        ( "[FAIL] "
            <> failure.caseId
            <> " "
            <> failure.glyph
            <> " -> "
            <> failure.reason
        )

renderGenErr :: GenErr -> String
renderGenErr err =
  case err of
    InvalidCfg msg -> "Invalid configuration: " <> msg
    MissingInput msg -> "Missing input: " <> msg
    Unsupported msg -> "Unsupported: " <> msg
    ExecFailed msg -> "Execution failed: " <> msg
    ParseFailed msg -> "Parse failure: " <> msg

parseArgs :: [String] -> Either String CliCfg
parseArgs args = parseLoop defaultCfg args

parseLoop :: CliCfg -> [String] -> Either String CliCfg
parseLoop cfg args =
  case args of
    [] -> Right cfg
    "-m" : path : rest -> parseLoop cfg {manifest = path} rest
    "--manifest" : path : rest -> parseLoop cfg {manifest = path} rest
    "--max-cases" : raw : rest -> do
      n <- parseInt raw
      parseLoop cfg {maxCases = Just n} rest
    "--verbose" : rest -> parseLoop cfg {verbose = True} rest
    "-h" : _ -> Left "help"
    "--help" : _ -> Left "help"
    raw : _ -> Left ("Unknown argument: " <> raw)

defaultCfg :: CliCfg
defaultCfg =
  CliCfg
    { manifest = "out/reference/inter-mtsdf/manifest.tsv",
      maxCases = Nothing,
      verbose = False
    }

usage :: String
usage =
  unlines
    [ "masdiff-validate",
      "Options:",
      "  --manifest <path>   (default: out/reference/inter-mtsdf/manifest.tsv)",
      "  --max-cases <n>     (optional, for quick runs)",
      "  --verbose"
    ]

parseInt :: String -> Either String Int
parseInt raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Expected integer for --max-cases, got: " <> raw)

whenVerbose :: Bool -> IO () -> IO ()
whenVerbose verbose action =
  if verbose
    then action
    else pure ()
