{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Font (FontCase (..), FontSource (..), interHarnessGlyphs, interOracleFontCases)
import MSDF.Compare (DiffStats (..), diffRGBA8, passesGate, strictGate)
import MSDF.Generate (BackendMode (..), RuntimeCfg (..), defaultRuntimeCfg, generateGlyphIO)
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    GlyphCode,
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkPxRange,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

data CliCfg = CliCfg
  { maxCases :: Maybe Int,
    requireExact :: Bool,
    verbose :: Bool
  }
  deriving stock (Eq, Show)

data Row = Row
  { caseId :: String,
    glyphChar :: Char,
    glyphHex :: String,
    src :: FontSrc,
    glyph :: GlyphCode
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
    strictFailed :: [Failure],
    exactMismatch :: Int,
    worstMaxAbs :: Int
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  cli <- either failWith pure . parseArgs =<< getArgs
  cfg <- either failWith pure mkHarnessCfg
  rows <- either failWith pure mkRows
  let selected = maybe rows (`take` rows) cli.maxCases
  let nativeRuntime = defaultRuntimeCfg {backend = BackendNative}
  let processRuntime = RuntimeCfg {backend = BackendProcess, msdfgenBin = defaultRuntimeCfg.msdfgenBin}
  summary <- foldl' (\ioSummary row -> ioSummary >>= checkOne cli cfg nativeRuntime processRuntime row) (pure emptySummary) selected
  report summary
  if not (null summary.strictFailed)
    then exitWith (ExitFailure 1)
    else
      if cli.requireExact && summary.exactMismatch > 0
        then exitWith (ExitFailure 1)
        else exitWith ExitSuccess

mkHarnessCfg :: Either String GenCfg
mkHarnessCfg = do
  dim <- mkDim 64
  pxr <- mkPxRange 8.0
  pure
    GenCfg
      { mode = Mtsdf,
        dim = dim,
        pxr = pxr,
        seed = 1,
        autoframe = True
      }

mkRows :: Either String [Row]
mkRows = do
  rows <- traverse rowsForFontCase interOracleFontCases
  pure (concat rows)
  where
    rowsForFontCase fontCase = do
      src <- fontCaseToFontSrc fontCase
      traverse (mkRow fontCase src) interHarnessGlyphs
    mkRow fontCase src glyphChar = do
      glyph <- mkGlyphCode (ord glyphChar)
      pure
        Row
          { caseId = fontCase.fontCaseId,
            glyphChar = glyphChar,
            glyphHex = printf "U+%04X" (ord glyphChar),
            src = src,
            glyph = glyph
          }

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

checkOne :: CliCfg -> GenCfg -> RuntimeCfg -> RuntimeCfg -> Row -> Summary -> IO Summary
checkOne cli cfg nativeRuntime processRuntime row summary = do
  nativeResult <- generateGlyphIO nativeRuntime cfg row.src row.glyph
  processResult <- generateGlyphIO processRuntime cfg row.src row.glyph
  case (nativeResult, processResult) of
    (Left err, _) ->
      pure $
        addFailure
          summary
          row
          ("Native generation failed: " <> renderGenErr err)
    (_, Left err) ->
      pure $
        addFailure
          summary
          row
          ("Process generation failed: " <> renderGenErr err)
    (Right nativeOut, Right processOut) ->
      case diffRGBA8 nativeOut.img processOut.img of
        Left err ->
          pure $
            addFailure
              summary
              row
              ("Diff failed: " <> err)
        Right stats -> do
          whenVerbose cli.verbose $
            putStrLn
              ( row.caseId
                  <> " "
                  <> row.glyphHex
                  <> " maxAbs="
                  <> show stats.maxAbs
                  <> " p99="
                  <> show stats.p99Abs
                  <> " mean="
                  <> show stats.meanAbs
              )
          let exactMismatch = if nativeOut.img == processOut.img then 0 else 1
          let next =
                summary
                  { checked = summary.checked + 1,
                    exactMismatch = summary.exactMismatch + exactMismatch,
                    worstMaxAbs = max summary.worstMaxAbs stats.maxAbs
                  }
          if passesGate strictGate stats
            then pure next
            else
              pure $
                next
                  { strictFailed =
                      next.strictFailed
                        <> [ Failure
                               { caseId = row.caseId,
                                 glyph = row.glyphHex,
                                 reason = renderGateFailure stats
                               }
                           ]
                  }

addFailure :: Summary -> Row -> String -> Summary
addFailure summary row reason =
  summary
    { checked = summary.checked + 1,
      strictFailed =
        summary.strictFailed
          <> [ Failure
                 { caseId = row.caseId,
                   glyph = row.glyphHex,
                   reason = reason
                 }
             ]
    }

emptySummary :: Summary
emptySummary =
  Summary
    { checked = 0,
      strictFailed = [],
      exactMismatch = 0,
      worstMaxAbs = 0
    }

renderGateFailure :: DiffStats -> String
renderGateFailure stats =
  "strict gate failed: "
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
  putStrLn ("Checked cases: " <> show summary.checked)
  putStrLn ("Strict failures: " <> show (length summary.strictFailed))
  putStrLn ("Exact mismatches: " <> show summary.exactMismatch)
  putStrLn ("Worst max abs diff: " <> show summary.worstMaxAbs)
  if null summary.strictFailed
    then pure ()
    else mapM_ renderFailure summary.strictFailed
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
        "-h" : _ -> Left usage
        "--help" : _ -> Left usage
        flag : _ -> Left ("Unknown argument: " <> flag <> "\n" <> usage)

defaultCfg :: CliCfg
defaultCfg =
  CliCfg
    { maxCases = Nothing,
      requireExact = False,
      verbose = False
    }

usage :: String
usage =
  unlines
    [ "masdiff-parity",
      "Options:",
      "  --max-cases <n>    Limit case count for faster runs",
      "  --require-exact    Exit non-zero when any exact mismatch exists",
      "  --verbose          Print per-case diff stats"
    ]

whenVerbose :: Bool -> IO () -> IO ()
whenVerbose verbose action =
  if verbose
    then action
    else pure ()

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 2)
