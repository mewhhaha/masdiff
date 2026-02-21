{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (ord, toLower)
import Data.List (find, intercalate, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Font
  ( FontCase (..),
    FontSource (..),
    fontFilePath,
    fontInputLabel,
    interHarnessGlyphs,
    interOracleFontCases,
  )
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate
  ( BackendMode (..),
    RuntimeCfg (..),
    defaultRuntimeCfg,
    generateGlyphIO,
    parseBackendModeEnv,
  )
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
    unGlyphCode,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    doesPathExist,
    findExecutable,
    removePathForcibly,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

data HarnessOptions = HarnessOptions
  { harnessBackend :: BackendMode,
    harnessMsdfgenBin :: FilePath,
    harnessOutDir :: FilePath,
    harnessDimension :: Int,
    harnessPxRange :: Double,
    harnessSeed :: Int,
    harnessClean :: Bool
  }
  deriving stock (Eq, Show)

data MissingFont = MissingFont
  { caseId :: String,
    path :: FilePath
  }
  deriving stock (Eq, Show)

data ManifestRow = ManifestRow
  { fontCase :: String,
    glyph :: Char,
    glyphHex :: String,
    outputPng :: FilePath,
    inputSpec :: String
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  optionsResult <- readHarnessOptions
  options <- either failWith pure optionsResult
  let runtime =
        RuntimeCfg
          { backend = options.harnessBackend,
            msdfgenBin = options.harnessMsdfgenBin
          }
  preflightMsdfgen runtime
  preflightFonts
  cfg <- either failWith pure (mkGenCfg options)
  when options.harnessClean $ do
    outExists <- doesPathExist options.harnessOutDir
    when outExists (removePathForcibly options.harnessOutDir)
  createDirectoryIfMissing True options.harnessOutDir
  rowsResult <- generateAll runtime cfg options.harnessOutDir
  rows <- either failWith pure rowsResult
  writeManifest options rows
  putStrLn "Fixture generation complete."
  putStrLn ("Manifest written to " <> manifestPath options)

readHarnessOptions :: IO (Either String HarnessOptions)
readHarnessOptions = do
  backendRaw <- lookupEnv "MASDIFF_BACKEND"
  msdfgenBin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  outDir <- fromMaybe "out/reference/inter-mtsdf" <$> lookupEnv "MTSDF_OUT"
  dimResult <- parseOptionalEnv "MTSDF_DIM" 64 parseDim
  pxRangeResult <- parseOptionalEnv "MTSDF_PXRANGE" 8.0 parsePxRange
  cleanResult <- parseOptionalEnv "MTSDF_CLEAN" False parseCleanFlag
  pure $ do
    backend <- parseBackendModeEnv backendRaw
    dimension <- dimResult
    pxRange <- pxRangeResult
    clean <- cleanResult
    pure
      HarnessOptions
        { harnessBackend = backend,
          harnessMsdfgenBin = msdfgenBin,
          harnessOutDir = outDir,
          harnessDimension = dimension,
          harnessPxRange = pxRange,
          harnessSeed = 1,
          harnessClean = clean
        }

parseOptionalEnv :: String -> a -> (String -> Either String a) -> IO (Either String a)
parseOptionalEnv name fallback parser = do
  raw <- lookupEnv name
  pure $
    case raw of
      Nothing -> Right fallback
      Just value -> parser value

parseDim :: String -> Either String Int
parseDim raw =
  case readMaybe raw of
    Just x | x > 0 -> Right x
    _ -> Left ("MTSDF_DIM must be a positive integer, but got: " <> raw)

parsePxRange :: String -> Either String Double
parsePxRange raw =
  case readMaybe raw of
    Just x | x > 0 && isFinite x -> Right x
    _ -> Left ("MTSDF_PXRANGE must be a positive number, but got: " <> raw)

parseCleanFlag :: String -> Either String Bool
parseCleanFlag raw =
  case fmap toLower raw of
    "1" -> Right True
    "true" -> Right True
    "yes" -> Right True
    "0" -> Right False
    "false" -> Right False
    "no" -> Right False
    _ -> Left ("MTSDF_CLEAN must be one of: 1,0,true,false,yes,no; got: " <> raw)

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

preflightMsdfgen :: RuntimeCfg -> IO ()
preflightMsdfgen runtime =
  case runtime.backend of
    BackendNative -> pure ()
    BackendProcess -> do
      resolved <- findExecutable runtime.msdfgenBin
      case resolved of
        Nothing ->
          failWith
            ( "Could not find msdfgen executable: "
                <> runtime.msdfgenBin
                <> ". Install msdfgen or set MSDFGEN_BIN."
            )
        Just _ -> pure ()

preflightFonts :: IO ()
preflightFonts = do
  missing <- catMaybes <$> traverse findMissing interOracleFontCases
  if null missing
    then pure ()
    else do
      hPutStrLn stderr "Font files are missing for the following cases:"
      traverse_ renderMissing missing
      exitWith (ExitFailure 1)
  where
    findMissing fontCase = do
      exists <- doesFileExist (fontFilePath fontCase)
      pure $
        if exists
          then Nothing
          else
            Just
              MissingFont
                { caseId = fontCase.fontCaseId,
                  path = fontFilePath fontCase
                }
    renderMissing missingFont =
      hPutStrLn stderr ("  " <> missingFont.caseId <> " -> " <> missingFont.path)

mkGenCfg :: HarnessOptions -> Either String GenCfg
mkGenCfg options = do
  dim <- mkDim options.harnessDimension
  pxr <- mkPxRange options.harnessPxRange
  pure
    GenCfg
      { mode = Mtsdf,
        dim = dim,
        pxr = pxr,
        seed = options.harnessSeed,
        autoframe = True,
        ovlp = False
      }

generateAll :: RuntimeCfg -> GenCfg -> FilePath -> IO (Either String [ManifestRow])
generateAll runtime cfg outDir = do
  let total = length interOracleFontCases * length interHarnessGlyphs
  putStrLn ("Generating " <> show total <> " MTSDF fixtures into " <> outDir)
  goCases [] interOracleFontCases
  where
    goCases rows [] = pure (Right (reverse rows))
    goCases rows (fontCase : rest) = do
      putStrLn ("  " <> fontCase.fontCaseId <> " (" <> show (length interHarnessGlyphs) <> " glyphs)")
      let caseDir = outDir </> fontCase.fontCaseId
      createDirectoryIfMissing True caseDir
      case fontCaseToFontSrc fontCase of
        Left err -> pure (Left err)
        Right src -> do
          rowResult <- goGlyphs rows fontCase src caseDir interHarnessGlyphs
          case rowResult of
            Left err -> pure (Left err)
            Right rows' -> goCases rows' rest

    goGlyphs rows _ _ _ [] = pure (Right rows)
    goGlyphs rows fontCase src caseDir (glyphChar : rest) =
      case mkGlyphCode (ord glyphChar) of
        Left err ->
          pure $
            Left $
              "Invalid glyph code for "
                <> [glyphChar]
                <> ": "
                <> err
        Right glyphCode -> do
          let glyphHex = renderGlyphHex glyphCode
          let outputPath = caseDir </> glyphHex <> ".png"
          generated <- generateGlyphIO runtime cfg src glyphCode
          case generated of
            Left err ->
              pure (Left (renderGenFailure fontCase.fontCaseId glyphHex err))
            Right out -> do
              let outImg =
                    case out of
                      GenOut {img = img} -> img
              writePngRGBA8File outputPath outImg
              let row =
                    ManifestRow
                      { fontCase = fontCase.fontCaseId,
                        glyph = glyphChar,
                        glyphHex = glyphHex,
                        outputPng = outputPath,
                        inputSpec = fontInputLabel fontCase
                      }
              goGlyphs (row : rows) fontCase src caseDir rest

fontCaseToFontSrc :: FontCase -> Either String FontSrc
fontCaseToFontSrc fontCase =
  case fontCase.fontCaseSource of
    StaticFont path ->
      Right FontFile {path = path}
    VariableFont path axes -> do
      axisPairs <- traverse (parseAxis fontCase.fontCaseId) axes
      pure
        VarFontFile
          { path = path,
            axes = Map.fromList axisPairs
          }
  where
    parseAxis caseId (name, rawValue) =
      case readMaybe rawValue of
        Just value | isFinite value ->
          Right (AxisTag (T.pack name), AxisVal value)
        _ ->
          Left
            ( "Invalid variable axis value for case "
                <> caseId
                <> ": "
                <> name
                <> "="
                <> rawValue
            )

renderGlyphHex :: GlyphCode -> String
renderGlyphHex glyph = printf "U+%04X" (unGlyphCode glyph)

renderGenFailure :: String -> String -> GenErr -> String
renderGenFailure fontCaseId glyphHex err =
  case err of
    ExecFailed details ->
      let commandLine = extractField "command=" details
          exitCode = extractField "exit=" details
       in unlines $
            ["msdfgen failed for case " <> fontCaseId <> " glyph " <> glyphHex]
              <> maybe [] (\cmd -> ["command: " <> cmd]) commandLine
              <> maybe [] (\code -> ["exit code: " <> code]) exitCode
    InvalidCfg msg -> "Invalid generator configuration: " <> msg
    MissingInput msg -> "Missing input: " <> msg
    Unsupported msg -> "Unsupported request: " <> msg
    ParseFailed msg -> "Failed to parse generated output: " <> msg

extractField :: String -> String -> Maybe String
extractField prefix raw =
  fmap (drop (length prefix)) $
    find (isPrefixOf prefix) (lines raw)

writeManifest :: HarnessOptions -> [ManifestRow] -> IO ()
writeManifest options rows = do
  let content =
        unlines $
          [ "# generator=generate-inter-mtsdf-fixtures",
            "# dimensions=" <> show options.harnessDimension,
            "# pxrange=" <> show options.harnessPxRange,
            "# seed=" <> show options.harnessSeed,
            "font_case\tglyph\tglyph_hex\toutput_png\tinput_spec"
          ]
            <> fmap renderRow rows
  writeFile (manifestPath options) content
  where
    renderRow row =
      intercalate
        "\t"
        [ row.fontCase,
          [row.glyph],
          row.glyphHex,
          row.outputPng,
          row.inputSpec
        ]

manifestPath :: HarnessOptions -> FilePath
manifestPath options = options.harnessOutDir </> "manifest.tsv"

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

traverse_ :: Applicative f => (a -> f b) -> [a] -> f ()
traverse_ _ [] = pure ()
traverse_ f (x : xs) = f x *> traverse_ f xs
