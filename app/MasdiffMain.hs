{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import MSDF.Encode (writeMsdfgenRgbaFile, writePngRGBA8File)
import MSDF.Generate
  ( RuntimeCfg (..),
    defaultRuntimeCfg,
    generateGlyphIO,
    parseBackendModeEnv,
    renderMetrics,
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
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

data OutFmt = Png | Rgba
  deriving stock (Eq, Show)

data CliReq = CliReq
  { cfg :: GenCfg,
    src :: FontSrc,
    glyph :: GlyphCode,
    out :: FilePath,
    fmt :: OutFmt,
    printMetrics :: Bool
  }
  deriving stock (Eq, Show)

data ParseState = ParseState
  { psSrc :: Maybe FontSrc,
    psGlyph :: Maybe GlyphCode,
    psDim :: Int,
    psPxr :: Double,
    psSeed :: Int,
    psAutoframe :: Bool,
    psOut :: FilePath,
    psFmt :: Maybe OutFmt,
    psPrintMetrics :: Bool
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  runtimeResult <- readRuntime
  runtime <- case runtimeResult of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 2)
    Right cfg -> pure cfg
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitWith (ExitFailure 2)
    Right req -> do
      result <- generateGlyphIO runtime req.cfg req.src req.glyph
      case result of
        Left err -> do
          hPutStrLn stderr (renderGenErr err)
          exitWith (ExitFailure 3)
        Right out -> do
          case req.fmt of
            Png -> writePngRGBA8File req.out out.img
            Rgba -> writeMsdfgenRgbaFile req.out out.img
          if req.printMetrics
            then putStr (renderMetrics out.metrics)
            else pure ()
          exitWith ExitSuccess

readRuntime :: IO (Either String RuntimeCfg)
readRuntime = do
  backendRaw <- lookupEnv "MASDIFF_BACKEND"
  bin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  pure $ do
    backend <- parseBackendModeEnv backendRaw
    pure RuntimeCfg {backend = backend, msdfgenBin = bin}

parseArgs :: [String] -> Either String CliReq
parseArgs args =
  case args of
    [] -> Left "Missing mode. Expected: mtsdf"
    "mtsdf" : rest -> finalize =<< parseLoop defaultState rest
    "help" : _ -> Left "help"
    "--help" : _ -> Left "help"
    "-h" : _ -> Left "help"
    mode : _ -> Left ("Unsupported mode: " <> mode <> ". Supported: mtsdf")

parseLoop :: ParseState -> [String] -> Either String ParseState
parseLoop st args =
  case args of
    [] -> Right st
    ["-h"] -> Left "help"
    ["--help"] -> Left "help"
    "-font" : path : glyphRaw : rest -> do
      glyph <- parseGlyphCode glyphRaw
      ensureNoSource st
      parseLoop st {psSrc = Just FontFile {path = path}, psGlyph = Just glyph} rest
    "-varfont" : spec : glyphRaw : rest -> do
      src <- parseVarFontSpec spec
      glyph <- parseGlyphCode glyphRaw
      ensureNoSource st
      parseLoop st {psSrc = Just src, psGlyph = Just glyph} rest
    "-dimensions" : wRaw : hRaw : rest -> do
      w <- parseInt "width" wRaw
      h <- parseInt "height" hRaw
      if w /= h
        then Left "Phase-1 only supports square dimensions (width == height)."
        else parseLoop st {psDim = w} rest
    "-pxrange" : raw : rest -> do
      pxr <- parseDouble "pxrange" raw
      parseLoop st {psPxr = pxr} rest
    "-seed" : raw : rest -> do
      seed <- parseInt "seed" raw
      parseLoop st {psSeed = seed} rest
    "-autoframe" : rest ->
      parseLoop st {psAutoframe = True} rest
    "-o" : path : rest ->
      parseLoop st {psOut = path} rest
    "-format" : fmtRaw : rest -> do
      fmt <- parseOutFmt fmtRaw
      parseLoop st {psFmt = Just fmt} rest
    "-printmetrics" : rest ->
      parseLoop st {psPrintMetrics = True} rest
    flag : _
      | isUnsupported flag ->
          Left $
            "Unsupported option in phase-1: "
              <> flag
              <> ". Supported options: -font, -varfont, -dimensions, -pxrange, -seed, -autoframe, -o, -format, -printmetrics"
    raw : _ -> Left ("Unexpected positional argument: " <> raw)

finalize :: ParseState -> Either String CliReq
finalize st = do
  src <- maybe (Left "Missing source: provide -font or -varfont.") Right st.psSrc
  glyph <- maybe (Left "Missing glyph code argument.") Right st.psGlyph
  dim <- firstInvalid "dimensions" (mkDim st.psDim)
  pxr <- firstInvalid "pxrange" (mkPxRange st.psPxr)
  let cfg =
        GenCfg
          { mode = Mtsdf,
            dim = dim,
            pxr = pxr,
            seed = st.psSeed,
            autoframe = st.psAutoframe
          }
  pure
    CliReq
      { cfg = cfg,
        src = src,
        glyph = glyph,
        out = st.psOut,
        fmt = fromMaybe (inferOutFmt st.psOut) st.psFmt,
        printMetrics = st.psPrintMetrics
      }

defaultState :: ParseState
defaultState =
  ParseState
    { psSrc = Nothing,
      psGlyph = Nothing,
      psDim = 64,
      psPxr = 8.0,
      psSeed = 1,
      psAutoframe = False,
      psOut = "output.png",
      psFmt = Nothing,
      psPrintMetrics = False
    }

usage :: String
usage =
  unlines
    [ "masdiff phase-1 CLI (subset compatible)",
      "Usage:",
      "  masdiff mtsdf -font <font.ttf> <char|code> [options]",
      "  masdiff mtsdf -varfont <font.ttf?axis=val&...> <char|code> [options]",
      "",
      "Options:",
      "  -dimensions <w> <h>   (phase-1 requires square dimensions)",
      "  -pxrange <range>",
      "  -seed <n>",
      "  -autoframe",
      "  -o <output>",
      "  -format <png|rgba>",
      "  -printmetrics"
    ]

renderGenErr :: GenErr -> String
renderGenErr err =
  case err of
    InvalidCfg msg -> "Invalid configuration: " <> msg
    MissingInput msg -> "Missing input: " <> msg
    Unsupported msg -> "Unsupported: " <> msg
    ExecFailed msg -> "Generator execution failed:\n" <> msg
    ParseFailed msg -> "Generator parse failure: " <> msg

parseOutFmt :: String -> Either String OutFmt
parseOutFmt raw =
  case raw of
    "png" -> Right Png
    "rgba" -> Right Rgba
    _ -> Left ("Unsupported output format: " <> raw <> ". Use png or rgba.")

inferOutFmt :: FilePath -> OutFmt
inferOutFmt path =
  if hasSuffix ".rgba" path
    then Rgba
    else Png

parseGlyphCode :: String -> Either String GlyphCode
parseGlyphCode raw =
  case raw of
    "" -> Left "Glyph code cannot be empty."
    ('g' : _) -> Left "Glyph index form (gNNN) is not supported in phase-1."
    [charCode] ->
      firstInvalid "glyph code" (mkGlyphCode (ord charCode))
    _ | hasHexPrefix raw -> do
          code <- parseHex (drop 2 raw)
          firstInvalid "glyph code" (mkGlyphCode code)
      | otherwise -> do
          code <- parseInt "glyph code" raw
          firstInvalid "glyph code" (mkGlyphCode code)

parseVarFontSpec :: String -> Either String FontSrc
parseVarFontSpec spec =
  case splitOnce "?" spec of
    Nothing ->
      Right
        VarFontFile
          { path = spec,
            axes = Map.empty
          }
    Just (path, query) -> do
      axes <- Map.fromList <$> traverse parseAxisPair (splitBy '&' query)
      pure
        VarFontFile
          { path = path,
            axes = axes
          }

parseAxisPair :: String -> Either String (AxisTag, AxisVal)
parseAxisPair raw =
  case splitOnce "=" raw of
    Nothing -> Left ("Invalid axis entry in -varfont value: " <> raw)
    Just (name, valueRaw) -> do
      value <- parseDouble "axis value" valueRaw
      pure (AxisTag (T.pack name), AxisVal value)

ensureNoSource :: ParseState -> Either String ()
ensureNoSource st =
  case st.psSrc of
    Nothing -> Right ()
    Just _ -> Left "Only one source is allowed: use either -font or -varfont."

firstInvalid :: String -> Either String a -> Either String a
firstInvalid label result =
  case result of
    Left err -> Left ("Invalid " <> label <> ": " <> err)
    Right x -> Right x

parseInt :: String -> String -> Either String Int
parseInt label raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Invalid " <> label <> ": " <> raw)

parseHex :: String -> Either String Int
parseHex raw =
  case reads ("0x" <> raw) of
    [(x, "")] -> Right x
    _ -> Left ("Invalid hex glyph code: 0x" <> raw)

parseDouble :: String -> String -> Either String Double
parseDouble label raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Invalid " <> label <> ": " <> raw)

isUnsupported :: String -> Bool
isUnsupported raw = take 1 raw == "-"

hasHexPrefix :: String -> Bool
hasHexPrefix raw = take 2 raw == "0x" || take 2 raw == "0X"

hasSuffix :: String -> String -> Bool
hasSuffix suffix value =
  length value >= length suffix
    && drop (length value - length suffix) value == suffix

splitBy :: Char -> String -> [String]
splitBy delim = foldr step [""]
  where
    step c acc
      | c == delim = "" : acc
      | otherwise =
          case acc of
            [] -> [[c]]
            (x : xs) -> (c : x) : xs

splitOnce :: String -> String -> Maybe (String, String)
splitOnce token input = go [] input
  where
    go _ [] = Nothing
    go acc rest@(x : xs)
      | token `prefixOf` rest =
          Just (reverse acc, drop (length token) rest)
      | otherwise = go (x : acc) xs

prefixOf :: String -> String -> Bool
prefixOf prefix value = take (length prefix) value == prefix
