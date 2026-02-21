{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate
  ( RuntimeCfg (..),
    defaultRuntimeCfg,
    generateGlyphIO,
    parseBackendModeEnv,
  )
import MSDF.TextRender
  ( ScreenPxRange (..),
    ShaderCfg,
    addBorder,
    hcatWithGap,
    mkShaderCfg,
    shadeMtsdfImg,
    shadeMtsdfImgTo,
    solidImg,
  )
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    ImgRGBA8 (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkPxRange,
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

data CliReq = CliReq
  { txt :: String,
    src :: FontSrc,
    out :: FilePath,
    cfg :: GenCfg,
    shader :: ShaderCfg,
    genDimPx :: Int,
    outDimPx :: Int,
    gapPx :: Int,
    spacePx :: Int,
    borderPx :: Int,
    verbose :: Bool
  }
  deriving stock (Eq, Show)

data ParseState = ParseState
  { psTxt :: Maybe String,
    psSrc :: Maybe FontSrc,
    psOut :: FilePath,
    psGenDim :: Int,
    psOutDim :: Int,
    psPxr :: Double,
    psSpr :: Maybe Double,
    psSeed :: Int,
    psGap :: Int,
    psSpace :: Int,
    psBorder :: Int,
    psFallback :: Bool,
    psFallbackThr :: Double,
    psOvlp :: Bool,
    psVerbose :: Bool
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  runtimeResult <- readRuntime
  runtime <- case runtimeResult of
    Left err -> failWith err
    Right x -> pure x
  args <- getArgs
  req <- case parseArgs args of
    Left err -> failWith (err <> "\n" <> usage)
    Right x -> pure x
  glyphImgsResult <- traverse (renderChar runtime req) req.txt
  glyphImgs <- either failWith pure (sequence glyphImgsResult)
  line <- either failWith pure (hcatWithGap req.gapPx glyphImgs)
  framed <- either failWith pure (addBorder req.borderPx line)
  writePngRGBA8File req.out framed
  if req.verbose
    then putStrLn ("Wrote " <> req.out <> " (" <> show framed.w <> "x" <> show framed.h <> ")")
    else pure ()
  exitWith ExitSuccess

readRuntime :: IO (Either String RuntimeCfg)
readRuntime = do
  backendRaw <- lookupEnv "MASDIFF_BACKEND"
  bin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  pure $ do
    backend <- parseBackendModeEnv backendRaw
    pure RuntimeCfg {backend = backend, msdfgenBin = bin}

renderChar :: RuntimeCfg -> CliReq -> Char -> IO (Either String ImgRGBA8)
renderChar runtime req ch
  | ch == ' ' = do
      blank <- pure (solidImg req.spacePx req.outDimPx (255, 255, 255, 255))
      pure blank
  | ch == '\n' =
      pure (Left "Newline is not supported yet. Render one line at a time.")
  | otherwise =
      case mkGlyphCode (ord ch) of
        Left err -> pure (Left ("Invalid glyph " <> show ch <> ": " <> err))
        Right glyph -> do
          generated <- generateGlyphIO runtime req.cfg req.src glyph
          case generated of
            Left err ->
              pure
                (Left ("Failed glyph " <> show ch <> ": " <> renderGenErr err))
            Right out -> do
              let GenOut {img = mtsdfImg} = out
              if req.outDimPx == req.genDimPx
                then pure (shadeMtsdfImg req.shader mtsdfImg)
                else pure (shadeMtsdfImgTo req.shader req.outDimPx req.outDimPx mtsdfImg)

parseArgs :: [String] -> Either String CliReq
parseArgs args = finalize =<< parseLoop defaultState args

parseLoop :: ParseState -> [String] -> Either String ParseState
parseLoop st args =
  case args of
    [] -> Right st
    "-h" : _ -> Left "help"
    "--help" : _ -> Left "help"
    "--text" : txt : rest ->
      parseLoop st {psTxt = Just txt} rest
    "-font" : path : rest -> do
      ensureNoSource st
      parseLoop st {psSrc = Just FontFile {path = path}} rest
    "-varfont" : spec : rest -> do
      src <- parseVarFontSpec spec
      ensureNoSource st
      parseLoop st {psSrc = Just src} rest
    "-o" : path : rest ->
      parseLoop st {psOut = path} rest
    "--dim" : raw : rest -> do
      x <- parseInt "--dim" raw
      parseLoop st {psOutDim = x} rest
    "--gen-dim" : raw : rest -> do
      x <- parseInt "--gen-dim" raw
      parseLoop st {psGenDim = x} rest
    "--pxrange" : raw : rest -> do
      x <- parseDouble "--pxrange" raw
      parseLoop st {psPxr = x} rest
    "--screen-px-range" : raw : rest -> do
      x <- parseDouble "--screen-px-range" raw
      parseLoop st {psSpr = Just x} rest
    "--seed" : raw : rest -> do
      x <- parseInt "--seed" raw
      parseLoop st {psSeed = x} rest
    "--gap" : raw : rest -> do
      x <- parseInt "--gap" raw
      parseLoop st {psGap = x} rest
    "--space" : raw : rest -> do
      x <- parseInt "--space" raw
      parseLoop st {psSpace = x} rest
    "--border" : raw : rest -> do
      x <- parseInt "--border" raw
      parseLoop st {psBorder = x} rest
    "--no-overlap-fix" : rest ->
      parseLoop st {psOvlp = False} rest
    "--overlap-fix" : rest ->
      parseLoop st {psOvlp = True} rest
    "--fallback-threshold" : raw : rest -> do
      x <- parseDouble "--fallback-threshold" raw
      parseLoop st {psFallbackThr = x} rest
    "--alpha-fallback" : rest ->
      parseLoop st {psFallback = True} rest
    "--no-alpha-fallback" : rest ->
      parseLoop st {psFallback = False} rest
    "--verbose" : rest ->
      parseLoop st {psVerbose = True} rest
    raw : _ ->
      Left ("Unknown argument: " <> raw)

finalize :: ParseState -> Either String CliReq
finalize st = do
  txt <- maybe (Left "Missing --text") Right st.psTxt
  src <- maybe (Left "Missing source. Use -font or -varfont.") Right st.psSrc
  if null txt
    then Left "--text cannot be empty."
    else pure ()
  if any (== '\n') txt
    then Left "Newline is not supported yet."
    else pure ()
  dim <- firstInvalid "gen-dim" (mkDim st.psGenDim)
  pxr <- firstInvalid "pxrange" (mkPxRange st.psPxr)
  if st.psOutDim <= 0
    then Left "--dim must be > 0."
    else pure ()
  if st.psGap < 0
    then Left "--gap must be >= 0."
    else pure ()
  if st.psSpace <= 0
    then Left "--space must be > 0."
    else pure ()
  if st.psBorder < 0
    then Left "--border must be >= 0."
    else pure ()
  shader <- mkShaderCfg spr st.psFallback st.psFallbackThr
  let cfg =
        GenCfg
          { mode = Mtsdf,
            dim = dim,
            pxr = pxr,
            seed = st.psSeed,
            autoframe = True,
            ovlp = st.psOvlp
          }
  pure
    CliReq
      { txt = txt,
        src = src,
        out = st.psOut,
        cfg = cfg,
        shader = shader,
        genDimPx = st.psGenDim,
        outDimPx = st.psOutDim,
        gapPx = st.psGap,
        spacePx = st.psSpace,
        borderPx = st.psBorder,
        verbose = st.psVerbose
      }
  where
    spr =
      case st.psSpr of
        Nothing -> AutoPxRange st.psPxr
        Just x -> FixedPxRange x

defaultState :: ParseState
defaultState =
  ParseState
    { psTxt = Nothing,
      psSrc = Nothing,
      psOut = "final.png",
      psGenDim = 96,
      psOutDim = 32,
      psPxr = 6.0,
      psSpr = Nothing,
      psSeed = 1,
      psGap = 2,
      psSpace = 12,
      psBorder = 20,
      psFallback = True,
      psFallbackThr = 0.0,
      psOvlp = False,
      psVerbose = False
    }

usage :: String
usage =
  unlines
    [ "masdiff-text-render",
      "Usage:",
      "  masdiff-text-render --text <text> (-font <font.ttf> | -varfont <font.ttf?axis=val&...>) [options]",
      "",
      "Options:",
      "  -o <output.png>            default: final.png",
      "  --dim <n>                  output glyph size, default: 32",
      "  --gen-dim <n>              field generation size, default: 96",
      "  --pxrange <x>              default: 6.0",
      "  --screen-px-range <x>      default: auto (generator-derived)",
      "  --gap <n>                  default: 2",
      "  --space <n>                default: 12",
      "  --border <n>               default: 20",
      "  --seed <n>                 default: 1",
      "  --fallback-threshold <x>   default: 0.0",
      "  --alpha-fallback           default: on",
      "  --no-alpha-fallback",
      "  --no-overlap-fix           default mode",
      "  --overlap-fix",
      "  --verbose"
    ]

ensureNoSource :: ParseState -> Either String ()
ensureNoSource st =
  case st.psSrc of
    Nothing -> Right ()
    Just _ -> Left "Only one source is allowed: use either -font or -varfont."

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

parseDouble :: String -> String -> Either String Double
parseDouble label raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Invalid " <> label <> ": " <> raw)

renderGenErr :: GenErr -> String
renderGenErr err =
  case err of
    InvalidCfg msg -> "Invalid configuration: " <> msg
    MissingInput msg -> "Missing input: " <> msg
    Unsupported msg -> "Unsupported: " <> msg
    ExecFailed msg -> "Execution failed: " <> msg
    ParseFailed msg -> "Parse failure: " <> msg

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

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 2)
