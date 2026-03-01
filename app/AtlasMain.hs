{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Char (ord)
import Data.Maybe (fromMaybe)
import MSDF.Atlas
  ( Atlas (..),
    AtlasCfg,
    AtlasPage (..),
    generateAtlasIO,
    mkAtlasCfg,
    renderAtlasTsv,
  )
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate
  ( RuntimeCfg (..),
    defaultRuntimeCfg,
    parseBackendModeEnv,
  )
import MSDF.VarFont (parseVarFontSpec)
import MSDF.Types
  ( FontSrc (..),
    GenCfg (..),
    GlyphCode,
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkPxRange,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

data CliReq = CliReq
  { txt :: String,
    src :: FontSrc,
    outPrefix :: FilePath,
    cfg :: GenCfg,
    atlasCfg :: AtlasCfg,
    jobs :: Int,
    verbose :: Bool
  }
  deriving stock (Eq, Show)

data ParseState = ParseState
  { psTxt :: Maybe String,
    psSrc :: Maybe FontSrc,
    psOutPrefix :: FilePath,
    psGenDim :: Int,
    psPxr :: Double,
    psSeed :: Int,
    psOvlp :: Bool,
    psAtlasW :: Int,
    psAtlasH :: Int,
    psPad :: Int,
    psJobs :: Int,
    psVerbose :: Bool
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  runtime <- either failWith pure =<< readRuntime
  args <- getArgs
  req <-
    case parseArgs args of
      Left "help" -> do
        putStr usage
        exitWith ExitSuccess
      Left err ->
        failWith (err <> "\n" <> usage)
      Right req ->
        pure req
  glyphs <- either failWith pure (parseGlyphs req.txt)
  atlasResult <- generateAtlasIO runtime req.jobs req.atlasCfg req.cfg req.src glyphs
  atlas <- either failWith pure atlasResult
  createDirectoryIfMissing True (takeDirectory req.outPrefix)
  traverse_ (writePage req.outPrefix) atlas.pages
  writeFile (req.outPrefix <> ".tsv") (renderAtlasTsv atlas)
  if req.verbose
    then
      putStrLn
        ( "Wrote atlas: pages="
            <> show (length atlas.pages)
            <> ", glyphs="
            <> show (length atlas.entries)
            <> ", meta="
            <> req.outPrefix
            <> ".tsv"
        )
    else pure ()
  exitWith ExitSuccess

readRuntime :: IO (Either String RuntimeCfg)
readRuntime = do
  backendRaw <- lookupEnv "MASDIFF_BACKEND"
  bin <- fromMaybe defaultRuntimeCfg.msdfgenBin <$> lookupEnv "MSDFGEN_BIN"
  pure $ do
    backend <- parseBackendModeEnv backendRaw
    pure RuntimeCfg {backend = backend, msdfgenBin = bin}

parseGlyphs :: String -> Either String [GlyphCode]
parseGlyphs txt
  | null txt = Left "--text cannot be empty."
  | any (== '\n') txt = Left "--text cannot contain newline."
  | otherwise = traverse toGlyph txt
  where
    toGlyph ch =
      case mkGlyphCode (ord ch) of
        Left err -> Left ("Invalid glyph " <> show ch <> ": " <> err)
        Right glyph -> Right glyph

writePage :: FilePath -> AtlasPage -> IO ()
writePage prefix page =
  writePngRGBA8File (pagePath prefix page.idx) page.img

pagePath :: FilePath -> Int -> FilePath
pagePath prefix pageIdx =
  prefix <> ".page-" <> padInt 3 pageIdx <> ".png"

padInt :: Int -> Int -> String
padInt w x =
  let s = show x
      n = max 0 (w - length s)
   in replicate n '0' <> s

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
    "--out-prefix" : prefix : rest ->
      parseLoop st {psOutPrefix = prefix} rest
    "--gen-dim" : raw : rest -> do
      x <- parseInt "--gen-dim" raw
      parseLoop st {psGenDim = x} rest
    "--pxrange" : raw : rest -> do
      x <- parseDouble "--pxrange" raw
      parseLoop st {psPxr = x} rest
    "--seed" : raw : rest -> do
      x <- parseInt "--seed" raw
      parseLoop st {psSeed = x} rest
    "--atlas-w" : raw : rest -> do
      x <- parseInt "--atlas-w" raw
      parseLoop st {psAtlasW = x} rest
    "--atlas-h" : raw : rest -> do
      x <- parseInt "--atlas-h" raw
      parseLoop st {psAtlasH = x} rest
    "--padding" : raw : rest -> do
      x <- parseInt "--padding" raw
      parseLoop st {psPad = x} rest
    "--jobs" : raw : rest -> do
      x <- parseInt "--jobs" raw
      parseLoop st {psJobs = x} rest
    "--overlap-fix" : rest ->
      parseLoop st {psOvlp = True} rest
    "--no-overlap-fix" : rest ->
      parseLoop st {psOvlp = False} rest
    "--verbose" : rest ->
      parseLoop st {psVerbose = True} rest
    raw : _ ->
      Left ("Unknown argument: " <> raw)

finalize :: ParseState -> Either String CliReq
finalize st = do
  txt <- maybe (Left "Missing --text") Right st.psTxt
  src <- maybe (Left "Missing source: use -font or -varfont.") Right st.psSrc
  dim <- firstInvalid "gen-dim" (mkDim st.psGenDim)
  pxr <- firstInvalid "pxrange" (mkPxRange st.psPxr)
  atlasCfg <- mkAtlasCfg st.psAtlasW st.psAtlasH st.psPad
  if st.psJobs <= 0
    then Left "--jobs must be > 0."
    else pure ()
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
        outPrefix = st.psOutPrefix,
        cfg = cfg,
        atlasCfg = atlasCfg,
        jobs = st.psJobs,
        verbose = st.psVerbose
      }

defaultState :: ParseState
defaultState =
  ParseState
    { psTxt = Nothing,
      psSrc = Nothing,
      psOutPrefix = "out/atlas/atlas",
      psGenDim = 64,
      psPxr = 8.0,
      psSeed = 1,
      psOvlp = False,
      psAtlasW = 1024,
      psAtlasH = 1024,
      psPad = 1,
      psJobs = 1,
      psVerbose = False
    }

usage :: String
usage =
  unlines
    [ "masdiff-atlas",
      "Usage:",
      "  masdiff-atlas --text <text> (-font <font.ttf> | -varfont <font.ttf?axis=val&...>) [options]",
      "",
      "Options:",
      "  --out-prefix <path>   default: out/atlas/atlas",
      "  --gen-dim <n>         default: 64",
      "  --pxrange <x>         default: 8.0",
      "  --seed <n>            default: 1",
      "  --atlas-w <n>         default: 1024",
      "  --atlas-h <n>         default: 1024",
      "  --padding <n>         default: 1",
      "  --jobs <n>            default: 1",
      "  --overlap-fix         enable overlap correction",
      "  --no-overlap-fix      default mode",
      "  --verbose"
    ]

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

parseDouble :: String -> String -> Either String Double
parseDouble label raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Invalid " <> label <> ": " <> raw)

traverse_ :: Applicative f => (a -> f b) -> [a] -> f ()
traverse_ f = foldr (\x acc -> f x *> acc) (pure ())

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 2)
