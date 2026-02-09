module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.Char (ord, toUpper)
import Data.List (nub)
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

import MSDF.Generated (generateMSDFFromTTF)
import MSDF.MSDF (MSDFConfig(..), defaultMSDFConfig, renderGlyphMSDF, GlyphSet(..))
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (Cmap(..), parseTTF, ParseError(..), TTF(..))
import MSDF.Types (AtlasImage(..), BitmapFormat(..), GlyphMSDF(..), MSDFAtlas(..), MSDFBitmap(..))
import Paths_masdiff (getDataFileName)

main :: IO ()
main = do
  root <- findRepoRoot
  let snapshotsRoot = root </> "tests" </> "snapshots"
      outRoot = root </> "out" </> "snapshots"
  update <- envIsTrue "MSDF_UPDATE_SNAPSHOTS"
  updateSdl <- envIsTrue "MSDF_UPDATE_SDL_SNAPSHOT"
  runSdl <- envIsTrue "MSDF_RUN_SDL_SNAPSHOT"
  whenTrue update (putStrLn "[snapshot] update mode enabled")
  fontPath <- getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
  ttf <- requireRight "parseTTF" =<< parseTTF fontPath
  results <- mapM (runTest update snapshotsRoot outRoot ttf) snapshotCases
  sdlOk <-
    if runSdl
    then runExternalTest "sdl_render_snapshot" (runSdlSnapshot root updateSdl)
    else pure True
  if and results && sdlOk
    then putStrLn "All tests passed."
    else exitFailure

runTest :: Bool -> FilePath -> FilePath -> TTF -> SnapshotCase -> IO Bool
runTest update snapshotsRoot outRoot ttf snap = do
  putStrLn ("[snapshot] " ++ snap.name)
  hFlush stdout
  result <- (try (snapshotTest update snapshotsRoot outRoot ttf snap) :: IO (Either SomeException ()))
  case result of
    Left e -> do
      hPutStrLn stderr ("  FAIL: " ++ show e)
      pure False
    Right _ -> pure True

runExternalTest :: String -> IO () -> IO Bool
runExternalTest name action = do
  putStrLn ("[snapshot] " ++ name)
  hFlush stdout
  result <- (try action :: IO (Either SomeException ()))
  case result of
    Left e -> do
      hPutStrLn stderr ("  FAIL: " ++ show e)
      pure False
    Right _ -> pure True

snapshotTest :: Bool -> FilePath -> FilePath -> TTF -> SnapshotCase -> IO ()
snapshotTest update snapshotsRoot outRoot ttf snap = do
  files <- renderSnapshotFiles snap ttf
  mapM_ (compareSnapshotFile update snapshotsRoot outRoot) files

-- Snapshot cases ------------------------------------------------------------

data SnapshotKind = SnapGlyphs | SnapAtlas

instance Show SnapshotKind where
  show SnapGlyphs = "glyphs"
  show SnapAtlas = "atlas"

data SnapshotCase = SnapshotCase
  { name :: String
  , kind :: SnapshotKind
  , text :: String
  , config :: MSDFConfig
  }

snapshotCases :: [SnapshotCase]
snapshotCases =
  [ SnapshotCase
      { name = "mtsdf_glyphs"
      , kind = SnapGlyphs
      , text = "fmad"
      , config = glyphConfig 128 12
      }
  , SnapshotCase
      { name = "mtsdf_atlas"
      , kind = SnapAtlas
      , text = "masdiff"
      , config = atlasConfig 64 12 "masdiff"
      }
  ]

baseConfig :: Int -> Int -> MSDFConfig
baseConfig px rangePx =
  defaultMSDFConfig
    { pixelSize = px
    , range = rangePx
    , outputFormat = BitmapMTSDF
    , parallelism = 1
    }

glyphConfig :: Int -> Int -> MSDFConfig
glyphConfig px rangePx =
  let cfg = baseConfig px rangePx
      MSDFConfig { atlas = atlasCfg } = cfg
      atlasCfg' = atlasCfg { MSDF.packAtlas = False, MSDF.buildAtlasImage = False }
  in cfg { MSDF.atlas = atlasCfg' }

atlasConfig :: Int -> Int -> String -> MSDFConfig
atlasConfig px rangePx txt =
  let cfg = baseConfig px rangePx
      MSDFConfig { atlas = atlasCfg } = cfg
      atlasCfg' = atlasCfg
        { MSDF.packAtlas = True
        , MSDF.buildAtlasImage = True
        , MSDF.atlasPadding = 16
        }
  in cfg
      { glyphSet = GlyphSetCodepoints (map ord txt)
      , MSDF.atlas = atlasCfg'
      }

-- Snapshot rendering --------------------------------------------------------

data SnapshotFile = SnapshotFile
  { relPath :: FilePath
  , width :: Int
  , height :: Int
  , bytes :: BS.ByteString
  }

renderSnapshotFiles :: SnapshotCase -> TTF -> IO [SnapshotFile]
renderSnapshotFiles snap ttf =
  case snap.kind of
    SnapGlyphs -> do
      let cps = nub (map ord snap.text)
      mapM (renderGlyphSnapshot snap ttf) cps
    SnapAtlas -> do
      let atlas = generateMSDFFromTTF snap.config ttf
      case atlas of
        MSDFAtlas { atlas = atlasImg } ->
          case atlasImg of
            Nothing -> error "snapshot atlas image missing"
            Just img -> pure [renderAtlasSnapshot snap img]

renderGlyphSnapshot :: SnapshotCase -> TTF -> Int -> IO SnapshotFile
renderGlyphSnapshot snap ttf cp =
  case lookup cp (ttfMappings ttf) of
    Nothing -> error ("missing codepoint U+" ++ padHex cp)
    Just glyphIndex -> do
      let glyph = renderGlyphMSDF snap.config ttf glyphIndex
          GlyphMSDF { bitmap = bmp } = glyph
          MSDFBitmap { width = bmpW, height = bmpH, format = bmpFmt, pixels = bmpPx } = bmp
          rgba = bitmapToRgba bmpFmt bmpPx
          outRel = snap.name </> ("U+" ++ padHex cp ++ ".rgba")
      pure SnapshotFile
        { relPath = outRel
        , width = bmpW
        , height = bmpH
        , bytes = BS.pack (UA.elems rgba)
        }

renderAtlasSnapshot :: SnapshotCase -> AtlasImage -> SnapshotFile
renderAtlasSnapshot snap img =
  let AtlasImage { width = imgW, height = imgH, format = imgFmt, pixels = imgPx } = img
      rgba = bitmapToRgba imgFmt imgPx
      outRel = snap.name </> "atlas.rgba"
  in SnapshotFile
      { relPath = outRel
      , width = imgW
      , height = imgH
      , bytes = BS.pack (UA.elems rgba)
      }

-- Snapshot comparison -------------------------------------------------------

compareSnapshotFile :: Bool -> FilePath -> FilePath -> SnapshotFile -> IO ()
compareSnapshotFile update snapshotsRoot outRoot file = do
  let snapPath = snapshotsRoot </> file.relPath
      outPath = outRoot </> file.relPath
  if update
    then writeSnapshot snapPath file.bytes
    else do
      exists <- doesFileExist snapPath
      if not exists
        then error ("missing snapshot: " ++ snapPath ++ " (run with MSDF_UPDATE_SNAPSHOTS=1)")
        else do
          ref <- BS.readFile snapPath
          let expectedLen = file.width * file.height * 4
              refLen = BS.length ref
              curLen = BS.length file.bytes
          if refLen /= expectedLen || curLen /= expectedLen
            then error ("snapshot size mismatch for " ++ snapPath ++ ": expected " ++ show expectedLen ++ " bytes, got ref " ++ show refLen ++ " current " ++ show curLen)
            else if ref == file.bytes
              then pure ()
              else do
                let (maxDiff, meanDiff, mismatch, diffBytes) = diffStats ref file.bytes
                writeSnapshot outPath file.bytes
                writeSnapshot (outPath ++ ".diff.rgba") diffBytes
                error ("snapshot mismatch: " ++ snapPath ++ " (maxDiff=" ++ show maxDiff ++ ", meanDiff=" ++ show meanDiff ++ ", mismatchBytes=" ++ show mismatch ++ ", wrote=" ++ outPath ++ ")")

writeSnapshot :: FilePath -> BS.ByteString -> IO ()
writeSnapshot path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path contents

runSdlSnapshot :: FilePath -> Bool -> IO ()
runSdlSnapshot root update = do
  let script = root </> "tools" </> "sdl_render_snapshot.sh"
      mode = if update then "update" else "compare"
  (code, out, err) <- readProcessWithExitCode script [mode] ""
  if not (null out) then putStr out else pure ()
  if not (null err) then hPutStrLn stderr err else pure ()
  case code of
    ExitSuccess -> pure ()
    ExitFailure 2 -> do
      hPutStrLn stderr "  SKIP: SDL render snapshot (backend unavailable or missing ref)"
      pure ()
    ExitFailure n -> error ("sdl render snapshot failed (exit " ++ show n ++ ")")

-- Utilities ----------------------------------------------------------------

envIsTrue :: String -> IO Bool
envIsTrue key = do
  v <- lookupEnv key
  pure (case fmap (map toUpper) v of
    Just "1" -> True
    Just "TRUE" -> True
    Just "YES" -> True
    Just "ON" -> True
    _ -> False)

whenTrue :: Bool -> IO () -> IO ()
whenTrue cond action = if cond then action else pure ()

requireRight :: String -> Either ParseError a -> IO a
requireRight label result =
  case result of
    Left err -> error (label ++ ": " ++ err.context ++ ": " ++ err.message)
    Right val -> pure val

findRepoRoot :: IO FilePath
findRepoRoot = do
  cwd <- getCurrentDirectory
  go cwd
  where
    go dir = do
      let cabalPath = dir </> "masdiff.cabal"
      exists <- doesFileExist cabalPath
      if exists
        then pure dir
        else do
          let parent = takeDirectory dir
          if parent == dir
            then error "could not find masdiff.cabal"
            else go parent

ttfMappings :: TTF -> [(Int, Int)]
ttfMappings ttf =
  case ttf of
    TTF { cmap = Cmap ms } -> ms

padHex :: Int -> String
padHex cp =
  let hex = map toUpper (showHex cp "")
      minWidth = if cp <= 0xFFFF then 4 else 6
      pad = replicate (max 0 (minWidth - length hex)) '0'
  in pad ++ hex

bitmapToRgba :: BitmapFormat -> UA.UArray Int Word8 -> UA.UArray Int Word8
bitmapToRgba fmt arr =
  let rgbaList = go fmt (UA.elems arr)
  in if null rgbaList
    then UA.listArray (0, -1) []
    else UA.listArray (0, length rgbaList - 1) rgbaList
  where
    go BitmapMSDF (r:g:b:rest) = r:g:b:255:go BitmapMSDF rest
    go BitmapMTSDF (r:g:b:a:rest) = r:g:b:a:go BitmapMTSDF rest
    go _ [] = []
    go BitmapMSDF _ = error "bitmapToRgba: invalid MSDF RGB length"
    go BitmapMTSDF _ = error "bitmapToRgba: invalid MTSDF RGBA length"

diffStats :: BS.ByteString -> BS.ByteString -> (Int, Double, Int, BS.ByteString)
diffStats ref cur =
  let diff8 a b = if a >= b then a - b else b - a
      diffsList = BS.zipWith diff8 ref cur
      maxDiff = if null diffsList then 0 else fromIntegral (maximum diffsList) :: Int
      (total, mismatch) = foldl'
        (\(acc, miss) w -> (acc + fromIntegral w, if w == 0 then miss else miss + 1))
        (0 :: Int, 0 :: Int)
        diffsList
      count = length diffsList
      meanDiff = if count == 0 then 0 else fromIntegral total / fromIntegral count
      diffBytes = BS.pack diffsList
  in (maxDiff, meanDiff, mismatch, diffBytes)
