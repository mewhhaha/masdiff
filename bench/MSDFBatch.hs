module Main (main) where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Exception (bracket_)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Data.Array (array, bounds, (!))
import Data.Char (ord, toLower)
import Data.List (sort, sortOn)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import Numeric (showFFloat)
import System.Environment (getArgs)
import Text.Printf (printf)

import MSDF.Generated (BuildTimings(..), generateMSDFFromTTFWithTimingsNoCaps)
import MSDF.MSDF (GlyphSet(..), defaultMSDFConfig, effectiveParallelism)
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (TTF(..), Cmap(..), Maxp(..), Loca(..), ParseError(..), parseTTF)
import MSDF.Types (BitmapFormat(..))
import Paths_masdiff (getDataFileName)

data OpszMode
  = OpszNone
  | OpszAuto
  | OpszFixed Double
  deriving (Eq, Show)

data Options = Options
  { fontPath :: FilePath
  , italicFontPath :: Maybe FilePath
  , pixelSize :: Int
  , sizesOpt :: Maybe [Int]
  , weights :: [Double]
  , styles :: [String]
  , opszMode :: OpszMode
  , vars :: [(String, Double)]
  , glyphCount :: Maybe Int
  , maxGlyphs :: Maybe Int
  , textOpt :: Maybe String
  , pack :: Bool
  , buildImage :: Bool
  , format :: BitmapFormat
  , jobs :: Maybe Int
  , innerParallelism :: Maybe Int
  , repeats :: Int
  , warmups :: Int
  } deriving (Eq, Show)

data Task = Task
  { taskLabel :: String
  , taskCfg :: MSDF.MSDFConfig
  , taskTTF :: TTF
  }

data TaskResult = TaskResult
  { timing :: BuildTimings
  , wallMs :: Double
  }

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs args
  fontPath' <- if null opts.fontPath
    then getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
    else pure opts.fontPath
  ttfRegular <- loadTTF fontPath'
  ttfItalic <- case opts.italicFontPath of
    Nothing -> pure ttfRegular
    Just path -> loadTTF path
  let ttfRegular' = clampMaybe opts.maxGlyphs ttfRegular
      ttfItalic' = clampMaybe opts.maxGlyphs ttfItalic
      glyphSet = buildGlyphSet opts ttfRegular'
      sizes = case opts.sizesOpt of
        Nothing -> [opts.pixelSize]
        Just xs -> xs
      styles' = normalizeStyles opts.styles
      weights' = if null opts.weights then [Nothing] else map Just opts.weights
      jobsEff = maybe (effectiveParallelism 0) effectiveParallelism opts.jobs
      tasks = buildTasks opts glyphSet ttfRegular' ttfItalic' sizes styles' weights'
      innerEff =
        case opts.innerParallelism of
          Just n -> n
          Nothing ->
            if length tasks <= jobsEff
              then 0
              else 1
  caps <- getNumCapabilities
  procs <- getNumProcessors
  printf "batch: tasks=%d jobs=%d inner=%d caps=%d procs=%d pack=%s format=%s\n"
    (length tasks) jobsEff innerEff caps procs (show opts.pack) (show opts.format)
  whenItalicMissing opts styles'
  let opts' = opts { innerParallelism = Just innerEff }
  withCapabilities jobsEff $ runBatch opts' tasks

loadTTF :: FilePath -> IO TTF
loadTTF path = do
  parsed <- parseTTF path
  case parsed of
    Left (ParseError ctx msg) -> error (ctx ++ ": " ++ msg)
    Right val -> pure val

runBatch :: Options -> [Task] -> IO ()
runBatch opts tasks = do
  start <- getMonotonicTimeNSec
  taskChan <- newChan
  resultChan <- newChan
  let worker = do
        mt <- readChan taskChan
        case mt of
          Nothing -> pure ()
          Just t -> do
            result <- runTask opts t
            writeChan resultChan (t.taskLabel, result)
            worker
  let workers = max 1 (effectiveParallelism (maybe 0 id opts.jobs))
  forM_ [1 .. workers] (\_ -> forkIO worker)
  forM_ tasks (\t -> writeChan taskChan (Just t))
  replicateM_ workers (writeChan taskChan Nothing)
  results <- forM [1 .. length tasks] (\_ -> readChan resultChan)
  end <- getMonotonicTimeNSec
  let wallMs = fromIntegral (end - start) / 1.0e6
      totals = map (\(_, r) -> r.timing.totalMs) results
      renders = map (\(_, r) -> r.timing.renderMs) results
      avgTotal = if null totals then 0 else sum totals / fromIntegral (length totals)
      avgRender = if null renders then 0 else sum renders / fromIntegral (length renders)
  forM_ results $ \(label, r) -> printTask label r
  printf "batch summary: wall=%.2f ms avg-total=%.2f ms avg-render=%.2f ms\n"
    wallMs avgTotal avgRender

runTask :: Options -> Task -> IO TaskResult
runTask opts t = do
  replicateM_ opts.warmups (generateMSDFFromTTFWithTimingsNoCaps t.taskCfg t.taskTTF >> pure ())
  runs <- replicateM (max 1 opts.repeats) (timedRun t.taskCfg t.taskTTF)
  let timings = avgTimings (map fst runs)
      wall = avgWall runs
  pure TaskResult { timing = timings, wallMs = wall }
  where
    timedRun cfg ttf = do
      start <- getMonotonicTimeNSec
      (_, timing) <- generateMSDFFromTTFWithTimingsNoCaps cfg ttf
      end <- getMonotonicTimeNSec
      let wall = fromIntegral (end - start) / 1.0e6
      pure (timing, wall)
    avgWall xs =
      let n = fromIntegral (length xs)
      in if n <= 0 then 0 else sum (map snd xs) / n

printTask :: String -> TaskResult -> IO ()
printTask label r = do
  let t = r.timing
  printf "task[%s]: wall=%.2f ms total=%.2f ms render=%.2f ms pack-place=%.2f ms pack-image=%.2f ms kerning=%.2f ms marks=%.2f ms rects=%d"
    label r.wallMs t.totalMs t.renderMs t.packPlaceMs t.packImageMs t.kerningMs t.marksMs t.packRectCount
  case t.atlasSize of
    Nothing -> printf " atlas=none\n"
    Just (w, h) -> printf " atlas=%dx%d\n" w h

withCapabilities :: Int -> IO a -> IO a
withCapabilities n action
  | n <= 0 = action
  | otherwise = do
      current <- getNumCapabilities
      if current == n
        then action
        else bracket_ (setNumCapabilities n) (setNumCapabilities current) action

buildTasks :: Options -> GlyphSet -> TTF -> TTF -> [Int] -> [String] -> [Maybe Double] -> [Task]
buildTasks opts glyphSet ttfRegular ttfItalic sizes styles weights =
  [ Task (taskLabel style weight size opszVal)
         (buildConfig opts glyphSet size (mergeVars opts.vars weight opszVal))
         (if isItalicStyle style then ttfItalic else ttfRegular)
  | size <- sizes
  , style <- styles
  , weight <- weights
  , let opszVal = opszValue opts.opszMode size
  ]

buildConfig :: Options -> GlyphSet -> Int -> [(String, Double)] -> MSDF.MSDFConfig
buildConfig opts glyphSet size vars =
  let cfgBase = defaultMSDFConfig
        { MSDF.pixelSize = size
        , MSDF.glyphSet = glyphSet
        , MSDF.variations = vars
        , MSDF.outputFormat = opts.format
        , MSDF.parallelism = maybe 0 id opts.innerParallelism
        }
      atlasCfg = cfgBase.atlas
        { MSDF.packAtlas = opts.pack
        , MSDF.buildAtlasImage = opts.buildImage
        }
  in cfgBase { MSDF.atlas = atlasCfg }

mergeVars :: [(String, Double)] -> Maybe Double -> Maybe Double -> [(String, Double)]
mergeVars base weight opszVal =
  let base' = maybe base (\v -> setVar "wght" v base) weight
  in maybe base' (\v -> setVar "opsz" v base') opszVal

setVar :: String -> Double -> [(String, Double)] -> [(String, Double)]
setVar tag val vars = (tag, val) : filter (\(t, _) -> t /= tag) vars

opszValue :: OpszMode -> Int -> Maybe Double
opszValue mode size =
  case mode of
    OpszNone -> Nothing
    OpszAuto -> Just (fromIntegral size)
    OpszFixed v -> Just v

taskLabel :: String -> Maybe Double -> Int -> Maybe Double -> String
taskLabel style weight size opszVal =
  let base = normalizeStyle style
      w = maybe "" (\v -> "_w" ++ fmtNum v) weight
      o = maybe "" (\v -> "_opsz" ++ fmtNum v) opszVal
  in base ++ w ++ o ++ "_px" ++ show size

fmtNum :: Double -> String
fmtNum x =
  let r = round x :: Int
  in if abs (x - fromIntegral r) < 1.0e-6
       then show r
       else showFFloat (Just 2) x ""

normalizeStyles :: [String] -> [String]
normalizeStyles xs =
  case map normalizeStyle xs of
    [] -> ["regular"]
    ys -> ys

normalizeStyle :: String -> String
normalizeStyle = map toLower . filter (/= ' ')

isItalicStyle :: String -> Bool
isItalicStyle s =
  let n = normalizeStyle s
  in n == "italic" || n == "bolditalic" || n == "oblique"

whenItalicMissing :: Options -> [String] -> IO ()
whenItalicMissing opts styles =
  if opts.italicFontPath == Nothing && any isItalicStyle styles
    then putStrLn "batch: italic styles requested but no --italic-font provided; using regular font."
    else pure ()

buildGlyphSet :: Options -> TTF -> GlyphSet
buildGlyphSet opts ttf =
  case opts.textOpt of
    Just txt -> GlyphSetCodepoints (uniqueSortedInts (map ord txt))
    Nothing ->
      case opts.glyphCount of
        Nothing -> GlyphSetAll
        Just n ->
          let mappings = ttf.cmap.mappings
          in GlyphSetCodepoints (takeUnique n (map fst (sortOn fst mappings)))

clampMaybe :: Maybe Int -> TTF -> TTF
clampMaybe n ttf =
  case n of
    Nothing -> ttf
    Just count -> clampGlyphCount count ttf

clampGlyphCount :: Int -> TTF -> TTF
clampGlyphCount n ttf =
  let maxG = ttf.maxp.numGlyphs
      target = max 1 (min n maxG)
      offsets0 = ttf.loca.offsets
      (lo, hi) = bounds offsets0
      hi' = min hi target
      offsets' = if lo > hi' then array (0, -1) [] else array (0, hi') [ (i, offsets0 ! i) | i <- [lo .. hi'] ]
      mappings' = [ (cp, g) | (cp, g) <- ttf.cmap.mappings, g >= 0, g < target ]
  in ttf
      { maxp = ttf.maxp { numGlyphs = target }
      , loca = ttf.loca { offsets = offsets' }
      , cmap = ttf.cmap { mappings = mappings' }
      }

uniqueSortedInts :: [Int] -> [Int]
uniqueSortedInts xs =
  case sort xs of
    [] -> []
    (y:ys) -> y : go y ys
  where
    go _ [] = []
    go prev (z:zs)
      | z == prev = go prev zs
      | otherwise = z : go z zs

takeUnique :: Int -> [Int] -> [Int]
takeUnique n xs = go n Nothing xs
  where
    go 0 _ _ = []
    go _ _ [] = []
    go k prev (y:ys) =
      case prev of
        Just p | p == y -> go k prev ys
        _ -> y : go (k - 1) (Just y) ys

avgTimings :: [BuildTimings] -> BuildTimings
avgTimings ts =
  let n = fromIntegral (length ts)
      avg f = sum (map f ts) / n
      packRects = round (avg (fromIntegral . (\t -> t.packRectCount)))
  in BuildTimings
      { totalMs = avg (\t -> t.totalMs)
      , renderMs = avg (\t -> t.renderMs)
      , packPlaceMs = avg (\t -> t.packPlaceMs)
      , packImageMs = avg (\t -> t.packImageMs)
      , kerningMs = avg (\t -> t.kerningMs)
      , marksMs = avg (\t -> t.marksMs)
      , packRectCount = packRects
      , atlasSize = (head ts).atlasSize
      }

parseArgs :: [String] -> IO Options
parseArgs args = go args defaults
  where
    defaults = Options
      { fontPath = ""
      , italicFontPath = Nothing
      , pixelSize = 32
      , sizesOpt = Nothing
      , weights = []
      , styles = ["regular"]
      , opszMode = OpszNone
      , vars = []
      , glyphCount = Nothing
      , maxGlyphs = Nothing
      , textOpt = Nothing
      , pack = True
      , buildImage = True
      , format = BitmapMSDF
      , jobs = Nothing
      , innerParallelism = Nothing
      , repeats = 1
      , warmups = 0
      }
    go [] acc = pure acc
    go ("--font":path:rest) acc = go rest acc { fontPath = path }
    go ("--italic-font":path:rest) acc = go rest acc { italicFontPath = Just path }
    go ("--pixel-size":v:rest) acc = go rest acc { pixelSize = read v }
    go ("--sizes":v:rest) acc = go rest acc { sizesOpt = Just (parseSizes v) }
    go ("--weights":v:rest) acc = go rest acc { weights = map read (splitComma v) }
    go ("--styles":v:rest) acc = go rest acc { styles = splitComma v }
    go ("--opsz":v:rest) acc = go rest acc { opszMode = parseOpsz v }
    go ("--glyphs":v:rest) acc = go rest acc { glyphCount = Just (read v) }
    go ("--max-glyphs":v:rest) acc = go rest acc { maxGlyphs = Just (read v) }
    go ("--text":v:rest) acc = go rest acc { textOpt = Just v }
    go ("--pack":rest) acc = go rest acc { pack = True }
    go ("--no-pack":rest) acc = go rest acc { pack = False }
    go ("--image":rest) acc = go rest acc { buildImage = True }
    go ("--no-image":rest) acc = go rest acc { buildImage = False }
    go ("--format":v:rest) acc = go rest acc { format = parseFormat v }
    go ("--jobs":v:rest) acc = go rest acc { jobs = Just (read v) }
    go ("--parallelism":v:rest) acc = go rest acc { innerParallelism = Just (read v) }
    go ("--repeat":v:rest) acc = go rest acc { repeats = read v }
    go ("--warmup":v:rest) acc = go rest acc { warmups = read v }
    go ("--var":v:rest) acc =
      case break (== '=') v of
        (tag, '=':val) -> go rest acc { vars = acc.vars ++ [(tag, read val)] }
        _ -> error ("invalid --var value: " ++ v ++ " (expected TAG=VALUE)")
    go (path:rest) acc = go rest acc { fontPath = path }

parseOpsz :: String -> OpszMode
parseOpsz v =
  case map toLower v of
    "auto" -> OpszAuto
    _ -> OpszFixed (read v)

parseFormat :: String -> BitmapFormat
parseFormat v =
  case map toLower v of
    "msdf" -> BitmapMSDF
    "mtsdf" -> BitmapMTSDF
    _ -> error ("invalid --format value: " ++ v ++ " (expected msdf or mtsdf)")

parseSizes :: String -> [Int]
parseSizes s =
  case s of
    "" -> []
    _ -> map read (splitComma s)

splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',':xs) = "" : splitComma xs
splitComma (c:xs) =
  case splitComma xs of
    [] -> [[c]]
    (y:ys) -> (c:y) : ys
