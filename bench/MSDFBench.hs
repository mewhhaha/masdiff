module Main (main) where

import Data.Array (array, bounds, (!))
import Control.Monad (replicateM, replicateM_, forM_)
import Data.Char (ord)
import Data.List (sortOn, sort)
import System.Environment (getArgs)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import Text.Printf (printf)

import MSDF.Generated (generateMSDFFromTTFWithTimings, BuildTimings(..))
import MSDF.MSDF (GlyphSet(..), defaultMSDFConfig, effectiveParallelism)
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (TTF(..), Cmap(..), Maxp(..), Loca(..), ParseError(..), parseTTF)
import Paths_masdiff (getDataFileName)

main :: IO ()
main = do
  args <- getArgs
  (fontPath, pixelSize, sizesOpt, glyphCount, vars, maxGlyphs, packOnly, textOpt, repeats, warmups, parOpt) <- parseArgs args
  fontPath' <- if null fontPath then getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf" else pure fontPath
  parsed <- parseTTF fontPath'
  ttf <- case parsed of
    Left (ParseError ctx msg) -> error (ctx ++ ": " ++ msg)
    Right val -> pure val
  let ttf' = case maxGlyphs of
        Nothing -> ttf
        Just n -> clampGlyphCount n ttf
      mappings = case ttf' of
        TTF { cmap = Cmap ms } -> ms
      glyphSet = case textOpt of
        Just txt -> GlyphSetCodepoints (uniqueSortedInts (map ord txt))
        Nothing ->
          case glyphCount of
            Nothing -> GlyphSetAll
            Just n -> GlyphSetCodepoints (takeUnique n (map fst (sortOn fst mappings)))
      sizes = case sizesOpt of
        Nothing -> [pixelSize]
        Just xs -> xs
      parCfg = case parOpt of
        Nothing -> 0
        Just n -> n
  caps <- getNumCapabilities
  procs <- getNumProcessors
  let eff = effectiveParallelism parCfg
  printf "parallelism: cfg=%d effective=%d caps=%d procs=%d\n" parCfg eff caps procs
  if length sizes > 1
    then do
      printf "sizes: %s\n" (show sizes)
      forSize ttf' glyphSet vars packOnly repeats warmups sizes
    else do
      let cfgBase = defaultMSDFConfig
            { MSDF.pixelSize = head sizes
            , MSDF.glyphSet = glyphSet
            , MSDF.variations = vars
            , MSDF.parallelism = parCfg
            }
          cfgNoPack = cfgBase { MSDF.atlas = cfgBase.atlas { MSDF.packAtlas = False } }
          cfgPack = cfgBase { MSDF.atlas = cfgBase.atlas { MSDF.packAtlas = True } }
      timingsNoPack <-
        if packOnly
        then pure Nothing
        else do
          t <- runTimings "no pack" cfgNoPack ttf' repeats warmups
          pure (Just t)
      timingsPack <- runTimings "pack" cfgPack ttf' repeats warmups
      case timingsNoPack of
        Nothing -> pure ()
        Just t -> do
          printf "generate (no pack avg): %.2f ms\n" t.totalMs
          printTimings "no pack breakdown" t
          printPerGlyph "no pack per-glyph" t
      printf "generate (pack avg): %.2f ms\n" timingsPack.totalMs
      printTimings "pack breakdown" timingsPack
      printPerGlyph "pack per-glyph" timingsPack
      case timingsNoPack of
        Nothing -> pure ()
        Just t -> printf "packing delta (avg): %.2f ms\n" (timingsPack.totalMs - t.totalMs)

parseArgs :: [String] -> IO (FilePath, Int, Maybe [Int], Maybe Int, [(String, Double)], Maybe Int, Bool, Maybe String, Int, Int, Maybe Int)
parseArgs args = go args ("", 32, Nothing, Nothing, [], Nothing, False, Nothing, 1, 0, Nothing)
  where
    go [] acc = pure acc
    go ("--font":path:rest) (_, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par) =
      go rest (path, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par)
    go ("--pixel-size":v:rest) (p, _, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par) =
      go rest (p, read v, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par)
    go ("--sizes":v:rest) (p, px, _, n, vars, maxGs, packOnly, txt, reps, warms, par) =
      go rest (p, px, Just (parseSizes v), n, vars, maxGs, packOnly, txt, reps, warms, par)
    go ("--glyphs":v:rest) (p, px, sizes, _, vars, maxGs, packOnly, txt, reps, warms, par) =
      go rest (p, px, sizes, Just (read v), vars, maxGs, packOnly, txt, reps, warms, par)
    go ("--max-glyphs":v:rest) (p, px, sizes, n, vars, _, packOnly, txt, reps, warms, par) =
      go rest (p, px, sizes, n, vars, Just (read v), packOnly, txt, reps, warms, par)
    go ("--pack-only":rest) (p, px, sizes, n, vars, maxGs, _, txt, reps, warms, par) =
      go rest (p, px, sizes, n, vars, maxGs, True, txt, reps, warms, par)
    go ("--text":v:rest) (p, px, sizes, n, vars, maxGs, packOnly, _, reps, warms, par) =
      go rest (p, px, sizes, n, vars, maxGs, packOnly, Just v, reps, warms, par)
    go ("--repeat":v:rest) (p, px, sizes, n, vars, maxGs, packOnly, txt, _, warms, par) =
      go rest (p, px, sizes, n, vars, maxGs, packOnly, txt, read v, warms, par)
    go ("--warmup":v:rest) (p, px, sizes, n, vars, maxGs, packOnly, txt, reps, _, par) =
      go rest (p, px, sizes, n, vars, maxGs, packOnly, txt, reps, read v, par)
    go ("--parallelism":v:rest) (p, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, _) =
      go rest (p, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, Just (read v))
    go ("--var":v:rest) (p, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par) =
      case break (== '=') v of
        (tag, '=':val) -> go rest (p, px, sizes, n, vars ++ [(tag, read val)], maxGs, packOnly, txt, reps, warms, par)
        _ -> error ("invalid --var value: " ++ v ++ " (expected TAG=VALUE)")
    go (path:rest) (_, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par) =
      go rest (path, px, sizes, n, vars, maxGs, packOnly, txt, reps, warms, par)

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

printTimings :: String -> BuildTimings -> IO ()
printTimings label t = do
  printf "%s: render=%.2f ms, pack-place=%.2f ms, pack-image=%.2f ms, kerning=%.2f ms, marks=%.2f ms\n"
    label t.renderMs t.packPlaceMs t.packImageMs t.kerningMs t.marksMs
  case t.atlasSize of
    Nothing -> printf "%s: atlas size: none (rects=%d)\n" label t.packRectCount
    Just (w, h) -> printf "%s: atlas size: %dx%d (rects=%d)\n" label w h t.packRectCount

printPerGlyph :: String -> BuildTimings -> IO ()
printPerGlyph label t = do
  let glyphs = max 1 t.packRectCount
      msPerGlyphTotal = t.totalMs / fromIntegral glyphs
      msPerGlyphRender = t.renderMs / fromIntegral glyphs
  printf "%s: glyphs=%d total=%.2f ms/glyph render=%.2f ms/glyph\n"
    label glyphs msPerGlyphTotal msPerGlyphRender

forSize :: TTF -> GlyphSet -> [(String, Double)] -> Bool -> Int -> Int -> [Int] -> IO ()
forSize ttf glyphSet vars packOnly repeats warmups sizes = do
  printf "size  glyphs  total(ms)  render(ms)  ms/glyph(total)  ms/glyph(render)\n"
  forM_ sizes $ \px -> do
    let cfgBase = defaultMSDFConfig
          { MSDF.pixelSize = px
          , MSDF.glyphSet = glyphSet
          , MSDF.variations = vars
          }
        cfg = cfgBase { MSDF.atlas = cfgBase.atlas { MSDF.packAtlas = True } }
    t <- runTimings ("size " ++ show px) cfg ttf repeats warmups
    let glyphs = max 1 t.packRectCount
        msPerGlyphTotal = t.totalMs / fromIntegral glyphs
        msPerGlyphRender = t.renderMs / fromIntegral glyphs
    printf "%4d  %6d  %9.2f  %10.2f  %15.2f  %17.2f\n"
      px glyphs t.totalMs t.renderMs msPerGlyphTotal msPerGlyphRender

runTimings :: String -> MSDF.MSDFConfig -> TTF -> Int -> Int -> IO BuildTimings
runTimings label cfg ttf repeats warmups = do
  replicateM_ warmups (generateMSDFFromTTFWithTimings cfg ttf >> pure ())
  runs <- replicateM (max 1 repeats) (snd <$> generateMSDFFromTTFWithTimings cfg ttf)
  let avg = avgTimings runs
      totals = map (\t -> t.totalMs) runs
  if length runs > 1
    then printf "%s: runs=%d avg=%.2f ms min=%.2f ms max=%.2f ms\n"
           label (length runs) avg.totalMs (minimum totals) (maximum totals)
    else pure ()
  pure avg

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
