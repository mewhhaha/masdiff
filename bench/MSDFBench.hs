module Main (main) where

import Data.Array (array, bounds, (!))
import Data.List (sortOn)
import System.Environment (getArgs)
import Text.Printf (printf)

import MSDF.Generated (generateMSDFFromTTFWithTimings, BuildTimings(..))
import MSDF.MSDF (GlyphSet(..), defaultMSDFConfig)
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (TTF(..), Cmap(..), Maxp(..), Loca(..), ParseError(..), parseTTF)
import Paths_masdiff (getDataFileName)

main :: IO ()
main = do
  args <- getArgs
  (fontPath, pixelSize, glyphCount, vars, maxGlyphs, packOnly) <- parseArgs args
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
      glyphSet = case glyphCount of
        Nothing -> GlyphSetAll
        Just n -> GlyphSetCodepoints (takeUnique n (map fst (sortOn fst mappings)))
      cfgBase = defaultMSDFConfig
        { MSDF.pixelSize = pixelSize
        , MSDF.glyphSet = glyphSet
        , MSDF.variations = vars
        }
      cfgNoPack = cfgBase { MSDF.atlas = cfgBase.atlas { MSDF.packAtlas = False } }
      cfgPack = cfgBase { MSDF.atlas = cfgBase.atlas { MSDF.packAtlas = True } }
  timingsNoPack <-
    if packOnly
    then pure Nothing
    else do
      (_, t) <- generateMSDFFromTTFWithTimings cfgNoPack ttf'
      pure (Just t)
  (_, timingsPack) <- generateMSDFFromTTFWithTimings cfgPack ttf'
  case timingsNoPack of
    Nothing -> pure ()
    Just t -> do
      printf "generate (no pack): %.2f ms\n" t.totalMs
      printTimings "no pack breakdown" t
  printf "generate (pack): %.2f ms\n" timingsPack.totalMs
  printTimings "pack breakdown" timingsPack
  case timingsNoPack of
    Nothing -> pure ()
    Just t -> printf "packing delta (approx): %.2f ms\n" (timingsPack.totalMs - t.totalMs)

parseArgs :: [String] -> IO (FilePath, Int, Maybe Int, [(String, Double)], Maybe Int, Bool)
parseArgs args = go args ("", 32, Nothing, [], Nothing, False)
  where
    go [] acc = pure acc
    go ("--font":path:rest) (_, px, n, vars, maxGs, packOnly) = go rest (path, px, n, vars, maxGs, packOnly)
    go ("--pixel-size":v:rest) (p, _, n, vars, maxGs, packOnly) = go rest (p, read v, n, vars, maxGs, packOnly)
    go ("--glyphs":v:rest) (p, px, _, vars, maxGs, packOnly) = go rest (p, px, Just (read v), vars, maxGs, packOnly)
    go ("--max-glyphs":v:rest) (p, px, n, vars, _, packOnly) = go rest (p, px, n, vars, Just (read v), packOnly)
    go ("--pack-only":rest) (p, px, n, vars, maxGs, _) = go rest (p, px, n, vars, maxGs, True)
    go ("--var":v:rest) (p, px, n, vars, maxGs, packOnly) =
      case break (== '=') v of
        (tag, '=':val) -> go rest (p, px, n, vars ++ [(tag, read val)], maxGs, packOnly)
        _ -> error ("invalid --var value: " ++ v ++ " (expected TAG=VALUE)")
    go (path:rest) (_, px, n, vars, maxGs, packOnly) = go rest (path, px, n, vars, maxGs, packOnly)

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


takeUnique :: Int -> [Int] -> [Int]
takeUnique n xs = go n Nothing xs
  where
    go 0 _ _ = []
    go _ _ [] = []
    go k prev (y:ys) =
      case prev of
        Just p | p == y -> go k prev ys
        _ -> y : go (k - 1) (Just y) ys
