module Main (main) where

import Control.DeepSeq (NFData, deepseq)
import Control.Exception (evaluate)
import Data.List (sortOn)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import MSDF.Generated (generateMSDFFromTTF)
import MSDF.MSDF (GlyphSet(..), defaultMSDFConfig)
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (TTF(..), Cmap(..), ParseError(..), parseTTF)
import Paths_masdiff (getDataFileName)

main :: IO ()
main = do
  args <- getArgs
  (fontPath, pixelSize, glyphCount) <- parseArgs args
  fontPath' <- if null fontPath then getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf" else pure fontPath
  parsed <- parseTTF fontPath'
  ttf <- case parsed of
    Left (ParseError ctx msg) -> error (ctx ++ ": " ++ msg)
    Right val -> pure val
  let mappings = case ttf of
        TTF { cmap = Cmap ms } -> ms
      glyphSet = case glyphCount of
        Nothing -> GlyphSetAll
        Just n -> GlyphSetCodepoints (takeUnique n (map fst (sortOn fst mappings)))
      cfgBase = defaultMSDFConfig
        { MSDF.pixelSize = pixelSize
        , MSDF.glyphSet = glyphSet
        }
      cfgNoPack = cfgBase { MSDF.packAtlas = False }
      cfgPack = cfgBase { MSDF.packAtlas = True }
  tNoPack <- timePure "generate (no pack)" (generateMSDFFromTTF cfgNoPack ttf)
  tPack <- timePure "generate (pack)" (generateMSDFFromTTF cfgPack ttf)
  printf "packing delta (approx): %.2f ms\n" (tPack - tNoPack)

parseArgs :: [String] -> IO (FilePath, Int, Maybe Int)
parseArgs args = go args ("", 32, Nothing)
  where
    go [] acc = pure acc
    go ("--font":path:rest) (_, px, n) = go rest (path, px, n)
    go ("--pixel-size":v:rest) (p, _, n) = go rest (p, read v, n)
    go ("--glyphs":v:rest) (p, px, _) = go rest (p, px, Just (read v))
    go (path:rest) (_, px, n) = go rest (path, px, n)

timePure :: NFData a => String -> a -> IO Double
timePure label thunk = do
  start <- getCPUTime
  result <- evaluate thunk
  result `deepseq` pure ()
  end <- getCPUTime
  let elapsedMs = fromIntegral (end - start) / 1.0e9
  printf "%s: %.2f ms\n" label (elapsedMs :: Double)
  pure elapsedMs


takeUnique :: Int -> [Int] -> [Int]
takeUnique n xs = go n Nothing xs
  where
    go 0 _ _ = []
    go _ _ [] = []
    go k prev (y:ys) =
      case prev of
        Just p | p == y -> go k prev ys
        _ -> y : go (k - 1) (Just y) ys
