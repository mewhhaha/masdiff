{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (when)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (numCapabilities)
import MSDF.Compare (DiffStats (..), diffRGBA8)
import MSDF.Generate (defaultRuntimeCfg, generateGlyphBatchIO, generateGlyphIO)
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenOut (..),
    GlyphCode,
    ImgRGBA8 (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkImgRGBA8,
    mkPxRange,
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  diffIters <- envInt "BENCH_DIFF_ITERS" 400
  genIters <- envInt "BENCH_GEN_ITERS" 12
  batchIters <- envInt "BENCH_BATCH_ITERS" 6
  batchJobs <- envInt "BENCH_BATCH_JOBS" (max 1 numCapabilities)
  warmupIters <- envInt "BENCH_WARMUP_ITERS" 1
  putStrLn ("diff iters: " <> show diffIters)
  putStrLn ("generate iters: " <> show genIters)
  putStrLn ("batch iters: " <> show batchIters)
  putStrLn ("batch jobs: " <> show batchJobs)
  putStrLn ("warmup iters: " <> show warmupIters)

  let leftImg = fixedImage64x64 73 19
  let rightImg = fixedImage64x64 31 101
  diffMs <- timeLoop diffIters (consumeDiffStats leftImg rightImg)
  putStrLn ("diffRGBA8 total ms: " <> show diffMs)
  putStrLn ("diffRGBA8 avg ms: " <> show (diffMs / fromIntegral diffIters))

  let cfg = benchmarkCfg
  let glyph = benchmarkGlyphA
  let glyphBatch = benchmarkGlyphBatch
  let staticFont = FontFile {path = "assets/Inter/static/Inter_24pt-Regular.ttf"}
  let variableFont =
        VarFontFile
          { path = "assets/Inter/Inter-VariableFont_opsz,wght.ttf",
            axes =
              Map.fromList
                [ (AxisTag (T.pack "opsz"), AxisVal 14.0),
                  (AxisTag (T.pack "wght"), AxisVal 400.0)
                ]
          }
  runGenerateCase warmupIters genIters cfg staticFont glyph "generateGlyphIO/static"
  runGenerateCase warmupIters genIters cfg variableFont glyph "generateGlyphIO/variable"
  runGenerateBatchCase warmupIters batchIters 1 cfg staticFont glyphBatch "generateGlyphBatchIO/static/jobs1"
  when
    (batchJobs > 1)
    ( runGenerateBatchCase
        warmupIters
        batchIters
        batchJobs
        cfg
        staticFont
        glyphBatch
        ("generateGlyphBatchIO/static/jobs" <> show batchJobs)
    )

runGenerateCase :: Int -> Int -> GenCfg -> FontSrc -> GlyphCode -> String -> IO ()
runGenerateCase warmupIters genIters cfg src glyph label = do
  _ <- timeLoop warmupIters (benchmarkGenerateGlyphIO cfg src glyph)
  genMs <- timeLoop genIters (benchmarkGenerateGlyphIO cfg src glyph)
  putStrLn (label <> " total ms: " <> show genMs)
  putStrLn (label <> " avg ms: " <> show (genMs / fromIntegral genIters))

timeLoop :: Int -> IO Int -> IO Double
timeLoop iters action = do
  t0 <- getPOSIXTime
  loop iters
  t1 <- getPOSIXTime
  pure (realToFrac ((t1 - t0) * 1000))
  where
    loop n
      | n <= 0 = pure ()
      | otherwise = action >> loop (n - 1)

fixedImage64x64 :: Int -> Int -> ImgRGBA8
fixedImage64x64 mul add = must "fixedImage64x64" (mkImgRGBA8 64 64 payload)
  where
    pxCount = 64 * 64 * 4
    payload = BS.pack [fromIntegral ((i * mul + add) `mod` 256) | i <- [0 .. pxCount - 1]]

consumeDiffStats :: ImgRGBA8 -> ImgRGBA8 -> IO Int
consumeDiffStats leftImg rightImg =
  evaluate $
    case diffRGBA8 leftImg rightImg of
      Left _ -> -1
      Right stats ->
        let (maxR, maxG, maxB, maxA) = stats.maxCh
         in stats.pxCount
              + stats.chCount
              + stats.maxAbs
              + maxR
              + maxG
              + maxB
              + maxA
              + stats.p99Abs
              + round (stats.meanAbs * 1000)
              + stats.mismatch

benchmarkGenerateGlyphIO :: GenCfg -> FontSrc -> GlyphCode -> IO Int
benchmarkGenerateGlyphIO cfg src glyph = do
  result <- generateGlyphIO defaultRuntimeCfg cfg src glyph
  case result of
    Left err -> error ("generateGlyphIO benchmark failed: " <> show err)
    Right out -> evaluate (BS.length out.img.px)

runGenerateBatchCase :: Int -> Int -> Int -> GenCfg -> FontSrc -> [GlyphCode] -> String -> IO ()
runGenerateBatchCase warmupIters genIters jobs cfg src glyphs label = do
  _ <- timeLoop warmupIters (benchmarkGenerateGlyphBatchIO jobs cfg src glyphs)
  genMs <- timeLoop genIters (benchmarkGenerateGlyphBatchIO jobs cfg src glyphs)
  putStrLn (label <> " total ms: " <> show genMs)
  putStrLn (label <> " avg ms: " <> show (genMs / fromIntegral genIters))

benchmarkGenerateGlyphBatchIO :: Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO Int
benchmarkGenerateGlyphBatchIO jobs cfg src glyphs = do
  results <- generateGlyphBatchIO defaultRuntimeCfg jobs cfg src glyphs
  case sequence results of
    Left err -> error ("generateGlyphBatchIO benchmark failed: " <> show err)
    Right outs -> evaluate (sum (fmap (BS.length . (.px) . (.img)) outs))

benchmarkCfg :: GenCfg
benchmarkCfg =
  GenCfg
    { mode = Mtsdf,
      dim = must "mkDim 64" (mkDim 64),
      pxr = must "mkPxRange 8.0" (mkPxRange 8.0),
      seed = 1,
      autoframe = False,
      ovlp = False
    }

benchmarkGlyphA :: GlyphCode
benchmarkGlyphA = must "mkGlyphCode A" (mkGlyphCode (ord 'A'))

benchmarkGlyphBatch :: [GlyphCode]
benchmarkGlyphBatch = fmap mk "PACKMYBOXWITHFIVEDOZENLIQUORJUGS"
  where
    mk ch = must ("mkGlyphCode " <> [ch]) (mkGlyphCode (ord ch))

envInt :: String -> Int -> IO Int
envInt name fallback = do
  raw <- lookupEnv name
  pure $
    case raw >>= readMaybe of
      Just x | x > 0 -> x
      _ -> fallback

must :: String -> Either String a -> a
must label result =
  case result of
    Right x -> x
    Left err -> error (label <> " failed: " <> err)
