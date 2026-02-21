{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Compare
  ( DiffGate (..),
    DiffStats (..),
    diffRGBA8,
    passesGate,
    strictGate,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Word (Word8)
import MSDF.Types (ImgRGBA8 (..))

data DiffStats = DiffStats
  { pxCount :: Int,
    chCount :: Int,
    maxAbs :: Int,
    maxCh :: (Int, Int, Int, Int),
    p99Abs :: Int,
    meanAbs :: Double,
    mismatch :: Int
  }
  deriving stock (Eq, Show)

data DiffGate = DiffGate
  { maxChLimit :: Int,
    p99Limit :: Int,
    meanLimit :: Double
  }
  deriving stock (Eq, Show)

strictGate :: DiffGate
strictGate =
  DiffGate
    { maxChLimit = 2,
      p99Limit = 1,
      meanLimit = 0.20
    }

passesGate :: DiffGate -> DiffStats -> Bool
passesGate gate stats =
  all (<= gate.maxChLimit) [mr, mg, mb, ma]
    && stats.p99Abs <= gate.p99Limit
    && stats.meanAbs <= gate.meanLimit
  where
    (mr, mg, mb, ma) = stats.maxCh

diffRGBA8 :: ImgRGBA8 -> ImgRGBA8 -> Either String DiffStats
diffRGBA8 a b
  | a.w /= b.w || a.h /= b.h =
      Left $
        "Image dimensions differ. left="
          <> show (a.w, a.h)
          <> " right="
          <> show (b.w, b.h)
  | BS.length a.px /= BS.length b.px =
      Left $
        "Image payload lengths differ. left="
          <> show (BS.length a.px)
          <> " right="
          <> show (BS.length b.px)
  | otherwise = Right (go a.px b.px)

go :: ByteString -> ByteString -> DiffStats
go left right =
  DiffStats
    { pxCount = length diffs `div` 4,
      chCount = n,
      maxAbs = maxAll,
      maxCh = (maxR, maxG, maxB, maxA),
      p99Abs = p99,
      meanAbs = mean,
      mismatch = mismatchCount
    }
  where
    diffs = zipWith absDiff (BS.unpack left) (BS.unpack right)
    n = length diffs
    sorted = sort diffs
    maxAll = if null diffs then 0 else last sorted
    p99Idx = max 0 (ceiling (0.99 * fromIntegral n :: Double) - 1)
    p99 =
      if null diffs
        then 0
        else sorted !! min p99Idx (n - 1)
    mean =
      if n == 0
        then 0
        else fromIntegral (sum diffs) / fromIntegral n
    mismatchCount = length (filter (> 0) diffs)
    (maxR, maxG, maxB, maxA) = maxByChannel diffs

maxByChannel :: [Int] -> (Int, Int, Int, Int)
maxByChannel diffs = foldl' step (0, 0, 0, 0) (chunks4 diffs)
  where
    step (rMax, gMax, bMax, aMax) [r, g, b, a] =
      (max rMax r, max gMax g, max bMax b, max aMax a)
    step acc _ = acc

chunks4 :: [a] -> [[a]]
chunks4 [] = []
chunks4 xs =
  let (front, back) = splitAt 4 xs
   in front : chunks4 back

absDiff :: Word8 -> Word8 -> Int
absDiff x y = abs (fromIntegral x - fromIntegral y)
