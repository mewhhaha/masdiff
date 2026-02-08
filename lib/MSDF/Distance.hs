{-# LANGUAGE BangPatterns #-}

module MSDF.Distance
  ( EdgeIndex(..)
  , buildEdgeIndex
  , minDistance
  , minDistanceSq
  , LineSeg
  , flattenEdges
  , windingNumber
  ) where

import Control.Monad (forM_)
import Data.Array (Array, array)
import qualified Data.Array as A
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray, thaw)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Control.Monad.ST (ST)
import MSDF.Geometry (EdgeRef(..), Edge(..), Vec2, edgeBBox, edgeDistanceSq)
import MSDF.Outline (flattenEdge)
import MSDF.Types (BBox(..))

type LineSeg = (Vec2, Vec2)

data EdgeIndex = EdgeIndex
  { cellSize :: !Double
  , originX :: !Double
  , originY :: !Double
  , gridW :: !Int
  , gridH :: !Int
  , cellStarts :: !(UArray Int Int)
  , cellCounts :: !(UArray Int Int)
  , cellEdges :: !(UArray Int Int)
  , edges :: !(Array Int Edge)
  }

buildEdgeIndex :: Double -> BBox -> [EdgeRef] -> EdgeIndex
buildEdgeIndex cellSize' bb edgesList =
  let cellSize'' = max 1 cellSize'
      width = (max 1 (ceiling (bb.xMax - bb.xMin) + 1) :: Int)
      height = (max 1 (ceiling (bb.yMax - bb.yMin) + 1) :: Int)
      gridW' = max 1 (ceiling (fromIntegral width / cellSize''))
      gridH' = max 1 (ceiling (fromIntegral height / cellSize''))
      edgeList = [ e.edge | e <- edgesList ]
      edgesArr =
        if null edgeList
        then array (0, -1) []
        else array (0, length edgeList - 1) (zip [0..] edgeList)
      cellCount = gridW' * gridH'
      countsArr =
        if cellCount <= 0
        then UA.listArray (0, -1) []
        else runSTUArray $ do
          arr <- (newArray (0, cellCount - 1) 0 :: ST s (STUArray s Int Int))
          forM_ edgesList $ \e -> do
            let bbE = edgeBBox e.edge
                x0 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMin)
                x1 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMax)
                y0 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMin)
                y1 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMax)
            forM_ [y0 .. y1] $ \y ->
              forM_ [x0 .. x1] $ \x -> do
                let idx = cellIndex gridW' x y
                cur <- readArray arr idx
                writeArray arr idx (cur + 1)
          pure arr
      (startsArr, totalEdges) = buildCellStarts countsArr
      edgeIdxArr =
        if totalEdges <= 0
        then UA.listArray (0, -1) []
        else runSTUArray $ do
          arr <- (newArray (0, totalEdges - 1) 0 :: ST s (STUArray s Int Int))
          posArr <- (thaw startsArr :: ST s (STUArray s Int Int))
          forM_ (zip [0..] edgesList) $ \(i, e) -> do
            let bbE = edgeBBox e.edge
                x0 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMin)
                x1 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMax)
                y0 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMin)
                y1 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMax)
            forM_ [y0 .. y1] $ \y ->
              forM_ [x0 .. x1] $ \x -> do
                let idx = cellIndex gridW' x y
                pos <- readArray posArr idx
                writeArray arr pos i
                writeArray posArr idx (pos + 1)
          pure arr
  in EdgeIndex
       { cellSize = cellSize''
       , originX = bb.xMin
       , originY = bb.yMin
       , gridW = gridW'
       , gridH = gridH'
       , cellStarts = startsArr
       , cellCounts = countsArr
       , cellEdges = edgeIdxArr
       , edges = edgesArr
       }

buildCellStarts :: UArray Int Int -> (UArray Int Int, Int)
buildCellStarts counts =
  let (lo, hi) = UA.bounds counts
  in if lo > hi
     then (UA.listArray (0, -1) [], 0)
     else
       let starts = runSTUArray $ do
             arr <- (newArray (lo, hi) 0 :: ST s (STUArray s Int Int))
             let go i acc
                   | i > hi = pure ()
                   | otherwise = do
                       writeArray arr i acc
                       go (i + 1) (acc + counts UA.! i)
             go lo 0
             pure arr
           total = starts UA.! hi + counts UA.! hi
       in (starts, total)

minDistance :: EdgeIndex -> Vec2 -> Double
minDistance idx p = sqrt (minDistanceSq idx p)

minDistanceSq :: EdgeIndex -> Vec2 -> Double
minDistanceSq idx p@(px', py') =
  let (elo, ehi) = A.bounds idx.edges
  in if elo > ehi
     then 1e18
     else
       let (cx, cy) = pointCell idx p
           gridW' = idx.gridW
           gridH' = idx.gridH
           maxR = max gridW' gridH'
           cellSize' = idx.cellSize
           pxCell = (px' - idx.originX) / cellSize'
           pyCell = (py' - idx.originY) / cellSize'
           invCellSizeSq = 1 / (cellSize' * cellSize')
           go !r !bestSq =
             let minX = max 0 (cx - r)
                 maxX = min (gridW' - 1) (cx + r)
                 minY = max 0 (cy - r)
                 maxY = min (gridH' - 1) (cy + r)
                 bestSq' = scanRing r minX maxX minY maxY bestSq
                 minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py')
                 fullGrid = minX == 0 && maxX == gridW' - 1 && minY == 0 && maxY == gridH' - 1
             in if bestSq' < 1e18 && bestSq' <= minBoundary * minBoundary
                then bestSq'
                else if fullGrid || r >= maxR
                     then bestSq'
                     else go (r + 1) bestSq'
           scanRing !r !minX !maxX !minY !maxY !best0 =
             let ringBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py')
                 bestSq0 = best0
             in if bestSq0 <= ringBoundary * ringBoundary
                then best0
                else
                  if r == 0
                  then scanRow minY best0
                  else
                    let best1 = scanRow minY best0
                        best2 = if maxY /= minY then scanRow maxY best1 else best1
                        best3 = scanCol minX (minY + 1) (maxY - 1) best2
                        best4 = if maxX /= minX then scanCol maxX (minY + 1) (maxY - 1) best3 else best3
                    in best4
             where
               scanRow !y !best =
                 let fy = fromIntegral y
                     fy1 = fy + 1
                     dy
                       | pyCell < fy = fy - pyCell
                       | pyCell > fy1 = pyCell - fy1
                       | otherwise = 0
                     dy2 = dy * dy
                     goX !x !fx !bestAcc
                       | x > maxX = bestAcc
                       | otherwise =
                           let fx1 = fx + 1
                               dx
                                 | pxCell < fx = fx - pxCell
                                 | pxCell > fx1 = pxCell - fx1
                                 | otherwise = 0
                               !bestNorm = bestAcc * invCellSizeSq
                               cellSqNorm = dx * dx + dy2
                               !best' = if cellSqNorm >= bestNorm then bestAcc else scanCell idx p bestAcc x y
                           in goX (x + 1) fx1 best'
                 in goX minX (fromIntegral minX) best
               scanCol !x !y0 !y1 !best =
                 if y0 > y1
                 then best
                 else
                   let fx = fromIntegral x
                       fx1 = fx + 1
                       dx
                         | pxCell < fx = fx - pxCell
                         | pxCell > fx1 = pxCell - fx1
                         | otherwise = 0
                       dx2 = dx * dx
                       goY !y !fy !bestAcc
                         | y > y1 = bestAcc
                         | otherwise =
                             let fy1 = fy + 1
                                 dy
                                   | pyCell < fy = fy - pyCell
                                   | pyCell > fy1 = pyCell - fy1
                                   | otherwise = 0
                                 !bestNorm = bestAcc * invCellSizeSq
                                 cellSqNorm = dx2 + dy * dy
                                 !best' = if cellSqNorm >= bestNorm then bestAcc else scanCell idx p bestAcc x y
                             in goY (y + 1) fy1 best'
                   in goY y0 (fromIntegral y0) best
       in go 0 1e18

scanCell :: EdgeIndex -> Vec2 -> Double -> Int -> Int -> Double
scanCell idx p bestSq x y =
  let cellIdx = cellIndex idx.gridW x y
      start = idx.cellStarts UA.! cellIdx
      count = idx.cellCounts UA.! cellIdx
      go !acc !j
        | j >= count = acc
        | otherwise =
            let i = idx.cellEdges UA.! (start + j)
                !d = edgeDistanceSq p (idx.edges A.! i)
                !acc' = if d < acc then d else acc
            in go acc' (j + 1)
  in go bestSq 0

distanceToBoundary :: EdgeIndex -> (Int, Int) -> (Int, Int) -> Vec2 -> Double
distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py') =
  let x0 = idx.originX + fromIntegral minX * idx.cellSize
      x1 = idx.originX + fromIntegral (maxX + 1) * idx.cellSize
      y0 = idx.originY + fromIntegral minY * idx.cellSize
      y1 = idx.originY + fromIntegral (maxY + 1) * idx.cellSize
      dx = min (px' - x0) (x1 - px')
      dy = min (py' - y0) (y1 - py')
  in max 0 (min dx dy)
{-# INLINE distanceToBoundary #-}

pointCell :: EdgeIndex -> Vec2 -> (Int, Int)
pointCell idx (px', py') =
  let cx = clamp 0 (idx.gridW - 1) (toCell idx.originX idx.cellSize px')
      cy = clamp 0 (idx.gridH - 1) (toCell idx.originY idx.cellSize py')
  in (cx, cy)

toCell :: Double -> Double -> Double -> Int
toCell origin size v =
  floor ((v - origin) / size)

cellIndex :: Int -> Int -> Int -> Int
cellIndex gridW' x y = y * gridW' + x

clamp :: Int -> Int -> Int -> Int
clamp lo hi v = max lo (min hi v)

flattenEdges :: Double -> [Edge] -> [LineSeg]
flattenEdges eps edges = concatMap (flattenEdge eps) edges

windingNumber :: [LineSeg] -> Vec2 -> Int
windingNumber segs (px', py') =
  foldl' step 0 segs
  where
    step wn ((x0, y0), (x1, y1))
      | y0 <= py' =
          if y1 > py' && isLeft (x0, y0) (x1, y1) (px', py') > 0
          then wn + 1
          else wn
      | y1 <= py' =
          if isLeft (x0, y0) (x1, y1) (px', py') < 0
          then wn - 1
          else wn
      | otherwise = wn

isLeft :: Vec2 -> Vec2 -> Vec2 -> Double
isLeft (x0, y0) (x1, y1) (px', py') =
  (x1 - x0) * (py' - y0) - (px' - x0) * (y1 - y0)
