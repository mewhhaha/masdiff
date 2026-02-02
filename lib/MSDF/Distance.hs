module MSDF.Distance
  ( EdgeIndex(..)
  , buildEdgeIndex
  , minDistance
  , minDistanceSq
  , LineSeg
  , flattenEdges
  , windingNumber
  ) where

import Data.Array (Array, accumArray, array, bounds, (!))
import MSDF.Geometry (EdgeRef(..), Edge(..), Vec2, edgeBBox, edgeDistanceSq)
import MSDF.Outline (flattenEdge)
import MSDF.Types (BBox(..))

type LineSeg = (Vec2, Vec2)

data EdgeIndex = EdgeIndex
  { cellSize :: Double
  , originX :: Double
  , originY :: Double
  , gridW :: Int
  , gridH :: Int
  , cells :: Array Int [Int]
  , edges :: Array Int EdgeRef
  }

buildEdgeIndex :: Double -> BBox -> [EdgeRef] -> EdgeIndex
buildEdgeIndex cellSize' bb edgesList =
  let cellSize'' = max 1 cellSize'
      width = (max 1 (ceiling (bb.xMax - bb.xMin) + 1) :: Int)
      height = (max 1 (ceiling (bb.yMax - bb.yMin) + 1) :: Int)
      gridW' = max 1 (ceiling (fromIntegral width / cellSize''))
      gridH' = max 1 (ceiling (fromIntegral height / cellSize''))
      edgesArr =
        if null edgesList
        then array (0, -1) []
        else array (0, length edgesList - 1) (zip [0..] edgesList)
      pairs = concat
        [ [ (cellIndex gridW' x y, i) | x <- [x0 .. x1], y <- [y0 .. y1] ]
        | (i, e) <- zip [0..] edgesList
        , let bbE = edgeBBox e.edge
              x0 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMin)
              x1 = clamp 0 (gridW' - 1) (toCell bb.xMin cellSize'' bbE.xMax)
              y0 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMin)
              y1 = clamp 0 (gridH' - 1) (toCell bb.yMin cellSize'' bbE.yMax)
        ]
      cellsArr = accumArray (flip (:)) [] (0, gridW' * gridH' - 1) pairs
  in EdgeIndex
       { cellSize = cellSize''
       , originX = bb.xMin
       , originY = bb.yMin
       , gridW = gridW'
       , gridH = gridH'
       , cells = cellsArr
       , edges = edgesArr
       }

minDistance :: EdgeIndex -> Vec2 -> Double
minDistance idx p = sqrt (minDistanceSq idx p)

minDistanceSq :: EdgeIndex -> Vec2 -> Double
minDistanceSq idx p@(px', py') =
  let (elo, ehi) = bounds idx.edges
  in if elo > ehi
     then 1e18
     else
       let (cx, cy) = pointCell idx p
           maxR = max idx.gridW idx.gridH
           go r bestSq =
             let minX = max 0 (cx - r)
                 maxX = min (idx.gridW - 1) (cx + r)
                 minY = max 0 (cy - r)
                 maxY = min (idx.gridH - 1) (cy + r)
                 bestSq' = foldl' (scanCell idx p) bestSq
                              [ (x, y)
                              | y <- [minY .. maxY]
                              , x <- [minX .. maxX]
                              , r == 0 || x == minX || x == maxX || y == minY || y == maxY
                              ]
                 minBoundary = distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py')
                 fullGrid = minX == 0 && maxX == idx.gridW - 1 && minY == 0 && maxY == idx.gridH - 1
             in if bestSq' < 1e18 && bestSq' <= minBoundary * minBoundary
                then bestSq'
                else if fullGrid || r >= maxR
                     then bestSq'
                     else go (r + 1) bestSq'
       in go 0 1e18

scanCell :: EdgeIndex -> Vec2 -> Double -> (Int, Int) -> Double
scanCell idx p bestSq (x, y) =
  let cellIdx = cellIndex idx.gridW x y
      edgeIdxs = idx.cells ! cellIdx
  in foldl' (\acc i -> min acc (edgeDistanceSq p (idx.edges ! i).edge)) bestSq edgeIdxs

distanceToBoundary :: EdgeIndex -> (Int, Int) -> (Int, Int) -> Vec2 -> Double
distanceToBoundary idx (minX, minY) (maxX, maxY) (px', py') =
  let x0 = idx.originX + fromIntegral minX * idx.cellSize
      x1 = idx.originX + fromIntegral (maxX + 1) * idx.cellSize
      y0 = idx.originY + fromIntegral minY * idx.cellSize
      y1 = idx.originY + fromIntegral (maxY + 1) * idx.cellSize
      dx = min (px' - x0) (x1 - px')
      dy = min (py' - y0) (y1 - py')
  in max 0 (min dx dy)

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
