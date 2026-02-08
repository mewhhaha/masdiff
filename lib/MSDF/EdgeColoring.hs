module MSDF.EdgeColoring
  ( colorEdges
  ) where

import Data.List (sort)
import Data.Array (listArray, (!))
import Data.Bits ((.&.))
import MSDF.Config (EdgeColoringConfig(..), ColoringStrategy(..))
import MSDF.Distance (buildEdgeIndex, minDistanceSq)
import MSDF.Outline (flattenEdge)
import MSDF.Geometry
  ( Edge(..)
  , EdgeRef(..)
  , EdgeColor(..)
  , ColoredEdge(..)
  , edgeLengthApprox
  , edgeSamplePoints
  , bboxFromEdges
  )
import MSDF.Types (BBox(..))

colorEdges :: EdgeColoringConfig -> [[Edge]] -> [ColoredEdge]
colorEdges cfg contours =
  let colored = concatMap (colorContour cfg) (zip [0..] contours)
  in resolveConflicts cfg colored

colorContour :: EdgeColoringConfig -> (Int, [Edge]) -> [ColoredEdge]
colorContour cfg (contourId, edges) =
  case edges of
    [] -> []
    _ ->
      let n = length edges
          edgeArr = listArray (0, n - 1) edges
          edgeAt i = edgeArr ! i
          orient = signum (contourArea edges)
          joinCorners =
            [ isCornerFor cfg orient n edgeAt i
            | i <- [0 .. n - 1]
            ]
          cornerStarts = pickCornerStarts cfg n joinCorners
      in if null cornerStarts
         then
           [ ColoredEdge ColorWhite (EdgeRef contourId i e)
           | (i, e) <- zip [0..] edges
           ]
         else
           let starts = ensureMinSegments cfg.minSegments n edgeAt cornerStarts
               segments = buildSegments contourId n edgeAt starts
               colors = cycle (drop (cfg.coloringSeed `mod` 3) [ColorRed, ColorGreen, ColorBlue])
          in concat (zipWith colorSegment colors segments)

colorSegment :: EdgeColor -> [EdgeRef] -> [ColoredEdge]
colorSegment c seg = [ ColoredEdge c er | er <- seg ]

isCorner :: Edge -> Edge -> Double -> Bool
isCorner e1 e2 thresholdDeg =
  let (x1, y1) = edgeEndDir e1
      (x2, y2) = edgeStartDir e2
      dotp = x1 * x2 + y1 * y2
      len1 = sqrt (x1 * x1 + y1 * y1)
      len2 = sqrt (x2 * x2 + y2 * y2)
      threshold = thresholdDeg * pi / 180
  in if len1 == 0 || len2 == 0
     then False
     else
       let cosang = max (-1) (min 1 (dotp / (len1 * len2)))
           ang = acos cosang
       in ang > threshold

isCornerFor :: EdgeColoringConfig -> Double -> Int -> (Int -> Edge) -> Int -> Bool
isCornerFor cfg orient n edgeAt i =
  let e1 = edgeAt i
      e2 = edgeAt ((i + 1) `mod` n)
      base = cfg.cornerAngleDeg
      crossZ = edgeCross e1 e2
      concave = if orient >= 0 then crossZ < 0 else crossZ > 0
      threshold =
        case cfg.strategy of
          ColoringInktrap -> if concave then base * 0.5 else base
          _ -> base
  in isCorner e1 e2 threshold

edgeCross :: Edge -> Edge -> Double
edgeCross e1 e2 =
  let (x1, y1) = edgeEndDir e1
      (x2, y2) = edgeStartDir e2
  in x1 * y2 - y1 * x2

contourArea :: [Edge] -> Double
contourArea edges =
  case edges of
    [] -> 0
    _ ->
      let segs = concatMap (flattenEdge 0.5) edges
      in 0.5 * sum [ cross a b | (a, b) <- segs ]
  where
    cross (x0, y0) (x1, y1) = x0 * y1 - x1 * y0

edgeStartDir :: Edge -> (Double, Double)
edgeStartDir (EdgeLine (x0, y0) (x1, y1)) = (x1 - x0, y1 - y0)
edgeStartDir (EdgeQuad (x0, y0) (x1, y1) _ ) = (x1 - x0, y1 - y0)

edgeEndDir :: Edge -> (Double, Double)
edgeEndDir (EdgeLine (x0, y0) (x1, y1)) = (x1 - x0, y1 - y0)
edgeEndDir (EdgeQuad _ (x1, y1) (x2, y2)) = (x2 - x1, y2 - y1)

ensureMinSegments :: Int -> Int -> (Int -> Edge) -> [Int] -> [Int]
ensureMinSegments minSegs n edgeAt starts =
  let
      base = uniqueSorted starts
      anchor = case base of
        (a:_) -> a
        [] -> 0
      merged =
        if length base >= minSegs
        then base
        else uniqueSorted (base ++ splitByLengthFrom n edgeAt anchor minSegs)
  in if n == 0
     then []
     else if length merged >= minSegs
          then merged
          else uniqueSorted (base ++ splitByIndexFrom anchor n minSegs)

splitByLengthFrom :: Int -> (Int -> Edge) -> Int -> Int -> [Int]
splitByLengthFrom n edgeAt anchor segments =
  if n == 0 || segments <= 0
     then []
     else
       let idxs = indicesFrom anchor n
           lens = map (edgeLengthApprox . edgeAt) idxs
           total = sum lens
       in if total <= 1e-6
          then splitByIndexFrom anchor n segments
          else
            let step = total / fromIntegral segments
                thresholds = [ step * fromIntegral k | k <- [1 .. segments - 1] ]
                (hitsRev, _cum, _ts) = foldl' accumThresholds ([], 0.0, thresholds) (zip idxs lens)
            in uniqueSorted (anchor : reverse hitsRev)

accumThresholds :: ([Int], Double, [Double]) -> (Int, Double) -> ([Int], Double, [Double])
accumThresholds (acc, cum, ts) (idx, len) =
  let cum' = cum + len
      (hit, rest) = span (<= cum') ts
      acc' = replicate (length hit) idx ++ acc
  in (acc', cum', rest)

splitByIndexFrom :: Int -> Int -> Int -> [Int]
splitByIndexFrom anchor n segments =
  let step = max 1 (n `div` segments)
  in uniqueSorted
       [ (anchor + i * step) `mod` n
       | i <- [0 .. segments - 1]
       ]

indicesFrom :: Int -> Int -> [Int]
indicesFrom anchor n =
  take n (iterate (\i -> (i + 1) `mod` n) anchor)

pickCornerStarts :: EdgeColoringConfig -> Int -> [Bool] -> [Int]
pickCornerStarts cfg n corners =
  let starts = [ (i + 1) `mod` n | (i, True) <- zip [0 .. n - 1] corners ]
  in case cfg.strategy of
       ColoringDistance ->
         if length starts <= cfg.minSegments
         then starts
         else sampleCorners cfg.minSegments starts
       _ -> starts

sampleCorners :: Int -> [Int] -> [Int]
sampleCorners target starts =
  let total = length starts
  in if target <= 0 || total == 0
     then []
     else
       let step :: Double
           step = fromIntegral total / fromIntegral target
           startsArr = listArray (0, total - 1) starts
       in [ startsArr ! min (total - 1) (floor (step * fromIntegral i)) | i <- [0 .. target - 1] ]

uniqueSorted :: [Int] -> [Int]
uniqueSorted xs =
  case sort xs of
    [] -> []
    (y:ys) -> y : go y ys
  where
    go _ [] = []
    go prev (z:zs)
      | z == prev = go prev zs
      | otherwise = z : go z zs

buildSegments :: Int -> Int -> (Int -> Edge) -> [Int] -> [[EdgeRef]]
buildSegments contourId n edgeAt starts =
  let
      starts' = case starts of
        [] -> [0]
        _ -> starts
      nexts = rotate starts'
  in zipWith (segmentFrom contourId n edgeAt) starts' nexts

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

segmentFrom :: Int -> Int -> (Int -> Edge) -> Int -> Int -> [EdgeRef]
segmentFrom contourId n edgeAt start next =
  let count = if next > start then next - start else if next == start then n else n - start + next
  in [ let idx = (start + i) `mod` n
       in EdgeRef contourId idx (edgeAt idx)
     | i <- [0 .. count - 1]
     ]

resolveConflicts :: EdgeColoringConfig -> [ColoredEdge] -> [ColoredEdge]
resolveConflicts cfg colored =
  if cfg.conflictDistance <= 0 || length colored <= 1
  then colored
  else recolorPasses 2 colored
  where
    recolorPasses :: Int -> [ColoredEdge] -> [ColoredEdge]
    recolorPasses 0 edges = edges
    recolorPasses n edges =
      let edges' = recolorOnce cfg edges
      in if edges' == edges then edges else recolorPasses (n - 1) edges'

recolorOnce :: EdgeColoringConfig -> [ColoredEdge] -> [ColoredEdge]
recolorOnce cfg edges =
  let (edgesR, edgesG, edgesB) = splitByColor edges
      allEdges = map (\e -> e.edgeRef) edges
      bb = case bboxFromEdges allEdges of
        Just b -> b
        Nothing -> BBox 0 0 1 1
      idxR = buildEdgeIndex 1 bb edgesR
      idxG = buildEdgeIndex 1 bb edgesG
      idxB = buildEdgeIndex 1 bb edgesB
      conflictDistSq = cfg.conflictDistance * cfg.conflictDistance
      distTo idx er =
        minimum [ minDistanceSq idx p | p <- edgeSamplePoints er.edge ]
      pickColorFrom cur dR dG dB =
        let otherA = case cur of
              ColorRed -> Just (ColorGreen, dG, ColorBlue, dB)
              ColorGreen -> Just (ColorRed, dR, ColorBlue, dB)
              ColorBlue -> Just (ColorRed, dR, ColorGreen, dG)
              _ -> Nothing
        in case otherA of
             Just (c1, d1, c2, d2) -> if d1 < d2 then c1 else c2
             Nothing -> cur
      conflicted c dR dG dB =
        case c of
          ColorRed -> dG < conflictDistSq && dB < conflictDistSq
          ColorGreen -> dR < conflictDistSq && dB < conflictDistSq
          ColorBlue -> dR < conflictDistSq && dG < conflictDistSq
          _ -> False
  in
  [ let dR = distTo idxR er
        dG = distTo idxG er
        dB = distTo idxB er
        c' = if conflicted c dR dG dB then pickColorFrom c dR dG dB else c
    in ColoredEdge c' er
  | ColoredEdge c er <- edges
  ]

splitByColor :: [ColoredEdge] -> ([EdgeRef], [EdgeRef], [EdgeRef])
splitByColor edges =
  foldr step ([], [], []) edges
  where
    step e (rs, gs, bs) =
      let m = colorMask e.color
          er = e.edgeRef
          rs' = if m .&. 1 /= 0 then er : rs else rs
          gs' = if m .&. 2 /= 0 then er : gs else gs
          bs' = if m .&. 4 /= 0 then er : bs else bs
      in (rs', gs', bs')

colorMask :: EdgeColor -> Int
colorMask c =
  case c of
    ColorRed -> 1
    ColorGreen -> 2
    ColorBlue -> 4
    ColorYellow -> 3
    ColorMagenta -> 5
    ColorCyan -> 6
    ColorWhite -> 7
