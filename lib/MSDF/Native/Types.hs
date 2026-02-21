{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native.Types
  ( Channel (..),
    Contour (..),
    Edge (..),
    Outline (..),
    Pt (..),
    buildEdgeContours,
    buildEdges,
    contoursFromPointLists,
    contoursFromPointListsWithGroups,
  )
where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.List (sortOn)
import Data.Word (Word64, Word8)

data Pt = Pt
  { x :: !Double,
    y :: !Double
  }
  deriving stock (Eq, Show)

data Contour = Contour
  { pts :: [Pt],
    segs :: [Edge],
    grps :: [Int]
  }
  deriving stock (Eq, Show)

data Outline = Outline
  { adv :: !Double,
    bounds :: !(Double, Double, Double, Double),
    unitsPerEm :: !Double,
    contours :: [Contour]
  }
  deriving stock (Eq, Show)

data Channel = ChR | ChG | ChB
  deriving stock (Eq, Show, Enum, Bounded)

data Edge = Edge
  { a :: !Pt,
    b :: !Pt,
    c :: !(Maybe Pt),
    col :: !Word8
  }
  deriving stock (Eq, Show)

contoursFromPointLists :: [[Pt]] -> Either String [Contour]
contoursFromPointLists = traverse mkContour
  where
    mkContour rawPoints = do
      let points = dropDuplicateClose (dropAdjacentDuplicateClose rawPoints)
      if length points < 2
        then Left "Contour must contain at least two distinct points."
        else
          let edgeCount = contourEdgeCount points
              segments = lineEdgesFromPoints points
           in Right
                Contour
                  { pts = points,
                    segs = segments,
                    grps = replicate edgeCount 1
                  }

contoursFromPointListsWithGroups :: [[Pt]] -> [[Int]] -> Either String [Contour]
contoursFromPointListsWithGroups pointLists groupLists =
  if length pointLists /= length groupLists
    then Left "Contour/group list count mismatch."
    else traverse mkContour (zip pointLists groupLists)
  where
    mkContour (points, groups)
      | length points < 2 =
          Left "Contour must contain at least two distinct points."
      | hasAdjacentDuplicate points =
          Left "Contour contains adjacent duplicate points."
      | contourHasDuplicateClose points =
          Left "Contour unexpectedly repeats the start point at the end."
      | not (validGroupsForEdgeCount (contourEdgeCount points) groups) =
          Left "Contour groups are invalid for the contour edge count."
      | otherwise =
          Right
            Contour
              { pts = points,
                segs = lineEdgesFromPoints points,
                grps = groups
              }

buildEdgeContours :: Int -> [Contour] -> [[Edge]]
buildEdgeContours seedInput contours = reverse (snd (go st0 contours []))
  where
    (seed0, col0) = initColor (fromIntegral (max 0 seedInput))
    st0 = ColorState {seed = seed0, cur = col0}

    go st [] acc = (st, acc)
    go st (contour : rest) acc =
      let rawSegments = contourSegments contour
          segments = normalizeContourSegments rawSegments
          groups
            | length rawSegments /= length segments = replicate (length segments) 1
            | otherwise = normalizeGroups (length segments) contour.grps
          (st', colored) = colorizeContour st segments groups
       in go st' rest (colored : acc)

buildEdges :: Int -> [Contour] -> [Edge]
buildEdges seedInput contours = concat (buildEdgeContours seedInput contours)

data ColorState = ColorState
  { seed :: !Word64,
    cur :: !Word8
  }
  deriving stock (Eq, Show)

colorizeContour :: ColorState -> [Edge] -> [Int] -> (ColorState, [Edge])
colorizeContour st segments groups
  | null segments = (st, [])
  | otherwise =
      case splitByLengthsMaybe groups segments of
        Nothing -> colorizeContourSegments st segments
        Just groupedSegments ->
          case contourCornersForGroups groupedSegments of
            [] ->
              let (color, seed') = switchColor st.cur st.seed
                  st' = st {seed = seed', cur = color}
                  colored = applyColors segments (replicate (length segments) color)
               in (st', colored)
            [corner]
              | length groupedSegments >= 3 ->
                  let (c0, seed1) = switchColor st.cur st.seed
                      c1 = colorWhite
                      (c2, seed2) = switchColor c0 seed1
                      st' = st {seed = seed2, cur = c2}
                      groupCount = length groupedSegments
                      groupColors =
                        [ colorForStep groupCount c0 c1 c2 ((ix - corner + groupCount) `mod` groupCount)
                          | ix <- [0 .. groupCount - 1]
                        ]
                      colors = expandGroupColors groups groupColors
                   in (st', applyColors segments colors)
              | otherwise ->
                  colorizeContourSegments st segments
            start : remainingCorners ->
              let groupCount = length groupedSegments
                  (initial, seed1) = switchColor st.cur st.seed
                  (colorFinal, seedFinal, assignments) =
                    colorizeManyCorners groupCount start remainingCorners initial seed1
                  st' = st {seed = seedFinal, cur = colorFinal}
                  groupColors = colorsFromAssignments groupCount colorFinal assignments
                  colors = expandGroupColors groups groupColors
               in (st', applyColors segments colors)

colorizeContourSegments :: ColorState -> [Edge] -> (ColorState, [Edge])
colorizeContourSegments st segments =
  case segments of
    [] -> (st, [])
    _ ->
      case corners of
        [] ->
          let (color, seed') = switchColor st.cur st.seed
              st' = st {seed = seed', cur = color}
              colors = replicate edgeCount color
           in (st', applyColors segments colors)
        [corner] ->
          let (c0, seed1) = switchColor st.cur st.seed
              c1 = colorWhite
              (c2, seed2) = switchColor c0 seed1
              st' = st {seed = seed2, cur = c2}
           in case edgeCount of
                n
                  | n >= 3 ->
                      let colors =
                            [ colorForStep edgeCount c0 c1 c2 ((ix - corner + edgeCount) `mod` edgeCount)
                              | ix <- [0 .. edgeCount - 1]
                            ]
                       in (st', applyColors segments colors)
                _ ->
                  (st', colorizeTeardropSmall corner c0 c1 c2 segments)
        start : remainingCorners ->
          let (initial, seed1) = switchColor st.cur st.seed
              (colorFinal, seedFinal, assignments) =
                colorizeManyCorners edgeCount start remainingCorners initial seed1
              st' = st {seed = seedFinal, cur = colorFinal}
              colors = colorsFromAssignments edgeCount colorFinal assignments
           in (st', applyColors segments colors)
  where
    edgeCount = length segments
    corners = contourCorners segments

colorizeTeardropSmall :: Int -> Word8 -> Word8 -> Word8 -> [Edge] -> [Edge]
colorizeTeardropSmall corner c0 c1 c2 segments =
  case segments of
    [edge0] ->
      zipWith setEdgeColor [c0, c1, c2] (splitEdgeInThirds edge0)
    [edge0, edge1] ->
      let edge0Parts = splitEdgeInThirds edge0
          edge1Parts = splitEdgeInThirds edge1
          orderedParts =
            if corner == 0
              then edge0Parts <> edge1Parts
              else edge1Parts <> edge0Parts
       in zipWith setEdgeColor [c0, c0, c1, c1, c2, c2] orderedParts
    _ -> applyColors segments (replicate (length segments) c1)

normalizeContourSegments :: [Edge] -> [Edge]
normalizeContourSegments segments =
  deconvergeContourSegments normalized
  where
    normalized =
      case segments of
        [edge] -> splitEdgeInThirds edge
        _ -> segments

deconvergeContourSegments :: [Edge] -> [Edge]
deconvergeContourSegments segments =
  case segments of
    [] -> []
    [_] -> segments
    _ -> foldl' deconvergeAt segments [0 .. edgeCount - 1]
  where
    edgeCount = length segments

    deconvergeAt current edgeIx =
      let prevIx = (edgeIx + edgeCount - 1) `mod` edgeCount
       in case (edgeAt prevIx current, edgeAt edgeIx current) of
            (Just prevEdge, Just edge) ->
              case deconvergeAdjacentEdges prevEdge edge of
                Nothing -> current
                Just (prevEdge', edge') ->
                  setEdgeAt edgeIx edge' (setEdgeAt prevIx prevEdge' current)
            _ -> current

deconvergeAdjacentEdges :: Edge -> Edge -> Maybe (Edge, Edge)
deconvergeAdjacentEdges prevEdge edge
  | dotProduct >= cornerDotEps - 1.0 =
      Nothing
  | otherwise =
      let axisBase = normalizeDirectionVec (subVec curDir prevDir)
          axis0 = scaleVec deconvergeFactor axisBase
          axis =
            if convergentOrdering prevEdge edge < 0
              then negateVec axis0
              else axis0
          prevAdjust = orthogonalVec axis True
          edgeAdjust = orthogonalVec axis False
       in Just
            (deconvergeEdgeQuadratic 1 prevAdjust prevEdge, deconvergeEdgeQuadratic 0 edgeAdjust edge)
  where
    prevDir = edgeEndDirectionUnit prevEdge
    curDir = edgeStartDirectionUnit edge
    dotProduct = dotVec prevDir curDir

deconvergeEdgeQuadratic :: Int -> (Double, Double) -> Edge -> Edge
deconvergeEdgeQuadratic param vector edge =
  case edge.c of
    Nothing -> edge
    Just ctrl ->
      let anchor =
            case param of
              0 -> edge.a
              _ -> edge.b
          ctrlOffset = scaleVec (distancePt ctrl anchor) vector
       in edge {c = Just (addVecToPt ctrl ctrlOffset)}

convergentOrdering :: Edge -> Edge -> Int
convergentOrdering prevEdge edge =
  case (simplifyCurve (curveFromEdge prevEdge), simplifyCurve (curveFromEdge edge)) of
    (Curve beforeOrder beforeP0 beforeP1 beforeP2, Curve afterOrder _ afterP1 afterP2)
      | not (beforeOrder > 0 && afterOrder > 0) -> 0
      | not (pointsClose prevEdge.b edge.a) -> 0
      | otherwise ->
          let corner = edge.a
              beforeLeadPoint =
                if beforeOrder >= 2
                  then beforeP1
                  else beforeP0
              a1raw = subPt beforeLeadPoint corner
              b1raw = subPt afterP1 corner
              a2 =
                if beforeOrder >= 2
                  then case beforeP2 of
                    Just _ -> subVec (subPt beforeP0 beforeP1) a1raw
                    Nothing -> zeroVec
                  else zeroVec
              b2 =
                if afterOrder >= 2
                  then case afterP2 of
                    Just afterP2' -> subVec (subPt afterP2' afterP1) b1raw
                    Nothing -> zeroVec
                  else zeroVec
              a1 = scaleVec (fromIntegral beforeOrder) a1raw
              b1 = scaleVec (fromIntegral afterOrder) b1raw
           in convergentOrderingFromTerms a1 a2 b1 b2
convergentOrderingFromTerms ::
  (Double, Double) ->
  (Double, Double) ->
  (Double, Double) ->
  (Double, Double) ->
  Int
convergentOrderingFromTerms a1 a2 b1 b2
  | vecNonZero a1 && vecNonZero b1 =
      let as = lengthVec a1
          bs = lengthVec b1
          thirdDerivative = (as * crossVec a1 b2) + (bs * crossVec a2 b1)
       in case signOf thirdDerivative of
            0 ->
              let fourthDerivative = as * bs * crossVec a2 b2
               in signOf fourthDerivative
            signValue -> signValue
  | otherwise =
      let (a2', b1', b2', directionSign)
            | vecNonZero a1 =
                (b2, a1, a2, -1)
            | otherwise =
                (a2, b1, b2, 1)
          diagonal =
            (sqrt (lengthVec a2') * crossVec a2' b1')
              + (sqrt (lengthVec b2') * crossVec a2' b2')
       in if diagonal /= 0
            then directionSign * signOf diagonal
            else directionSign * signOf (crossVec a2' b2')

data Curve = Curve
  { order :: !Int,
    p0 :: !Pt,
    p1 :: !Pt,
    p2 :: !(Maybe Pt)
  }

curveFromEdge :: Edge -> Curve
curveFromEdge edge =
  case edge.c of
    Nothing ->
      Curve
        { order = 1,
          p0 = edge.a,
          p1 = edge.b,
          p2 = Nothing
        }
    Just ctrl ->
      Curve
        { order = 2,
          p0 = edge.a,
          p1 = ctrl,
          p2 = Just edge.b
        }

simplifyCurve :: Curve -> Curve
simplifyCurve curve =
  case curve.order of
    2 ->
      case curve.p2 of
        Nothing -> curve
        Just endPt ->
          let (order', p1') =
                if pointsClose curve.p1 curve.p0 || pointsClose curve.p1 endPt
                  then (1, endPt)
                  else (2, curve.p1)
              order'' =
                if order' == 1 && pointsClose curve.p0 p1'
                  then 0
                  else order'
           in curve {order = order'', p1 = p1'}
    1 ->
      if pointsClose curve.p0 curve.p1
        then curve {order = 0}
        else curve
    _ -> curve {order = 0}

edgeAt :: Int -> [a] -> Maybe a
edgeAt index values
  | index < 0 = Nothing
  | otherwise =
      case drop index values of
        value : _ -> Just value
        [] -> Nothing

setEdgeAt :: Int -> a -> [a] -> [a]
setEdgeAt index replacement values
  | index < 0 = values
  | otherwise = go index values
  where
    go _ [] = []
    go 0 (_ : rest) = replacement : rest
    go n (value : rest) = value : go (n - 1) rest

setEdgeColor :: Word8 -> Edge -> Edge
setEdgeColor edgeColor edge = edge {col = edgeColor}

splitEdgeInThirds :: Edge -> [Edge]
splitEdgeInThirds edge =
  let (first, tailEdge) = splitEdgeAt (1.0 / 3.0) edge
      (second, third) = splitEdgeAt 0.5 tailEdge
   in [first, second, third]

splitEdgeAt :: Double -> Edge -> (Edge, Edge)
splitEdgeAt t edge =
  case edge.c of
    Nothing ->
      let splitPoint = lerpPt edge.a edge.b t
          left = edge {b = splitPoint}
          right = edge {a = splitPoint}
       in (left, right)
    Just ctrl ->
      let q0 = lerpPt edge.a ctrl t
          q1 = lerpPt ctrl edge.b t
          splitPoint = lerpPt q0 q1 t
          left =
            edge
              { b = splitPoint,
                c = Just q0
              }
          right =
            edge
              { a = splitPoint,
                c = Just q1
              }
       in (left, right)

lerpPt :: Pt -> Pt -> Double -> Pt
lerpPt p0 p1 t =
  Pt
    { x = p0.x + ((p1.x - p0.x) * t),
      y = p0.y + ((p1.y - p0.y) * t)
    }

cornerDotEps :: Double
cornerDotEps = 1.0e-6

deconvergeOvershoot :: Double
deconvergeOvershoot = 1.11111111111111111

deconvergeFactor :: Double
deconvergeFactor =
  let cornerDot = cornerDotEps - 1.0
   in deconvergeOvershoot * sqrt (1.0 - (cornerDot * cornerDot)) / cornerDot

zeroVec :: (Double, Double)
zeroVec = (0.0, 0.0)

subPt :: Pt -> Pt -> (Double, Double)
subPt p q = (p.x - q.x, p.y - q.y)

subVec :: (Double, Double) -> (Double, Double) -> (Double, Double)
subVec (ax, ay) (bx, by) = (ax - bx, ay - by)

scaleVec :: Double -> (Double, Double) -> (Double, Double)
scaleVec scale (vx, vy) = (scale * vx, scale * vy)

negateVec :: (Double, Double) -> (Double, Double)
negateVec (vx, vy) = (-vx, -vy)

dotVec :: (Double, Double) -> (Double, Double) -> Double
dotVec (ax, ay) (bx, by) = (ax * bx) + (ay * by)

crossVec :: (Double, Double) -> (Double, Double) -> Double
crossVec (ax, ay) (bx, by) = (ax * by) - (ay * bx)

lengthVec :: (Double, Double) -> Double
lengthVec (vx, vy) = sqrt ((vx * vx) + (vy * vy))

vecNonZero :: (Double, Double) -> Bool
vecNonZero (vx, vy) = vx /= 0.0 || vy /= 0.0

normalizeDirectionVec :: (Double, Double) -> (Double, Double)
normalizeDirectionVec (vx, vy) = normalizeDirection vx vy

orthogonalVec :: (Double, Double) -> Bool -> (Double, Double)
orthogonalVec (vx, vy) polarity
  | polarity = (-vy, vx)
  | otherwise = (vy, -vx)

addVecToPt :: Pt -> (Double, Double) -> Pt
addVecToPt p (vx, vy) = Pt {x = p.x + vx, y = p.y + vy}

distancePt :: Pt -> Pt -> Double
distancePt p q =
  let dx = p.x - q.x
      dy = p.y - q.y
   in sqrt ((dx * dx) + (dy * dy))

signOf :: Double -> Int
signOf value
  | value > 0.0 = 1
  | value < 0.0 = -1
  | otherwise = 0

colorizeManyCorners ::
  Int ->
  Int ->
  [Int] ->
  Word8 ->
  Word64 ->
  (Word8, Word64, [(Int, Word8)])
colorizeManyCorners edgeCount start pendingCorners initialColor seed0 =
  go 0 pendingCorners 0 initialColor seed0 []
  where
    cornerCount = 1 + length pendingCorners

    go step remainingCorners spline currentColor seedCur acc
      | step >= edgeCount = (currentColor, seedCur, acc)
      | otherwise =
          let edgeIx = (start + step) `mod` edgeCount
              (nextRemaining, spline', assignedColor, nextSeed) =
                case remainingCorners of
                  nextCorner : rest
                    | nextCorner == edgeIx ->
                        let splineNext = spline + 1
                            banned =
                              if splineNext == cornerCount - 1
                                then Just initialColor
                                else Nothing
                            (nextColor, seedAfter) = switchColorAvoid currentColor seedCur banned
                         in (rest, splineNext, nextColor, seedAfter)
                  _ -> (remainingCorners, spline, currentColor, seedCur)
           in go (step + 1) nextRemaining spline' assignedColor nextSeed ((edgeIx, assignedColor) : acc)

colorForStep :: Int -> Word8 -> Word8 -> Word8 -> Int -> Word8
colorForStep edgeCount c0 c1 c2 step =
  case symmetricalTrichotomy step edgeCount of
    (-1) -> c0
    0 -> c1
    _ -> c2

symmetricalTrichotomy :: Int -> Int -> Int
symmetricalTrichotomy pos count
  | count <= 1 = 0
  | otherwise = floor (3.0 + (2.875 * posVal / spanVal) - 1.4375 + 0.5 :: Double) - 3
  where
    posVal = fromIntegral pos :: Double
    spanVal = fromIntegral (count - 1) :: Double

contourCorners :: [Edge] -> [Int]
contourCorners segments =
  [ i
    | (i, prevEndDir, nextStartDir) <- zip3 [0 ..] (rotateRight endDirs) startDirs,
      isCorner prevEndDir nextStartDir
  ]
  where
    startDirs = fmap edgeStartDirectionUnit segments
    endDirs = fmap edgeEndDirectionUnit segments

contourCornersForGroups :: [[Edge]] -> [Int]
contourCornersForGroups groupedSegments =
  [ i
    | (i, prevEndDir, nextStartDir) <- zip3 [0 ..] (rotateRight endDirs) startDirs,
      isCorner prevEndDir nextStartDir
  ]
  where
    startDirs = map groupStartDir groupedSegments
    endDirs = map groupEndDir groupedSegments
    groupStartDir segs =
      case segs of
        seg : _ -> edgeStartDirectionUnit seg
        [] -> (0.0, 1.0)
    groupEndDir segs =
      case reverse segs of
        seg : _ -> edgeEndDirectionUnit seg
        [] -> (0.0, 1.0)

rotateRight :: [a] -> [a]
rotateRight values =
  case reverse values of
    [] -> []
    (lastValue : reversedInit) -> lastValue : reverse reversedInit

segmentDirectionUnit :: (Pt, Pt) -> (Double, Double)
segmentDirectionUnit (start, end) =
  normalizeDirection (end.x - start.x) (end.y - start.y)

edgeStartDirectionUnit :: Edge -> (Double, Double)
edgeStartDirectionUnit edge =
  case edge.c of
    Nothing -> segmentDirectionUnit (edge.a, edge.b)
    Just ctrl ->
      let tx = ctrl.x - edge.a.x
          ty = ctrl.y - edge.a.y
       in if (tx * tx) + (ty * ty) <= 1.0e-18
            then segmentDirectionUnit (edge.a, edge.b)
            else normalizeDirection tx ty

edgeEndDirectionUnit :: Edge -> (Double, Double)
edgeEndDirectionUnit edge =
  case edge.c of
    Nothing -> segmentDirectionUnit (edge.a, edge.b)
    Just ctrl ->
      let tx = edge.b.x - ctrl.x
          ty = edge.b.y - ctrl.y
       in if (tx * tx) + (ty * ty) <= 1.0e-18
            then segmentDirectionUnit (edge.a, edge.b)
            else normalizeDirection tx ty

normalizeDirection :: Double -> Double -> (Double, Double)
normalizeDirection vx vy
  | lenSq <= 1.0e-18 = (0.0, 1.0)
  | otherwise =
      let invLen = recip (sqrt lenSq)
       in (vx * invLen, vy * invLen)
  where
    lenSq = (vx * vx) + (vy * vy)

isCorner :: (Double, Double) -> (Double, Double) -> Bool
isCorner (ax, ay) (bx, by) =
  dot <= 0 || abs cross > crossThreshold
  where
    dot = (ax * bx) + (ay * by)
    cross = (ax * by) - (ay * bx)

crossThreshold :: Double
crossThreshold = sin 3.0

applyColors :: [Edge] -> [Word8] -> [Edge]
applyColors edges colors =
  go edges colors
  where
    go [] _ = []
    go (edge : restEdges) (edgeColor : restColors) =
      edge {col = edgeColor} : go restEdges restColors
    go (edge : restEdges) [] =
      edge {col = colorWhite} : go restEdges []

contourSegments :: Contour -> [Edge]
contourSegments contour =
  case contour.segs of
    [] -> lineEdgesFromPoints contour.pts
    segments -> segments

colorsFromAssignments :: Int -> Word8 -> [(Int, Word8)] -> [Word8]
colorsFromAssignments edgeCount fallback assignments =
  let ordered = map snd (sortOn fst assignments)
   in take edgeCount (ordered <> repeat fallback)

seedExtract2 :: Word64 -> (Int, Word64)
seedExtract2 seedVal = (fromIntegral (seedVal .&. 1), seedVal `shiftR` 1)

seedExtract3 :: Word64 -> (Int, Word64)
seedExtract3 seedVal = (fromIntegral (seedVal `mod` 3), seedVal `div` 3)

initColor :: Word64 -> (Word64, Word8)
initColor seedVal =
  let (idx, seedNext) = seedExtract3 seedVal
      color =
        case idx of
          0 -> colorCyan
          1 -> colorMagenta
          _ -> colorYellow
   in (seedNext, color)

switchColor :: Word8 -> Word64 -> (Word8, Word64)
switchColor color seedVal =
  let (bit, seedNext) = seedExtract2 seedVal
      shifted = (fromIntegral color :: Int) `shiftL` (1 + bit)
      out = (shifted .|. (shifted `shiftR` 3)) .&. fromIntegral colorWhite
   in (fromIntegral out, seedNext)

switchColorAvoid :: Word8 -> Word64 -> Maybe Word8 -> (Word8, Word64)
switchColorAvoid color seedVal banned =
  case banned of
    Nothing -> switchColor color seedVal
    Just bannedColor ->
      let combined = color .&. bannedColor
       in if combined == colorRed || combined == colorGreen || combined == colorBlue
            then (combined `xor` colorWhite, seedVal)
            else switchColor color seedVal

colorRed :: Word8
colorRed = 1

colorGreen :: Word8
colorGreen = 2

colorBlue :: Word8
colorBlue = 4

colorYellow :: Word8
colorYellow = 3

colorMagenta :: Word8
colorMagenta = 5

colorCyan :: Word8
colorCyan = 6

colorWhite :: Word8
colorWhite = 7

segmentPairs :: [Pt] -> [(Pt, Pt)]
segmentPairs points =
  case points of
    [] -> []
    [_] -> []
    _ -> zip points (drop 1 points <> take 1 points)

lineEdgesFromPoints :: [Pt] -> [Edge]
lineEdgesFromPoints points =
  fmap mkEdge (segmentPairs points)
  where
    mkEdge (start, end) =
      Edge
        { a = start,
          b = end,
          c = Nothing,
          col = colorWhite
        }

splitByLengthsMaybe :: [Int] -> [a] -> Maybe [[a]]
splitByLengthsMaybe lengths values = go lengths values
  where
    go [] [] = Just []
    go [] (_ : _) = Nothing
    go (len : rest) xs = do
      (front, back) <- takeExactMaybe len xs
      chunks <- go rest back
      pure (front : chunks)

takeExactMaybe :: Int -> [a] -> Maybe ([a], [a])
takeExactMaybe n items
  | n < 0 = Nothing
  | otherwise = go n [] items
  where
    go 0 acc rest = Just (reverse acc, rest)
    go _ _ [] = Nothing
    go k acc (x : xs) = go (k - 1) (x : acc) xs

normalizeGroups :: Int -> [Int] -> [Int]
normalizeGroups edgeCount groups
  | validGroupsForEdgeCount edgeCount groups = groups
  | edgeCount <= 0 = []
  | otherwise = replicate edgeCount 1

validGroupsForEdgeCount :: Int -> [Int] -> Bool
validGroupsForEdgeCount edgeCount groups =
  edgeCount >= 0
    && not (null groups)
    && all (> 0) groups
    && sum groups == edgeCount

expandGroupColors :: [Int] -> [Word8] -> [Word8]
expandGroupColors groupLens groupColors =
  concat (zipWith replicate groupLens (groupColors <> repeat colorWhite))

contourEdgeCount :: [Pt] -> Int
contourEdgeCount points =
  case points of
    [] -> 0
    [_] -> 0
    _ -> length points

contourHasDuplicateClose :: [Pt] -> Bool
contourHasDuplicateClose points =
  case (points, reverse points) of
    (first : _, lastPoint : _) -> pointsClose first lastPoint
    _ -> False

hasAdjacentDuplicate :: [Pt] -> Bool
hasAdjacentDuplicate points =
  case points of
    [] -> False
    _ : [] -> False
    a : b : rest -> pointsClose a b || hasAdjacentDuplicate (b : rest)

dropDuplicateClose :: [Pt] -> [Pt]
dropDuplicateClose points =
  case (points, reverse points) of
    ([], _) -> []
    (_, []) -> []
    (first : _, lastPoint : restRev)
      | pointsClose first lastPoint -> reverse restRev
      | otherwise -> points

dropAdjacentDuplicateClose :: [Pt] -> [Pt]
dropAdjacentDuplicateClose points =
  case points of
    [] -> []
    first : rest -> reverse (foldl' step [first] rest)
  where
    step acc point =
      case acc of
        prev : _
          | pointsClose prev point -> acc
        _ -> point : acc

pointsClose :: Pt -> Pt -> Bool
pointsClose p q =
  let dx = p.x - q.x
      dy = p.y - q.y
   in (dx * dx) + (dy * dy) <= 1.0e-18
