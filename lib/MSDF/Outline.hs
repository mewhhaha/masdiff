module MSDF.Outline
  ( Point(..)
  , Edge(..)
  , Vec2
  , insertImpliedPoints
  , contourToEdges
  , flattenEdge
  , edgeStartDir
  , edgeEndDir
  ) where

-- | A point from the glyph outline.
data Point = Point
  { px :: Double
  , py :: Double
  , pOn :: Bool
  } deriving (Eq, Show)

type Vec2 = (Double, Double)

-- | A quadratic edge or straight line.
data Edge
  = EdgeLine !Vec2 !Vec2
  | EdgeQuad !Vec2 !Vec2 !Vec2
  deriving (Eq, Show)

-- | Insert implied on-curve points between consecutive off-curve points.
insertImpliedPoints :: [Point] -> [Point]
insertImpliedPoints [] = []
insertImpliedPoints pts =
  case ensureStartsOnCurve pts of
    [] -> []
    (p:ps) -> go (p:ps) p []
  where
    ensureStartsOnCurve xs =
      case xs of
        [] -> []
        (p:_) ->
          if pOn p then xs
          else
            let lastP = last xs
                mid = midpointPoint lastP p
            in mid : xs

    midpointPoint a b = Point ((px a + px b) * 0.5) ((py a + py b) * 0.5) True

    go [] _ acc = reverse acc
    go [p] first acc =
      let acc' = p : acc
      in if pOn p && pOn first
         then reverse acc'
         else if not (pOn p) && not (pOn first)
              then reverse (midpointPoint p first : acc')
              else reverse acc'
    go (p1:p2:rest) first acc
      | pOn p1 && not (pOn p2) = go (p2:rest) first (p1:acc)
      | not (pOn p1) && not (pOn p2) =
          let mid = midpointPoint p1 p2
          in go (p2:rest) first (mid:p1:acc)
      | otherwise = go (p2:rest) first (p1:acc)

-- | Convert a contour into edges (quadratic or line segments).
contourToEdges :: [Point] -> [Edge]
contourToEdges [] = []
contourToEdges pts0 =
  let pts = insertImpliedPoints pts0
      n = length pts
  in if n < 2 then [] else buildEdges pts
  where
    buildEdges pts =
      case pts of
        [] -> []
        (startPoint:rest) ->
          let points = rest ++ [startPoint]
          in go startPoint points

    go _ [] = []
    go current (p1:rest)
      | pOn p1 =
          EdgeLine (toVec current) (toVec p1) : go p1 rest
      | otherwise =
          case rest of
            [] -> []
            (p2:rest2) ->
              let endPoint = if pOn p2
                             then p2
                             else Point ((px p1 + px p2) * 0.5) ((py p1 + py p2) * 0.5) True
              in EdgeQuad (toVec current) (toVec p1) (toVec endPoint) : go endPoint (if pOn p2 then rest2 else p2:rest2)

    toVec p = (px p, py p)

edgeStartDir :: Edge -> Vec2
edgeStartDir (EdgeLine (x0, y0) (x1, y1)) = (x1 - x0, y1 - y0)
edgeStartDir (EdgeQuad (x0, y0) (x1, y1) _ ) = (x1 - x0, y1 - y0)

edgeEndDir :: Edge -> Vec2
edgeEndDir (EdgeLine (x0, y0) (x1, y1)) = (x1 - x0, y1 - y0)
edgeEndDir (EdgeQuad _ (x1, y1) (x2, y2)) = (x2 - x1, y2 - y1)

-- | Flatten an edge to line segments with a flatness tolerance.
flattenEdge :: Double -> Edge -> [(Vec2, Vec2)]
flattenEdge _ (EdgeLine a b) = [(a, b)]
flattenEdge eps (EdgeQuad p0 p1 p2)
  | quadFlatEnough eps p0 p1 p2 = [(p0, p2)]
  | otherwise =
      let (l0, l1, l2, r0, r1, r2) = subdivide p0 p1 p2
      in flattenEdge eps (EdgeQuad l0 l1 l2) ++ flattenEdge eps (EdgeQuad r0 r1 r2)

quadFlatEnough :: Double -> Vec2 -> Vec2 -> Vec2 -> Bool
quadFlatEnough eps p0 p1 p2 =
  let dist = distancePointLine p1 p0 p2
  in dist <= eps

subdivide :: Vec2 -> Vec2 -> Vec2 -> (Vec2, Vec2, Vec2, Vec2, Vec2, Vec2)
subdivide p0 p1 p2 =
  let p01 = midpoint p0 p1
      p12 = midpoint p1 p2
      p012 = midpoint p01 p12
  in (p0, p01, p012, p012, p12, p2)

midpoint :: Vec2 -> Vec2 -> Vec2
midpoint (x0, y0) (x1, y1) = ((x0 + x1) * 0.5, (y0 + y1) * 0.5)

distancePointLine :: Vec2 -> Vec2 -> Vec2 -> Double
distancePointLine (px', py') (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else ((px' - x0) * dx + (py' - y0) * dy) / len2
      t' = max 0 (min 1 t)
      cx = x0 + t' * dx
      cy = y0 + t' * dy
      ex = px' - cx
      ey = py' - cy
  in sqrt (ex * ex + ey * ey)
