module MSDF.Geometry
  ( Vec2
  , Point(..)
  , Edge(..)
  , EdgeRef(..)
  , EdgeColor(..)
  , ColoredEdge(..)
  , edgePointAt
  , edgeStartPoint
  , edgeEndPoint
  , edgeDistanceSqWithParam
  , edgeTangentAt
  , edgeBBox
  , edgeDistanceSq
  , edgeLengthApprox
  , edgeSamplePoints
  , bboxFromEdges
  , vecAdd
  , vecSub
  , vecScale
  , dot
  ) where

import MSDF.Outline (Point(..), Edge(..), Vec2)
import MSDF.Types (BBox(..), bboxUnion)

data EdgeRef = EdgeRef
  { contourId :: !Int
  , edgeId :: !Int
  , edge :: !Edge
  } deriving (Eq, Show)

data EdgeColor
  = ColorRed
  | ColorGreen
  | ColorBlue
  | ColorCyan
  | ColorMagenta
  | ColorYellow
  | ColorWhite
  deriving (Eq, Show)

data ColoredEdge = ColoredEdge
  { color :: !EdgeColor
  , edgeRef :: !EdgeRef
  } deriving (Eq, Show)

edgePointAt :: Edge -> Double -> Vec2
edgePointAt (EdgeLine (x0, y0) (x1, y1)) t =
  (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)
edgePointAt (EdgeQuad p0 p1 p2) t =
  bezier p0 p1 p2 t

edgeStartPoint :: Edge -> Vec2
edgeStartPoint (EdgeLine p0 _) = p0
edgeStartPoint (EdgeQuad p0 _ _) = p0

edgeEndPoint :: Edge -> Vec2
edgeEndPoint (EdgeLine _ p1) = p1
edgeEndPoint (EdgeQuad _ _ p2) = p2

edgeTangentAt :: Edge -> Double -> Vec2
edgeTangentAt (EdgeLine a b) _ = vecSub b a
edgeTangentAt (EdgeQuad p0 p1 p2) t =
  let mt = 1 - t
      v0 = vecScale mt (vecSub p1 p0)
      v1 = vecScale t (vecSub p2 p1)
  in vecScale 2 (vecAdd v0 v1)

edgeBBox :: Edge -> BBox
edgeBBox (EdgeLine (x0, y0) (x1, y1)) =
  BBox
    { xMin = min x0 x1
    , yMin = min y0 y1
    , xMax = max x0 x1
    , yMax = max y0 y1
    }
edgeBBox (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
  let xs = [x0, x1, x2] ++ quadExtrema1D x0 x1 x2
      ys = [y0, y1, y2] ++ quadExtrema1D y0 y1 y2
  in BBox
      { xMin = minimum xs
      , yMin = minimum ys
      , xMax = maximum xs
      , yMax = maximum ys
      }

quadExtrema1D :: Double -> Double -> Double -> [Double]
quadExtrema1D a0 a1 a2 =
  let denom = a0 - 2 * a1 + a2
  in if abs denom < 1e-12
     then []
     else
       let t = (a0 - a1) / denom
       in if t > 0 && t < 1
          then [quadAt1D a0 a1 a2 t]
          else []

quadAt1D :: Double -> Double -> Double -> Double -> Double
quadAt1D a0 a1 a2 t =
  let mt = 1 - t
  in mt * mt * a0 + 2 * mt * t * a1 + t * t * a2

edgeDistanceSq :: Vec2 -> Edge -> Double
edgeDistanceSq p (EdgeLine a b) = distanceSqPointLine p a b
edgeDistanceSq p (EdgeQuad p0 p1 p2) = distanceSqPointQuad p0 p1 p2 p

edgeDistanceSqWithParam :: Vec2 -> Edge -> (Double, Double)
edgeDistanceSqWithParam p (EdgeLine a b) =
  let dx = fst b - fst a
      dy = snd b - snd a
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else ((fst p - fst a) * dx + (snd p - snd a) * dy) / len2
      t' = max 0 (min 1 t)
      cx = fst a + t' * dx
      cy = snd a + t' * dy
      ex = fst p - cx
      ey = snd p - cy
  in (ex * ex + ey * ey, t')
edgeDistanceSqWithParam p (EdgeQuad p0 p1 p2) =
  let (x0, y0) = p0
      (x1, y1) = p1
      (x2, y2) = p2
      (px, py) = p
      ax = x0 - 2 * x1 + x2
      ay = y0 - 2 * y1 + y2
      bx = 2 * (x1 - x0)
      by = 2 * (y1 - y0)
      cx = x0 - px
      cy = y0 - py
      c3 = 2 * (ax * ax + ay * ay)
      c2 = 3 * (ax * bx + ay * by)
      c1 = (bx * bx + by * by) + 2 * (ax * cx + ay * cy)
      c0 = bx * cx + by * cy
      d0 = distanceSq p0 p
      d1 = distanceSq p2 p
      start = if d0 < d1 then (d0, 0) else (d1, 1)
      pick t (bestD, bestT) =
        if t >= 0 && t <= 1
        then
          let d = distanceSqBezier2 p0 p1 p2 p t
          in if d < bestD then (d, t) else (bestD, bestT)
        else (bestD, bestT)
  in foldCubicRoots c3 c2 c1 c0 start pick

distanceSqPointLine :: Vec2 -> Vec2 -> Vec2 -> Double
distanceSqPointLine (px', py') (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else ((px' - x0) * dx + (py' - y0) * dy) / len2
      t' = max 0 (min 1 t)
      cx = x0 + t' * dx
      cy = y0 + t' * dy
      ex = px' - cx
      ey = py' - cy
  in ex * ex + ey * ey

distanceSqPointQuad :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double
distanceSqPointQuad p0 p1 p2 p =
  let (x0, y0) = p0
      (x1, y1) = p1
      (x2, y2) = p2
      (px, py) = p
      ax = x0 - 2 * x1 + x2
      ay = y0 - 2 * y1 + y2
      bx = 2 * (x1 - x0)
      by = 2 * (y1 - y0)
      cx = x0 - px
      cy = y0 - py
      c3 = 2 * (ax * ax + ay * ay)
      c2 = 3 * (ax * bx + ay * by)
      c1 = (bx * bx + by * by) + 2 * (ax * cx + ay * cy)
      c0 = bx * cx + by * cy
      d0 = distanceSq p0 p
      d1 = distanceSq p2 p
      start = if d0 < d1 then d0 else d1
      pick t best =
        if t >= 0 && t <= 1
        then min best (distanceSqBezier2 p0 p1 p2 p t)
        else best
  in foldCubicRoots c3 c2 c1 c0 start pick

distanceSqBezier2 :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Double -> Double
distanceSqBezier2 (x0, y0) (x1, y1) (x2, y2) (px, py) t =
  let u = 1 - t
      uu = u * u
      tt = t * t
      x = uu * x0 + 2 * u * t * x1 + tt * x2
      y = uu * y0 + 2 * u * t * y1 + tt * y2
      dx = x - px
      dy = y - py
  in dx * dx + dy * dy

foldCubicRoots :: Double -> Double -> Double -> Double -> r -> (Double -> r -> r) -> r
foldCubicRoots a b c d z f
  | abs a < 1e-12 = foldQuadraticRoots b c d z f
  | otherwise =
      let a' = b / a
          b' = c / a
          c' = d / a
          p = b' - a' * a' / 3
          q = 2 * a' * a' * a' / 27 - a' * b' / 3 + c'
          disc = (q * q) / 4 + (p * p * p) / 27
      in if disc > 0
         then
           let sqrtDisc = sqrt disc
               u = cbrt (-q / 2 + sqrtDisc)
               v = cbrt (-q / 2 - sqrtDisc)
               t = u + v - a' / 3
           in f t z
         else if abs disc < 1e-12
              then
                let u = cbrt (-q / 2)
                    t1 = 2 * u - a' / 3
                    t2 = -u - a' / 3
                in f t2 (f t1 z)
              else
                let r = sqrt (-(p * p * p) / 27)
                    phi = acos (-q / (2 * r))
                    t = 2 * cbrt r
                    t1 = t * cos (phi / 3) - a' / 3
                    t2 = t * cos ((phi + 2 * pi) / 3) - a' / 3
                    t3 = t * cos ((phi + 4 * pi) / 3) - a' / 3
                in f t3 (f t2 (f t1 z))

foldQuadraticRoots :: Double -> Double -> Double -> r -> (Double -> r -> r) -> r
foldQuadraticRoots a b c z f
  | abs a < 1e-12 = foldLinearRoots b c z f
  | otherwise =
      let disc = b * b - 4 * a * c
      in if disc < 0
         then z
         else
           let sqrtDisc = sqrt disc
               t1 = (-b + sqrtDisc) / (2 * a)
               t2 = (-b - sqrtDisc) / (2 * a)
           in f t2 (f t1 z)

foldLinearRoots :: Double -> Double -> r -> (Double -> r -> r) -> r
foldLinearRoots a b z f
  | abs a < 1e-12 = z
  | otherwise = f (-b / a) z

edgeLengthApprox :: Edge -> Double
edgeLengthApprox (EdgeLine (x0, y0) (x1, y1)) =
  let dx = x1 - x0
      dy = y1 - y0
  in sqrt (dx * dx + dy * dy)
edgeLengthApprox (EdgeQuad (x0, y0) (x1, y1) (x2, y2)) =
  let dx01 = x1 - x0
      dy01 = y1 - y0
      dx12 = x2 - x1
      dy12 = y2 - y1
      d01 = sqrt (dx01 * dx01 + dy01 * dy01)
      d12 = sqrt (dx12 * dx12 + dy12 * dy12)
  in d01 + d12

edgeSamplePoints :: Edge -> [Vec2]
edgeSamplePoints e =
  case e of
    EdgeLine _ _ -> map (edgePointAt e) [0.25, 0.5, 0.75]
    EdgeQuad _ _ _ -> map (edgePointAt e) [0.2, 0.5, 0.8]

bboxFromEdges :: [EdgeRef] -> Maybe BBox
bboxFromEdges [] = Nothing
bboxFromEdges (e:es) =
  let bb0 = edgeBBox e.edge
  in Just (foldl' (\acc er -> bboxUnion acc (edgeBBox er.edge)) bb0 es)

bezier :: Vec2 -> Vec2 -> Vec2 -> Double -> Vec2
bezier p0 p1 p2 t =
  let u = 1 - t
      tt = t * t
      uu = u * u
      p0' = vecScale uu p0
      p1' = vecScale (2 * u * t) p1
      p2' = vecScale tt p2
  in vecAdd p0' (vecAdd p1' p2')

distanceSq :: Vec2 -> Vec2 -> Double
distanceSq (x0, y0) (x1, y1) =
  let dx = x0 - x1
      dy = y0 - y1
  in dx * dx + dy * dy

dot :: Vec2 -> Vec2 -> Double
dot (x0, y0) (x1, y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
{-# INLINE vecAdd #-}

vecSub :: Vec2 -> Vec2 -> Vec2
vecSub (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)
{-# INLINE vecSub #-}

vecScale :: Double -> Vec2 -> Vec2
vecScale s (x, y) = (s * x, s * y)
{-# INLINE vecScale #-}

cbrt :: Double -> Double
cbrt x = if x < 0 then -((abs x) ** (1 / 3)) else x ** (1 / 3)
