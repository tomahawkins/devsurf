module Language.DevSurf.Types
  ( Vector
  , Vertex
  , Normal
  , Triangle
  , Mesh
  , Panel
  , PanelFace (..)
  , Curve
  , Transform (..)
  , scale
  , move
  , rotateX
  , rotateY
  , rotateZ
  , cross
  , dot
  , add
  , sub
  , neg
  , normalize
  , magnitude
  , meshPanel
  , canFlatten
  , flatten
  , profile
  , subdivide
  , subdivideN
  , subdivideToPrecision
  , loft
  , triangleNormal
  ) where

import Data.List
import Data.Maybe (fromJust)

type Vector   = (Double, Double, Double)
type Vertex   = Vector
type Normal   = Vector
type Triangle = (Vertex, Vertex, Vertex)
type Mesh     = [Triangle]

-- | Analogous to an OpenGL triangle strip.
type Panel = [Vertex]

-- | A curve is built up of line segments.
type Curve = [Vertex]

-- | Vector cross product.
cross :: Vector -> Vector -> Vector
cross (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

-- | Vector dot product.
dot :: Vector -> Vector -> Double
dot (a1, a2, a3) (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3

-- | Vector normalization.
normalize :: Vector -> Vector
normalize a@(x, y, z) = (x / m, y / m, z / m)
  where
  m = magnitude a

-- | Magnitude of a vector.
magnitude :: Vector -> Double
magnitude (x, y, z) = sqrt $ x ** 2 + y ** 2 + z ** 2

-- | Vector addition.
add :: Vector -> Vector -> Vector
add (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

-- | Vector subtraction.
sub :: Vector -> Vector -> Vector
sub (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

-- | Vector negation.
neg :: Vector -> Vector
neg a = (0, 0, 0) `sub` a

data PanelFace = RHR | LHR

-- | Generate a 'Mesh' from a 'Panel'.  XXX Need to handle different face directions.
meshPanel :: PanelFace -> Panel -> Mesh
meshPanel face a = case a of
  _ : _ : _ : _ -> [ if flip then (b, a, c) else (a, b, c) | (flip, a, b, c) <- zip4 flips a (tail a) (tail $ tail a) ]
  _             -> error "meshPanel: Not enough points to form a triangle."
  where
  flips = concat $ repeat $ case face of
    RHR -> [False, True]
    LHR -> [True, False]

class    Transform a                  where transform :: (Vector -> Vector) -> a -> a
instance Transform Vector             where transform f a = f a
instance Transform Triangle           where transform f (a, b, c) = (f a, f b, f c)
instance Transform a => Transform [a] where transform f a = map (transform f) a

move :: Transform a => Vector -> a -> a
move a = transform $ add a

scale :: Transform a => Vector -> a -> a
scale (a, b, c) = transform $ \(x, y, z) -> (a * x, b * y, c * z)

rotateX :: Transform a => Double -> a -> a
rotateX angle = transform f
  where
  f (x, y, z) = (x, m * cos angle', m * sin angle')
    where
    angle' = angle + atan2 z y
    m = sqrt $ y ** 2 + z ** 2

rotateY :: Transform a => Double -> a -> a
rotateY angle = transform f
  where
  f (x, y, z) = ((-m) * cos angle', y, m * sin angle')
    where
    angle' = angle + atan2 z (-x)
    m = sqrt $ x ** 2 + z ** 2

rotateZ :: Transform a => Double -> a -> a
rotateZ angle = transform f
  where
  f (x, y, z) = (m * cos angle', m * sin angle', z)
    where
    angle' = angle + atan2 y x
    m = sqrt $ x ** 2 + y ** 2

-- | Given a surface normal dot product threshold, checks if a panel can be flattened (unrolled).
canFlatten :: Double -> Panel -> Bool
canFlatten threshold panel = all (>= threshold) [ a `dot` b | (a, b) <- zip normals $ tail normals ]
  where
  normals = map triangleNormal $ meshPanel RHR panel

-- | Flatten (unroll) a 'Panel' to the XY plane.
flatten :: Panel -> Panel
flatten (a : rest) = f [(0, 0, 0)] $ move (neg a) rest
  where
  -- On entry: last point at origin, second to last point on Y axis.
  f :: Panel -> Panel -> Panel
  f sofar [] = sofar
  f b0 a0@((x0, _, z0) : _) = f (b4 ++ [p4]) a4
    where
    -- Rotate point to X-Y plane.
    a1@((x1, y1, _) : _) = rotateY (atan2 z0 x0) a0
    -- Rotate point to positive Y axis.
    a2@(p2 : _) = rotateZ (pi / 2 - atan2 y1 x1) a1
    b2          = rotateZ (pi / 2 - atan2 y1 x1) b0
    -- Move point to origin.
    a3 = move (neg p2) a2
    b3 = move (neg p2) b2
    -- Scale to mirror X-Z plane.
    p4 : a4 = scale (1, -1, 1) a3
    b4      = scale (1, -1, 1) b3

flatten _ = error "flatten: Not enough points to form a triangle."

-- | Build a profile of a 'Panel'
profile :: Panel -> Curve
profile curve = a ++ reverse b ++ [head a]
  where
  (a, b) = split curve
  split :: [Vector] -> ([Vector], [Vector])
  split a = case a of
    [] -> ([], [])
    [a] -> ([a], [])
    a : b : rest -> (a : a', b : b') where (a', b') = split rest


-- | Subdivide a 'Curve' once.
subdivide :: Curve -> Curve
subdivide = f2 . f1
  where
  f1 :: Curve -> Curve
  f1 a = case a of
    a : b : c -> a : ave2 a b : f1 (b : c)
    a -> a

  f2 :: Curve -> Curve
  f2 a = case a of
    [] -> []
    a : b -> a : f3 b

  f3 :: Curve -> Curve
  f3 a = case a of
    a : b : c : d -> a : ave3 a b c : f3 (c : d)
    a -> a

  ave2 :: Vertex -> Vertex -> Vertex
  ave2 (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

  ave3 :: Vertex -> Vertex -> Vertex -> Vertex
  ave3 (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = ((x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3, (z1 + z2 + z3) / 3)

-- | Subdivde a 'Curve' N times.
subdivideN :: Int -> Curve -> Curve
subdivideN n a = iterate subdivide a !! n

-- | Subdivide a 'Curve' until max distance between points has a given precision.
subdivideToPrecision :: Double -> Curve -> Curve
subdivideToPrecision p a = fromJust $ find prec $ iterate subdivide a
  where
  prec :: Curve -> Bool
  prec a = all (<= p) [ magnitude (b `sub` a) | (a, b) <- zip a (tail' a) ]

  tail' a = case a of
    [] -> []
    _ : a -> a

-- | Loft a 'Panel' between two 'Curve's.
loft :: Curve -> Curve -> Panel
loft a b = concat [ [a, b] | (a, b) <- zip a b ]

-- | Computes a surface normal for a 'Triangle'.
triangleNormal :: Triangle -> Vector
triangleNormal (v1, v2, v3) = normalize $ a `cross` b
  where
  a = v2 `sub` v1
  b = v3 `sub` v2

