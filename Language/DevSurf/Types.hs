module Language.DevSurf.Types
  ( Vector
  , Vertex
  , Normal
  , Triangle
  , Mesh
  , Panel
  , PanelFace (..)
  , Transform (..)
  , scale
  , move
  , rotateX
  , rotateY
  , rotateZ
  , cross
  , add
  , sub
  , neg
  , normalize
  , magnitude
  , meshPanel
  , flattenPanel
  ) where

import Data.List

type Vector   = (Double, Double, Double)
type Vertex   = Vector
type Normal   = Vector
type Triangle = (Vertex, Vertex, Vertex)
type Mesh     = [Triangle]

-- | Analogous to an OpenGL triangle strip.
type Panel = [Vertex]

-- | Vector cross product.
cross :: Vector -> Vector -> Vector
cross (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

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

class    Transform a        where transform :: (Vector -> Vector) -> a -> a
instance Transform Vector   where transform f a = f a
instance Transform Panel    where transform f a = map (transform f) a
instance Transform Triangle where transform f (a, b, c) = (f a, f b, f c)
instance Transform Mesh     where transform f a = map (transform f) a

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

-- | Flatten (unroll) a 'Panel' to the XY plane.
flattenPanel :: Panel -> Panel
flattenPanel p0@(v0 : (x1, y1, _) : _) = p3
  where
  -- Move first point to the origin.
  p1 = move (neg v0) p0
  -- Rotate second point to YZ plane.
  p2@(_ : (_, y2, z2) : _) = rotateZ (pi / 2 - atan2 y1 x1) p1
  -- Rotate second point to Y axis.
  p3 = rotateX (- atan2 z2 y2) p2

flattenPanel _ = error "flattenPanel: Not enough points to form a triangle."

