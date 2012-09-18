module Language.DevSurf.Types
  ( Vector
  , Vertex
  , Normal
  , Triangle
  , Mesh
  , cross
  , sub
  , normalize
  , magnitude
  ) where

type Vector   = (Double, Double, Double)
type Vertex   = Vector
type Normal   = Vector
type Triangle = (Vertex, Vertex, Vertex)
type Mesh     = [Triangle]

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

-- | Vector subtraction.
sub :: Vector -> Vector -> Vector
sub (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

