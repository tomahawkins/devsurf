module Language.DevSurf.Types
  ( Vector
  , Vertex
  , Normal
  , Triangle
  , Mesh
  , Panel
  , cross
  , sub
  , normalize
  , magnitude
  , meshPanel
  ) where

import Data.List

type Vector   = (Double, Double, Double)
type Vertex   = Vector
type Normal   = Vector
type Triangle = (Vertex, Vertex, Vertex)
type Mesh     = [Triangle]

-- | Analogous to an OpenGL triangle strip.  First three vertices determine surface normals.
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

-- | Vector subtraction.
sub :: Vector -> Vector -> Vector
sub (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

-- | Generate a 'Mesh' from a 'Panel'.
meshPanel :: Panel -> Mesh
meshPanel a = case a of
  _ : _ : _ : _ -> [ if flip then (b, a, c) else (a, b, c) | (flip, a, b, c) <- zip4 (concat $ repeat [False, True]) a (tail a) (tail $ tail a) ]
  _             -> error "meshPanel: Not enough points to form a triangle."

