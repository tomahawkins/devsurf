module Language.DevSurf.STL
  ( exportSTL
  ) where

import Language.DevSurf.Types

-- | Generate a STL file from a 'Mesh'.
exportSTL :: Mesh -> String
exportSTL mesh = unlines
  [ "solid "
  , concatMap triangle mesh
  , "endsolid "
  ]
  where
  triangle :: Triangle -> String
  triangle t@(a, b, c) = unlines
    [ "facet normal " ++ showVector (triangleNormal t)
    , "  outer loop"
    , "    vertex " ++ showVector a
    , "    vertex " ++ showVector b
    , "    vertex " ++ showVector c
    , "  endloop"
    , "endfacet"
    ]

  showVector :: Vector -> String
  showVector (x, y, z) = show x ++ " " ++ show y ++ "  " ++ show z

triangleNormal :: Triangle -> Vector
triangleNormal (v1, v2, v3) = normalize $ a `cross` b
  where
  a = v2 `sub` v1
  b = v3 `sub` v2


