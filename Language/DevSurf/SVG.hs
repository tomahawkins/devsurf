module Language.DevSurf.SVG
  ( exportSVG
  ) where

import Text.Printf

import Language.DevSurf.Types

-- | Plot a list of 'Curve's, projected onto the XY plane, in SVG.
exportSVG :: [Curve] -> String
exportSVG curves = unlines
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
  , unlines $ concat [ [ line' p1 p2 | (p1, p2) <- zip curve (tail curve) ] | curve <- curves ]
  , "</svg>"
  ]

line' :: Vector -> Vector -> String
line' (x1, y1, _) (x2, y2, _) = printf "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"black\" stroke-width=\"1\" />" x1 y1 x2 y2

