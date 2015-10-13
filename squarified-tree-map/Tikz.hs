module Tikz
where

import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.List
import Text.Printf

import Squarified

render :: [Color] -> Tree -> String
render colors t =
    "\\begin{tikzpicture}\n" ++
    concat (zipWith (uncurry . renderRect) (cycle colors) (layoutAt t (Point 0 0))) ++
    "\\end{tikzpicture}\n"

renderRect :: Color -> Point -> Rectangle -> String
renderRect c (Point x y) (Rectangle dx dy) =
  printf "  \\draw [draw=black, fill=%s] " (tikzColor c) ++
  intercalate " -- " (map show corners ++ ["cycle"]) ++
  ";\n"

  where corners = [(x, y), (x+dx, y), (x+dx, y+dy), (x, y+dy)]
                
type Color = Colour Double
tikzColor :: Color -> String
tikzColor = uncurryRGB cvt . fmap ((round::Double->Int) . (100.0 *)) . toSRGB
    where cvt r g b = printf "{rgb,100:red,%d;green,%d;blue,%d}" r g b
