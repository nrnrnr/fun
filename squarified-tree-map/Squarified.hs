module Squarified
where

import Debug.Trace
import Data.List
import Text.Printf

data Axis = X | Y
  deriving Show

type Position = Double
type Area = Double

data Tree = Split Axis Position Tree Tree
          | Leaf Rectangle 

instance Show Tree where
    show = showTree "" 0 0

showTree pfx x y (Leaf r) = pfx ++ show r ++ "\n"
showTree pfx x y (Split X pos t1 t2) =
    pfx ++ "x @ " ++ show (x + twidth t1) ++ "   [y = " ++ show y ++ "]\n" ++
        showTree (pfx ++ "  ") x y t1 ++
        showTree (pfx ++ "  ") (x+twidth t1) y t2
showTree pfx x y (Split Y pos t1 t2) =
    pfx ++ "y @ " ++ show (y + theight t1) ++ "   [x = " ++ show x ++ "]\n" ++
        showTree (pfx ++ "  ") x y t1 ++
        showTree (pfx ++ "  ") x (y+theight t1) t2


twidth :: Tree -> Double
twidth (Leaf r) = dx r
twidth (Split X _ t1 t2) = twidth t1 + twidth t2
twidth (Split Y _ t1 t2) = twidth t1

theight :: Tree -> Double
theight (Leaf r) = dy r
theight (Split Y _ t1 t2) = theight t1 + theight t2
theight (Split X _ t1 t2) = theight t1

data Point = Point { px, py :: Double }

instance Show Point where
  show p = "(" ++ show (px p) ++ ", " ++ show (py p) ++ ")"

instance Show Rectangle where
  show r = show (dx r) ++ "x" ++ show (dy r)

layoutAt :: Tree -> Point -> [(Point, Rectangle)]
layoutAt (Leaf r) p = [(p, r)]
layoutAt (Split Y _ t1 t2) p = 
    layoutAt t1 p ++ layoutAt t2 (p { py = py p + theight t1 })
layoutAt (Split X _ t1 t2) p = 
    layoutAt t1 p ++ layoutAt t2 (p { px = px p + twidth t1 })

layout t = layoutAt t (Point 0 0)

test = layout . squarify

data Rectangle = Rectangle { dx, dy :: Double }

try :: Rectangle -> ([Area], [Area]) -> Trial
try space (prefix, suffix) = trace ("trying " ++ show space ++ "\n") $
                             Trial (squareness lastr) tree
    where
      tree = Split (long_side space) undefined (fill rectangle1 prefix)
                                                (fill leftover suffix)
      -- try placing prefix on the short side
      (rectangle1, leftover) = place_in (short_side space) (sum prefix) space
      -- how good was the last area in the prefix?
      (lastr, _) = place_in (short_side space) (last prefix) rectangle1

fill :: Rectangle -> [Area] -> Tree
fill _ [] = error $ "there were no areas"
fill space [area] = 
    if dx space * dy space =~= area then Leaf space
    else error $ "rectangle " ++ show space ++ " does not have area " ++ show area
fill space areas = greedy $ map (try space) (splits areas)
  where greedy [] = error "this can't happen -- never made a trial"
        greedy [t] = tree t
        greedy (t1:t2:ts) =
--            trace ("quality " ++ show (quality t1) ++ " vs " ++ show (quality t2)) $
            if quality t2 > quality t1 then greedy (t2:ts)
            else tree t1

-- this version doesn't rely on lazy evaluation
find_strict space (split:splits) = greedy (try space split) splits
  where greedy :: Trial -> [([Area], [Area])] -> Tree
        greedy best [] = tree best
        greedy best (split:splits) =
            let next = try space split
            in  if quality next > quality best then
                    greedy next splits
                else
                    tree best


data Trial = Trial { quality :: Double, tree :: Tree }
instance Show Trial where
  show t = "Q" ++ printf "%.2f" (quality t) ++ " " ++
           show (layoutAt (tree t) (Point 0 0)) ++ "\n" ++
           show (tree t)



splits :: [a] -> [([a], [a])] -- argument has at least two elements
splits as = [(take n as, drop n as) | n <- [1..length as -1]]

type Space = Rectangle

squareness :: Rectangle -> Double --- at most 1; more is better
squareness rect = if dx rect < dy rect then dx rect / dy rect
                  else dy rect / dx rect

place_in :: Axis -> Area -> Space -> (Rectangle, Space)
place_in Y area space = ( Rectangle rwidth (dy space)
                        , Rectangle (dx space - rwidth) (dy space)
                        )
   where rwidth = area / dy space
place_in X area space = ( Rectangle (dx space) rheight
                        , Rectangle (dx space) (dy space - rheight)
                        )
   where rheight = area / dx space

short_side :: Rectangle -> Axis
short_side r = if dx r < dy r then X else Y
         
long_side :: Rectangle -> Axis
long_side r = if dx r < dy r then Y else X
  



squarify :: [Area] -> Tree
squarify areas = fill (Rectangle side side) (reverse $ sort areas)
  where side = sqrt (sum areas)

infix 2 =~=
(=~=) :: Double -> Double -> Bool
x =~= y = abs (x - y) < epsilon
    where epsilon = 0.001
