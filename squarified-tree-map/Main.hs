module Main
where

import Data.Colour
import Data.Colour.Names
import System.Environment

import Squarified
import Tikz

spectrum :: [Colour Double]
spectrum = take 7 [blend factor blue white | factor <- factors]
  where factors = 0.8 : map (/1.1) factors

data Problem = Problem { width, height :: Int
                       , areas :: [Double]
                       }

solve :: Problem -> Tree
solve p = fill (Rectangle (fromIntegral $ width p) (fromIntegral $ height p)) (areas p)

trials :: Problem -> [Trial]
trials p =
    map (try (Rectangle (fromIntegral $ width p) (fromIntegral $ height p))) (splits $ areas p)

main :: IO ()
main = do -- putStrLn $ show spectrum
          args <- getArgs
          putStrLn "\\documentclass{article}"
          putStrLn "\\usepackage{tikz}"
          putStrLn "\\begin{document}"
          putStrLn "\\begin{center}"
          mapM_ (putStrLn . renderTrial spectrum) (trials $ problem args)
          putStrLn "\\end{center}"
          putStrLn "\\end{document}"
    where problem (w : h : a : as) =
              let canvas_area = read w * read h
                  canvas_area :: Double
                  unnormalized = map read (a : as)
                  total = sum unnormalized
              in  Problem { width = read w, height = read h
                          , areas = [a * canvas_area / total | a <- unnormalized]
                          }
          problem _ = error $ "Usage: $0 width height area ..."

          renderTrial sp t = render sp (tree t) ++ "\n\nQuality " ++ show (quality t) ++
                         "\n\n\\bigskip\n\n"
