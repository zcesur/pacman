module Main where

import Data.List 
import World

main = (writeFile "assets/classic" . unlines . map show) walls
  where
    walls = nub $ leftWalls ++ rightWalls
    leftWalls = map (\(x,y) -> (-x,y)) rightWalls
    rightWalls = map (flip (,) ( 10)) [0..9]
              ++ map (flip (,) (  9)) [0,9]
              ++ map (flip (,) (  8)) [0,2,3,4,6,7,9]
              ++ map (flip (,) (  7)) [9]
              ++ map (flip (,) (  6)) [0,1,2,4,6,7,9]
              ++ map (flip (,) (  5)) [0,4,9]
              ++ map (flip (,) (  4)) [0,2,3,4,6,7,8,9]
              ++ map (flip (,) (  3)) [4,6]
              ++ map (flip (,) (  2)) [1,2,4,6,7,8,9,10]
              ++ map (flip (,) (  1)) [2]
              ++ map (flip (,) (- 0)) [0,1,2,4,6,7,8,9,10]
              ++ map (flip (,) (- 1)) [4,6]
              ++ map (flip (,) (- 2)) [0,1,2,4,6,7,8,9]
              ++ map (flip (,) (- 3)) [0,9]
              ++ map (flip (,) (- 4)) [0,2,3,4,6,7,9]
              ++ map (flip (,) (- 5)) [6,9]
              ++ map (flip (,) (- 6)) [0,1,2,4,6,8,9]
              ++ map (flip (,) (- 7)) [0,4,9]
              ++ map (flip (,) (- 8)) [0,2,3,4,5,6,7,9]
              ++ map (flip (,) (- 9)) [9]
              ++ map (flip (,) (-10)) [0..9]
