module Main where

import Data.List 
import World

main = (writeFile "assets/map002" . unlines . map show) walls
  where
    walls = mkWalls (5, 5) 
