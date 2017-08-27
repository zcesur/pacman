module Main where

import Data.List 
import World

main = (writeFile "assets/map001" . unlines . map show) walls
  where
    walls = (mkWalls (13, 13) ++ mkWalls (9, 9)) \\ [(-4, 0), (4, 0), (0, -4), (0, 4)] 
