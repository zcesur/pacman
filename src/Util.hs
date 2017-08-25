module Util where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

data Direction = L | R | U | D

fromDirection :: Direction -> Vector 
fromDirection L = (-1,  0)
fromDirection R = ( 1,  0)
fromDirection U = ( 0,  1)
fromDirection D = ( 0, -1)

type Box = Point
type Radius = Float

makeGrid :: (Int, Int) -> [Box]
makeGrid (nb1, nb2) = [(x,y) | x <- xs, y <- ys]
  where
    xs = let bnd = fromIntegral $ nb1 `div` 2 in [-bnd..bnd]
    ys = let bnd = fromIntegral $ nb2 `div` 2 in [-bnd..bnd]

ptInBox :: Point -> Box -> Bool
ptInBox (x1,y1) (x2,y2) = y2 - 0.5 < y1 && y1 < y2 + 0.5 &&
                          x2 - 0.5 < x1 && x1 < x2 + 0.5
