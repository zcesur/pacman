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

boxInFront :: Box -> Direction -> Box
boxInFront (x, y) L = (ceiling' (x-1), round' y)
boxInFront (x, y) R = (floor' (x+1), round' y)
boxInFront (x, y) U = (round' x, floor' (y+1))
boxInFront (x, y) D = (round' x, ceiling' (y-1))

ceiling' :: (RealFrac a) => a -> Float
ceiling' = fromIntegral . ceiling

floor' :: (RealFrac a) => a -> Float
floor' = fromIntegral . floor

round' :: (RealFrac a) => a -> Float
round' = fromIntegral . round
