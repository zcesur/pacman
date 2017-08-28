module Util where

import Graphics.Gloss

import Types

fromDirection :: Direction -> Point
fromDirection L = (-1,  0)
fromDirection R = ( 1,  0)
fromDirection U = ( 0,  1)
fromDirection D = ( 0, -1)

boxInFrontOf :: Box -> Direction -> Box
boxInFrontOf (x, y) L = (ceiling' (x-1), round' y)
boxInFrontOf (x, y) R = (floor' (x+1), round' y)
boxInFrontOf (x, y) U = (round' x, floor' (y+1))
boxInFrontOf (x, y) D = (round' x, ceiling' (y-1))

-- | Check if two boxes are overlapping
collision :: Box -> Box -> Bool
collision (x1, y1) (x2, y2) = abs (x1-x2) < 1 && abs (y1-y2) < 1 

addVV :: Point -> Point -> Point
(x1, y1) `addVV` (x2, y2) = (x1+x2, y1+y2)
infixl 6 `addVV`

addSV :: Float -> Point -> Point
a `addSV` (x,y) = (a+x, a+y)
infixl 6 `addSV`

subVV :: Point -> Point -> Point
(x1, y1) `subVV` (x2, y2) = (x1-x2, y1-y2)
infixl 6 `subVV`

subSV :: Float -> Point -> Point
a `subSV` (x,y) = (a-x, a-y)
infixl 6 `subSV`

mulVV :: Point -> Point -> Point
(x1, y1) `mulVV` (x2, y2) = (x1*x2, y1*y2)
infixl 7 `mulVV`

mulSV :: Float -> Point -> Point
a `mulSV` (x,y) = (a*x, a*y)
infixl 7 `mulSV`

ceiling' :: (RealFrac a) => a -> Float
ceiling' = fromIntegral . ceiling

floor' :: (RealFrac a) => a -> Float
floor' = fromIntegral . floor

round' :: (RealFrac a) => a -> Float
round' = fromIntegral . round
