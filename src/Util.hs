module Util where

import Graphics.Gloss

import System.Random

import Types

-- | Given a direction, return a unit-length vector representing the
-- direction.
fromDirection :: Direction -> Point
fromDirection x = case x of
    L -> (-1,  0)
    R -> ( 1,  0)
    U -> ( 0,  1)
    D -> ( 0, -1)

-- | Given a direction, return the negative direction, i.e., one that
-- is rotated 180 degrees w.r.t the original.
neg :: Direction -> Direction
neg x = case x of
    L -> R
    R -> L
    U -> D
    D -> U

-- | Return all directions.
allDirections :: [Direction]
allDirections = [minBound :: Direction ..]

-- | Given a box and direction, return the box ahead.
boxInFrontOf :: Box -> Direction -> Box
boxInFrontOf (x, y) L = (ceiling' (x-1), round' y)
boxInFrontOf (x, y) R = (floor' (x+1), round' y)
boxInFrontOf (x, y) U = (round' x, floor' (y+1))
boxInFrontOf (x, y) D = (round' x, ceiling' (y-1))

-- | Check if two boxes are overlapping, where overlapping is defined as
-- the existence of some point that is an interior point of both boxes.
collision :: Box -> Box -> Bool
collision (x1, y1) (x2, y2) = abs (x1-x2) < 1 && abs (y1-y2) < 1 

-- | Given a list and an RNG g, return an element from the list chosen
-- uniformly at random along with a new generator. Note that this function
-- is unsafe in that it throws an error if it is applied on an empty list. 
randomL :: (RandomGen g, Random a) => [a] -> g -> (a, g)
randomL [] _ = error "Cannot take a random element from an empty list!"
randomL xs seed = (\(i, s) -> (xs !! i, s)) $ randomR (0, length xs - 1) seed

-- | Add 2 points element-wise.
addVV :: Point -> Point -> Point
(x1, y1) `addVV` (x2, y2) = (x1+x2, y1+y2)
infixl 6 `addVV`

-- | Add a scalar to both components of a point.
addSV :: Float -> Point -> Point
a `addSV` (x,y) = (a+x, a+y)
infixl 6 `addSV`

-- | Subtract 2 points element-wise.
subVV :: Point -> Point -> Point
(x1, y1) `subVV` (x2, y2) = (x1-x2, y1-y2)
infixl 6 `subVV`

-- | Subtract a scalar from both components of a point.
subSV :: Float -> Point -> Point
a `subSV` (x,y) = (a-x, a-y)
infixl 6 `subSV`

-- | Multiply 2 points element-wise (a.k.a the Hadamard product).
mulVV :: Point -> Point -> Point
(x1, y1) `mulVV` (x2, y2) = (x1*x2, y1*y2)
infixl 7 `mulVV`

-- | Multiply both components of a point by a scalar (i.e. scale)
mulSV :: Float -> Point -> Point
a `mulSV` (x,y) = (a*x, a*y)
infixl 7 `mulSV`

-- | Non-generic 'ceiling' used for convenience
ceiling' :: (RealFrac a) => a -> Float
ceiling' = fromIntegral . ceiling

-- | Non-generic 'floor' used for convenience
floor' :: (RealFrac a) => a -> Float
floor' = fromIntegral . floor

-- | Non-generic 'round' used for convenience
round' :: (RealFrac a) => a -> Float
round' = fromIntegral . round
