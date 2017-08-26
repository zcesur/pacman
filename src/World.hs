module World where

import Graphics.Gloss

import Data.List

import Util

numBlocks :: (Int, Int)
numBlocks = (13, 13)

grid :: [Box]
grid = makeGrid numBlocks

outerWalls :: [Box]
outerWalls = [pt | pt <- grid, fst pt `elem` fst bds || snd pt `elem` snd bds]
  where
    bds = splitAt 2 $ map ($ grid) $ (.) <$> [fst, snd] <*> [maximum, minimum]

innerWalls :: [Box]
innerWalls = [pt | pt <- makeGrid (9, 9), fst pt `elem` fst bds || snd pt `elem` snd bds]
  where
    bds = splitAt 2 $ map ($ makeGrid (9, 9)) $ (.) <$> [fst, snd] <*> [maximum, minimum]

walls :: [Box]
walls = (outerWalls ++ innerWalls) \\ [(-4, 0), (4, 0), (0, -4), (0, 4)]

data GameState = Game
    { pacmanLoc :: Point
    , pacmanDir :: Direction
    }
