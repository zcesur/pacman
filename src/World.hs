module World where

import Graphics.Gloss

import Util

numBlocks :: (Int, Int)
numBlocks = (5,5)

grid :: [Box]
grid = makeGrid numBlocks

walls :: [Box]
walls = [pt | pt <- grid, fst pt `elem` fst bds || snd pt `elem` snd bds]
  where
    bds = splitAt 2 $ map ($ grid) $ (.) <$> [fst, snd] <*> [maximum, minimum]

data GameState = Game
    { pacmanLoc :: Point
    , pacmanDir :: Direction
    }
