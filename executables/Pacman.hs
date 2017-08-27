module Main where

import Graphics.Gloss

import Data.Char (toUpper)

import Animation (step)
import Event (handleKeys)
import Rendering (renderWorld, unitRadius)
import Types
import Util

windowPosition :: (Int, Int)
windowPosition = (100, 100)

window :: [Box] -> Display
window walls = InWindow "Pacman" (width, height) windowPosition
  where
    width = 2 * round unitRadius * round (fst nBlocks)
    height = 2 * round unitRadius * round (snd nBlocks)
    nBlocks = 1 `addSV` (maximum walls) `subVV` (minimum walls)

background :: Color
background = black

fps :: Int
fps = 60

initialState :: GameState
initialState = Game { pacmanLoc = (0, 0)
                    , pacmanDir = R }

main :: IO ()
main = do
    window' <- window <$> walls 
    renderWorld' <- renderWorld <$> walls
    step' <- step <$> walls
    play window' background fps initialState renderWorld' handleKeys step'
  where walls = map read <$> lines <$> readFile "assets/map001"
