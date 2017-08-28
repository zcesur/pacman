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

initialState :: [Agent]
initialState = pacman:clyde:[]
  where
    pacman = Agent { species = Pacperson
                   , position = (0, 0)
                   , direction = R
                   , bufferedDirection = R
                   , velocity = 5 }

    clyde = Agent { species = Ghost
                  , position = (0, 0)
                  , direction = L
                  , bufferedDirection = L
                  , velocity = 5 }

main :: IO ()
main = do
    window' <- window <$> walls 
    renderWorld' <- renderWorld <$> walls
    step' <- step <$> walls
    play window' background fps initialState renderWorld' handleKeys step'
  where walls = map read <$> lines <$> readFile "assets/map001"
