module Main where

import Graphics.Gloss

import Data.Char (toUpper)
import System.Random

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
initialState = pacman:clyde:blinky:[]
  where
    pacman = Agent { species = Pacperson
                   , position = (5, 5)
                   , lastCrossroads = (0, 0)
                   , direction = R
                   , bufferedDirection = R
                   , velocity = 5
                   , seed = mkStdGen 0
                   , alive = True
                   , name = Pacman
                   }

    clyde = Agent { species = Ghost
                  , position = (5, -5)
                  , lastCrossroads = (0, 0)
                  , direction = U
                  , bufferedDirection = R
                  , velocity = 5
                  , seed = mkStdGen 4
                  , alive = True
                  , name = Clyde
                  }

    blinky = Agent { species = Ghost
                   , position = (-5, 5)
                   , lastCrossroads = (0, 0)
                   , direction = U
                   , bufferedDirection = R
                   , velocity = 5
                   , seed = mkStdGen 42
                   , alive = True
                   , name = Blinky
                   }

main :: IO ()
main = do
    window' <- window <$> walls 
    renderWorld' <- renderWorld <$> walls
    step' <- step <$> walls
    play window' background fps initialState renderWorld' handleKeys step'
  where walls = map read <$> lines <$> readFile "assets/map001"
