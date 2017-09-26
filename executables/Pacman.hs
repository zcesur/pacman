module Main where

import Graphics.Gloss

import Data.Char (toUpper)
import System.Random

import Animation (step)
import Event (handleKeys)
import Rendering (renderWorld, unitRadius)
import Types
import Util

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 60

initialState :: [Agent]
initialState = pacman:blinky:inky:pinky:clyde:[]
  where
    pacman = Agent { species = Pacperson
                   , position = (0, -5)
                   , lastCrossroads = (0, 0)
                   , direction = L
                   , bufferedDirection = L
                   , velocity = 5
                   , seed = mkStdGen 0
                   , alive = True
                   , name = Pacman
                   }

    blinky = Agent { species = Ghost
                   , position = (0, 3)
                   , lastCrossroads = (0, 0)
                   , direction = U
                   , bufferedDirection = R
                   , velocity = 5
                   , seed = mkStdGen 1
                   , alive = True
                   , name = Blinky
                   }
    
    inky = Agent { species = Ghost
                 , position = (-1, 1)
                 , lastCrossroads = (0, 0)
                 , direction = U
                 , bufferedDirection = R
                 , velocity = 5
                 , seed = mkStdGen 2
                 , alive = True
                 , name = Inky
                 }
   
    pinky = Agent { species = Ghost
                  , position = (0, 1)
                  , lastCrossroads = (0, 0)
                  , direction = U
                  , bufferedDirection = R
                  , velocity = 5
                  , seed = mkStdGen 3
                  , alive = True
                  , name = Pinky
                  }
    
    clyde = Agent { species = Ghost
                  , position = (1, 1)
                  , lastCrossroads = (0, 0)
                  , direction = U
                  , bufferedDirection = R
                  , velocity = 5
                  , seed = mkStdGen 4
                  , alive = True
                  , name = Clyde
                  }  

main :: IO ()
main = do
    renderWorld' <- renderWorld <$> walls
    step' <- step <$> walls
    play window background fps initialState renderWorld' handleKeys step'
  where walls = map read <$> lines <$> readFile "assets/classic"
