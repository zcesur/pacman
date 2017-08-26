module Main where

import Graphics.Gloss

import Data.Char (toUpper)

import World 
import Animation (step)
import Event (handleKeys)
import Rendering (renderWorld, unitRadius)
import Util (Direction (..))

windowPosition :: (Int, Int)
windowPosition = (100, 100)

window :: Display
window = InWindow "Pacman" (width, height) windowPosition
  where
    width = 2 * round unitRadius * fst numBlocks
    height = 2 * round unitRadius * snd numBlocks

background :: Color
background = black

fps :: Int
fps = 60

initialState :: GameState
initialState = Game { pacmanLoc = (0, 0)
                    , pacmanDir = R }

main :: IO ()
main = do
    renderWorld' <- renderWorld <$> walls
    step' <- step <$> walls
    play window background fps initialState renderWorld' handleKeys step'
  where walls = map read <$> lines <$> readFile "assets/map001"
