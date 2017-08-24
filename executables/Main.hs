module Main where

import Graphics.Gloss
import World

windowPosition :: (Int, Int)
windowPosition = (100, 100)

window :: Display
window = InWindow "Pacman" (width, height) windowPosition
  where
    width = 2 * round unitRadius * fst numBlocks
    height = 2 * round unitRadius * snd numBlocks

background :: Color
background = black

drawing :: Picture
drawing = pictures
  [ translate 0 0 $ color pacmanColor $ circleSolid unitRadius
  , wallsRendered
  ]
  where
    pacmanColor = yellow

main :: IO ()
main = display window background drawing 
