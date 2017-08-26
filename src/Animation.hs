module Animation where

import Graphics.Gloss.Data.Vector

import World
import Event
import Util

step :: [Box] -> Float -> GameState -> GameState
step walls seconds = handleWallCollision walls . move seconds

move :: Float -> GameState -> GameState
move seconds game = game { pacmanLoc = (x', y') }
  where
    (x, y) = pacmanLoc game
    (vx, vy) = mulSV 5 $ fromDirection $ pacmanDir game

    x' = x + vx * seconds
    y' = y + vy * seconds
