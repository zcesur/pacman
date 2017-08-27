module Animation where

import Event
import Util
import Types

step :: [Box] -> Float -> GameState -> GameState
step walls seconds = handleWallCollision walls . move seconds

move :: Float -> GameState -> GameState
move seconds game = game { pacmanLoc = (x', y') }
  where
    (x, y) = pacmanLoc game
    (vx, vy) = mulSV 5 $ fromDirection $ pacmanDir game

    x' = x + vx * seconds
    y' = y + vy * seconds
