module Event where

import Graphics.Gloss.Interface.Pure.Game

import World
import Util

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char '0') _ _ _) game =
  game { pacmanLoc = (0, 0) }

handleKeys (EventKey (Char c) _ _ _) game
    | c `elem` ['h', 'a'] = newState L
    | c `elem` ['l', 'd'] = newState R
    | c `elem` ['k', 'w'] = newState U
    | c `elem` ['j', 's'] = newState D
  where
    newState dir = game { pacmanDir = dir }

handleKeys _ game = game

collision :: Box -> Box -> Bool
collision (x, y) b = ptInBox (x+0.5, y+0.5) b ||
                     ptInBox (x-0.5, y-0.5) b

handleWallCollision :: GameState -> GameState
handleWallCollision game = game { pacmanLoc = pt' }
  where
    pt = pacmanLoc game
    dir = pacmanDir game
    pt' = if null collidingWalls
              then pt
              else relocate pt dir (head collidingWalls)

    collidingWalls = filter (collision pt) walls
    
    relocate :: Box -> Direction -> Box -> Box
    relocate (x1, y1) L (x2, y2) = (x2+1, y1)
    relocate (x1, y1) R (x2, y2) = (x2-1, y1)
    relocate (x1, y1) U (x2, y2) = (x1, y2-1)
    relocate (x1, y1) D (x2, y2) = (x1, y2+1)
