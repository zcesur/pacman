module Event where

import Graphics.Gloss.Interface.Pure.Game

import World
import Util

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char '0') _ _ _) game =
  game { pacmanLoc = (0, 0) }

handleKeys (EventKey (Char c) _ _ _) game
    | c `elem` ['h', 'a'] = turn L
    | c `elem` ['l', 'd'] = turn R
    | c `elem` ['k', 'w'] = turn U
    | c `elem` ['j', 's'] = turn D
  where
    turn dir = game { pacmanDir = dir }

handleKeys _ game = game

-- | Check if two boxes are overlapping
collision :: Box -> Box -> Bool
collision (x1, y1) (x2, y2) = abs (x1-x2) < 1 && abs (y1-y2) < 1 

handleWallCollision :: GameState -> GameState
handleWallCollision game = game { pacmanLoc = loc' }
  where
    loc = pacmanLoc game
    dir = pacmanDir game

    loc'
        | null $ filter (collision loc) walls = loc
        | boxAhead `elem` walls               = pushback loc dir boxAhead
        | otherwise                           = relocate loc dir boxAhead
      where boxAhead = boxInFront loc dir

    pushback :: Box -> Direction -> Box -> Box
    pushback (x1, y1) d (x2, y2) =
        case d of 
            L -> (x2+1, y1)
            R -> (x2-1, y1)
            U -> (x1, y2-1)
            D -> (x1, y2+1)

    relocate :: Box -> Direction -> Box -> Box
    relocate (x1, y1) d (x2, y2) =
        case d of 
            L -> (x1, y2)
            R -> (x1, y2)
            U -> (x2, y1)
            D -> (x2, y1)
