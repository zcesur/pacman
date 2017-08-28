module Event where

import Graphics.Gloss.Interface.Pure.Game

import Types
import Util

handleKeys :: Event -> [Agent] -> [Agent]
handleKeys (EventKey (Char c) _ _ _) (p:gs)
    | c == '0'            = (p { position = (0, 0) }):gs
    | c `elem` ['h', 'a'] = turn L
    | c `elem` ['l', 'd'] = turn R
    | c `elem` ['k', 'w'] = turn U
    | c `elem` ['j', 's'] = turn D
    | otherwise           = (p:gs)
  where
    turn dir = (p { direction = dir }):gs
handleKeys _ as = as

handleWallCollision :: [Box] -> Agent -> Agent
handleWallCollision walls a = a { position = pos' }
  where  
    pos = position a
    dir = direction a

    pos'
        | null $ filter (collision pos) walls = pos
        | boxAhead `elem` walls               = pushback pos dir boxAhead
        | otherwise                           = relocate pos dir boxAhead
      where boxAhead = boxInFrontOf pos dir

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
