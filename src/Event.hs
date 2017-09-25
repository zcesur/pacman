module Event where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Types
import Util

handleKeys :: Event -> [Agent] -> [Agent]
handleKeys (EventKey (Char c) _ _ _) (p:gs)
    | c == '0'            = (p { position = (0, 0) }):gs
    | c `elem` ['h', 'a'] = buffer L
    | c `elem` ['l', 'd'] = buffer R
    | c `elem` ['k', 'w'] = buffer U
    | c `elem` ['j', 's'] = buffer D
    | otherwise           = (p:gs)
  where
    buffer dir = (p { bufferedDirection = dir }):gs
handleKeys _ as = as

handleGhostCollision :: [Agent] -> Agent -> Agent
handleGhostCollision gs a
    | not ghostCollision = a
    | otherwise          =
        case species a of
            Pacperson -> a { alive = False }
            Ghost     -> a
  where
    ghostCollision = any (collision (position a))
                   . map position
                   . filter ((name a /=) . name) $ gs

handleWallCollision :: [Box] -> Agent -> Agent
handleWallCollision walls a = case species a of
    Pacperson -> a { position = pos' }
    Ghost -> a { position = pos' 
               , bufferedDirection = bufDir'
               , seed = seed' 
               }
  where  
    pos = position a
    dir = direction a
    bufDir = bufferedDirection a
    boxAhead = boxInFrontOf pos dir

    pos'
        | null $ filter (collision pos) walls = pos
        | boxAhead `elem` walls               = pushback pos dir boxAhead
        | otherwise                           = relocate pos dir boxAhead

    (bufDir', seed')
        | boxAhead `elem` walls = randomL candidates (seed a)
        | otherwise             = (bufDir, seed a)

    candidates = let f = (\x -> not $ boxInFrontOf pos' x `elem` walls)
                 in filter f allDirections

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
