module Animation where

import System.Random
import Data.List

import Event
import Util
import Types

step :: [Box] -> Float -> [Agent] -> [Agent]
step walls seconds (p:gs) = map fs (p:gs)
  where
    fs = handleWallCollision walls
       . handleGhostCollision gs
       . updatePosition seconds
       . turn walls 0.001

updatePosition :: Float -> Agent -> Agent
updatePosition seconds agent = agent { position = (x', y') }
  where
    (x, y) = position agent
    (vx, vy) = mulSV (velocity agent) $ fromDirection $ direction agent

    x' = x + vx * seconds
    y' = y + vy * seconds

turn :: [Box] -> Float -> Agent -> Agent
turn walls tolerance a
    | dir == bufDir = a
    | canTurn       = a' 
    | otherwise     = a
  where
    a' = case species a of
        Pacperson -> a { position = pos'
                       , direction = bufDir
                       , lastCrossroads = pos'
                       }

        Ghost -> a { position = pos'
                   , direction = bufDir
                   , lastCrossroads = pos'
                   , bufferedDirection = bufDir'
                   , seed = seed'
                   }

    canTurn =
        not (boxAhead `elem` walls) &&
        not (pos' == lastCrossroads a) &&
        case bufDir of
            L -> abs (x-x') < 1 + tolerance && abs (y-y') < tolerance
            R -> abs (x-x') < 1 + tolerance && abs (y-y') < tolerance
            U -> abs (y-y') < 1 + tolerance && abs (x-x') < tolerance
            D -> abs (y-y') < 1 + tolerance && abs (x-x') < tolerance

    dir = direction a
    bufDir = bufferedDirection a
    pos@(x, y) = position a
    pos' = relocate pos bufDir boxAhead
    boxAhead@(x', y') = boxInFrontOf (x, y) bufDir

    (bufDir', seed') = randomL (allDirections \\ [bufDir, neg bufDir]) (seed a)
