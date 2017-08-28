module Animation where

import Event
import Util
import Types

step :: [Box] -> Float -> [Agent] -> [Agent]
step walls seconds = map fs
  where
    fs = handleWallCollision walls
       . updatePosition seconds
       . turn walls 0.0000001

updatePosition :: Float -> Agent -> Agent
updatePosition seconds agent = agent { position = (x', y') }
  where
    (x, y) = position agent
    (vx, vy) = mulSV (velocity agent) $ fromDirection $ direction agent

    x' = x + vx * seconds
    y' = y + vy * seconds

turn :: [Box] -> Float -> Agent -> Agent
turn walls tolerance a
    | direction a == dir = a
    | canTurn            = a { position = relocate (x, y) dir (x', y')
                             , direction = dir }
    | otherwise          = a
  where
    canTurn = not ((x', y') `elem` walls) && case dir of
        L -> abs (x-x') < 1 + tolerance
        R -> abs (x-x') < 1 + tolerance
        U -> abs (y-y') < 1 + tolerance
        D -> abs (y-y') < 1 + tolerance
    dir = bufferedDirection a
    (x, y) = position a
    (x', y') = boxInFrontOf (x, y) dir
