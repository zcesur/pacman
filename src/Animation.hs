module Animation where

import Event
import Util
import Types

step :: [Box] -> Float -> [Agent] -> [Agent]
step walls seconds = map fs
  where
    fs = handleWallCollision walls
       . updatePosition seconds

updatePosition :: Float -> Agent -> Agent
updatePosition seconds agent = agent { position = (x', y') }
  where
    (x, y) = position agent
    (vx, vy) = mulSV (velocity agent) $ fromDirection $ direction agent

    x' = x + vx * seconds
    y' = y + vy * seconds
