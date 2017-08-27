module Types where

import Graphics.Gloss.Data.Point

data GameState = Game
    { pacmanLoc :: Point
    , pacmanDir :: Direction
    }

data Direction = L | R | U | D

type Box = Point
type Radius = Float


