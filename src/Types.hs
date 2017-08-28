module Types where

import Graphics.Gloss.Data.Point

data Species = Pacperson | Ghost

data Agent = Agent
    { species :: Species
    , position :: Point
    , direction :: Direction
    , velocity :: Velocity
    }


data Direction = L | R | U | D

type Velocity = Float
type Box = Point
type Radius = Float
