module Types where

import System.Random
import Graphics.Gloss.Data.Point

data Species = Pacperson | Ghost

data Agent = Agent
    { species :: Species
    , position :: Point
    , lastCrossroads :: Point
    , direction :: Direction
    , bufferedDirection :: Direction
    , velocity :: Velocity
    , seed :: StdGen
    , alive :: Bool
    , name :: Name
    }

data Name = Pacman | Blinky | Inky | Pinky | Clyde deriving Eq

data Direction = L | R | U | D deriving (Eq, Show, Bounded, Enum)

-- This implementation is due to Daniel Fischer and Joachim Breitner,
-- found at https://stackoverflow.com/a/11811809/5829427
instance Random Direction where
    random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

type Velocity = Float
type Box = Point
type Radius = Float
