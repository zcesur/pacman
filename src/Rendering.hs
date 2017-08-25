module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import World
import Util

unitRadius :: Float
unitRadius = 10

toCoords :: Point -> Point
toCoords = mulSV (2*unitRadius)

renderStatic :: [Box] -> Picture
renderStatic walls = 
    pictures $ map (($ block) . uncurry translate . toCoords) walls
  where
    block =  color (dark blue) $ rectangleWire (2*r) (2*r)
    r = unitRadius

renderDynamic :: GameState -> Picture
renderDynamic game =
    pictures [(uncurry translate . toCoords) (pacmanLoc game) $ pacmanCol $ circleSolid unitRadius]
  where
    pacmanCol = color yellow

prerendered :: Picture
prerendered = renderStatic walls

render :: GameState -> Picture
render game = pictures [ prerendered
                       , renderDynamic game
                       ]
