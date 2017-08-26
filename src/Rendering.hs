module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import World
import Util

unitRadius :: Float
unitRadius = 10

translatePic :: Point -> Picture -> Picture
translatePic = uncurry translate . (mulSV (2*unitRadius))

renderStatic :: [Box] -> Picture
renderStatic walls = 
    pictures $ map (($ block) . translatePic) walls
  where
    block =  color (dark blue) $ rectangleWire (2*r) (2*r)
    r = unitRadius

renderDynamic :: GameState -> Picture
renderDynamic game =
    pictures [translatePic (pacmanLoc game) $ pacmanCol $ circleSolid unitRadius]
  where
    pacmanCol = color yellow

renderWorld :: [Box] -> GameState -> Picture
renderWorld walls game = pictures [ renderStatic walls
                                  , renderDynamic game
                                  ]
