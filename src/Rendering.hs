module Rendering where

import Graphics.Gloss

import Types
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

renderDynamic :: [Agent] -> Picture
renderDynamic as = pictures $ map f as
  where
    f a = translatePic (position a) $ color (col a) $ circleSolid unitRadius
    col a = case name a of
        Pacman -> if alive a then yellow else white
        Blinky -> red
        Inky -> cyan
        Pinky -> light red
        Clyde -> orange


renderWorld :: [Box] -> [Agent] -> Picture
renderWorld walls as = pictures [ renderStatic walls
                                , renderDynamic as
                                ]
