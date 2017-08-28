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
    f a = case species a of
        Pacperson -> translatePic (position a) $ color yellow $ circleSolid unitRadius
        Ghost -> translatePic (position a) $ color blue $ circleSolid unitRadius

renderWorld :: [Box] -> [Agent] -> Picture
renderWorld walls as = pictures [ renderStatic walls
                                , renderDynamic as
                                ]
