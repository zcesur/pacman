module World where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

toCoords :: Point -> Point
toCoords = mulSV (2*unitRadius)

unitRadius :: Float
unitRadius = 10

numBlocks :: (Int, Int)
numBlocks = (19,19)

makeGrid :: (Int, Int) -> [Point]
makeGrid (nb1, nb2) = [(x,y) | x <- xs, y <- ys]
  where
    xs = let bnd = fromIntegral $ nb1 `div` 2 in [-bnd..bnd]
    ys = let bnd = fromIntegral $ nb2 `div` 2 in [-bnd..bnd]

grid :: [Point]
grid = makeGrid numBlocks

walls :: [Point]
walls = [pt | pt <- grid, fst pt `elem` fst bds || snd pt `elem` snd bds]
  where
    bds = splitAt 2 $ map ($ grid) $ (.) <$> [fst, snd] <*> [maximum, minimum]

wallsRendered = pictures $ map (($ block) . uncurry translate . toCoords) walls
  where
    block =  color (dark blue) $ rectangleWire (2*r) (2*r)
    r = unitRadius
