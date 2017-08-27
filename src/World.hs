module World where

mkGrid :: (Int, Int) -> [(Float, Float)]
mkGrid (nb1, nb2) = [(x,y) | x <- xs, y <- ys]
  where
    xs = let bnd = fromIntegral $ nb1 `div` 2 in [-bnd..bnd]
    ys = let bnd = fromIntegral $ nb2 `div` 2 in [-bnd..bnd]

mkWalls :: (Int, Int) -> [(Float, Float)]
mkWalls nBlocks = [pt | pt <- grid, fst pt `elem` fst bds || snd pt `elem` snd bds]
  where
    bds = splitAt 2 $ map ($ grid) $ (.) <$> [fst, snd] <*> [maximum, minimum]
    grid = mkGrid nBlocks
