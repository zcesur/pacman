module UtilSpec where

import Data.List
import Test.Hspec
import Util

spec :: Spec
spec = do
    it "can make a grid with the given number of blocks in each axis" $ do
      let xs = makeGrid (3,3)
      let ys = [ (-1,1),  (0,1),  (1,1)
               , (-1,0),  (0,0),  (1,0)
               , (-1,-1), (0,-1), (1,-1)
               ]

      null (xs \\ ys) && null (ys \\ xs) `shouldBe` True
