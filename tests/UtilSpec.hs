module UtilSpec where

import Test.Hspec
import Util
import Types

spec :: Spec
spec = do
  describe "collision" $ do
    it "can determine if 2 disjoint boxes are overlapping" $ do
      collision (0, 0) (2, 2) `shouldBe` False
      collision (0, 0) (-2, 2) `shouldBe` False
      collision (0, 0) (2, -2) `shouldBe` False
      collision (0, 0) (-2, -2) `shouldBe` False

    it "can determine if 2 boxes with mutual corners are overlapping" $ do
      collision (0, 0) (1, 1) `shouldBe` False
      collision (0, 0) (-1, 1) `shouldBe` False
      collision (0, 0) (1, -1) `shouldBe` False
      collision (0, 0) (-1, -1) `shouldBe` False

    it "can determine if 2 boxes with mutual edges are overlapping" $ do
      collision (0, 0) (1, 0) `shouldBe` False
      collision (0, 0) (0, 1) `shouldBe` False
      collision (0, 0) (-1, 0) `shouldBe` False
      collision (0, 0) (0, -1) `shouldBe` False

    it "can determine if 2 boxes with mutual interiors are overlapping" $ do
      collision (0, 0) (0.99, 0.99) `shouldBe` True
      collision (0, 0) (-0.99, 0.99) `shouldBe` True
      collision (0, 0) (0.99, -0.99) `shouldBe` True
      collision (0, 0) (-0.99, -0.99) `shouldBe` True
      collision (0, 0) (0.99, 0) `shouldBe` True
      collision (0, 0) (0, 0.99) `shouldBe` True
      collision (0, 0) (-0.99, 0) `shouldBe` True
      collision (0, 0) (0, -0.99) `shouldBe` True

  describe "boxInFrontOf" $ do
    it "can correctly determine the box ahead if the edges are touching" $ do
      let bsU = [(-0.51, -1), (-0.49, -1), (0, -1), (0.49, -1), (0.51, -1)]
      let bsU' = [(-1, 0), (0, 0), (0, 0), (0, 0), (1, 0)]

      let bsL = [(1, -0.51), (1, -0.49), (1, 0), (1, 0.49), (1, 0.51)]
      let bsL' = [(0, -1), (0, 0), (0, 0), (0, 0), (0, 1)]
      
      let bsD = [(-0.51, 1), (-0.49, 1), (0, 1), (0.49, 1), (0.51, 1)]
      let bsD' = [(-1, 0), (0, 0), (0, 0), (0, 0), (1, 0)]
      
      let bsR = [(-1, -0.51), (-1, -0.49), (-1, 0), (-1, 0.49), (-1, 0.51)]
      let bsR' = [(0, -1), (0, 0), (0, 0), (0, 0), (0, 1)]

      boxInFrontOf <$> bsU <*> [U] `shouldBe` bsU'
      boxInFrontOf <$> bsL <*> [L] `shouldBe` bsL'
      boxInFrontOf <$> bsD <*> [D] `shouldBe` bsD'
      boxInFrontOf <$> bsR <*> [R] `shouldBe` bsR'

    it "can correctly determine the box ahead if overlapping" $ do
      let bsU = [(-0.51, -0.99), (-0.49, -0.99), (0, -0.99), (0.49, -0.99), (0.51, -0.99)]
      let bsU' = [(-1, 0), (0, 0), (0, 0), (0, 0), (1, 0)]

      let bsL = [(0.99, -0.51), (0.99, -0.49), (0.99, 0), (0.99, 0.49), (0.99, 0.51)]
      let bsL' = [(0, -1), (0, 0), (0, 0), (0, 0), (0, 1)]
      
      let bsD = [(-0.51, 0.99), (-0.49, 0.99), (0, 0.99), (0.49, 0.99), (0.51, 0.99)]
      let bsD' = [(-1, 0), (0, 0), (0, 0), (0, 0), (1, 0)]
      
      let bsR = [(-0.99, -0.51), (-0.99, -0.49), (-0.99, 0), (-0.99, 0.49), (-0.99, 0.51)]
      let bsR' = [(0, -1), (0, 0), (0, 0), (0, 0), (0, 1)]
      
      boxInFrontOf <$> bsU <*> [U] `shouldBe` bsU'
      boxInFrontOf <$> bsL <*> [L] `shouldBe` bsL'
      boxInFrontOf <$> bsD <*> [D] `shouldBe` bsD'
      boxInFrontOf <$> bsR <*> [R] `shouldBe` bsR'
