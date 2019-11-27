module TextUtils.ListUtilsSpec(spec) where

import Test.Hspec
import TextUtils.ListUtils


spec :: Spec
spec = do
  describe "minTermDistance" $ do
    it "should return minimum distance" $ do
      minTermDistance [1, 7, 18, 24, 25, 33] [4, 16, 27] `shouldBe` 2

  describe "minQueryDistance" $ do
    it "should return distance between elemented" $ do
      minQueryDistance "abcdef" 'a' 'd' `shouldBe` 3

    it "should return the minimum distance between elemented" $ do
      minQueryDistance "abcdcccacd" 'a' 'd' `shouldBe` 2

  describe "pairs" $ do
    it "should return empty if input is empty" $ do
      pairs ([] :: [Int]) `shouldBe` []
    it "should return empty if input is of length 1" $ do
      pairs ([1] :: [Int]) `shouldBe` []
    it "should return single pair if two-element list is passed" $ do
      pairs ([1, 2] :: [Int]) `shouldBe` [(1, 2)]
    it "should return all posible pairs" $ do
      pairs ([1, 2, 3, 4] :: [Int]) `shouldBe` [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]

