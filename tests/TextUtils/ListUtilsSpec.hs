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

