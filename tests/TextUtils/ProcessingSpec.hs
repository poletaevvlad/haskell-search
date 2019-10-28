module TextUtils.ProcessingSpec (spec) where

import Test.Hspec
import TextUtils.Processing

spec :: Spec
spec = do
  describe "getExcerpt" $ do
    it "should return full string if no enough words are passed" $ do
      getExcerpt 3 "Lorem ipsum" `shouldBe` "Lorem ipsum"
    it "should return full string if exact number of words is passed" $ do
      getExcerpt 3 "Lorem ipsum dolor" `shouldBe` "Lorem ipsum dolor"
    it "should return part of a string" $ do
      getExcerpt 3 "Lorem ipsum dolor sit amet" `shouldBe` "Lorem ipsum dolor"

