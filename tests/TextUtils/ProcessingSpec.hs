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

  describe "splitWords" $ do
    it "should return null list if the string is empty" $ do
      splitWords "" `shouldBe` []
    it "should remove spaces and punctuation"  $ do
      splitWords "hello, world" `shouldBe` ["hello", "world"]
    it "should preserve apostrophy" $ do
      splitWords "shouldn't don't" `shouldBe` ["shouldnt", "dont"]
    it "should ignore trailing and leading spaces and punctuation marks" $ do
      splitWords "  hello --" `shouldBe` ["hello"]
    it "should correctly split text with newlines" $ do
      splitWords "hello\nworld" `shouldBe` ["hello", "world"]
