module TextUtils.ProcessingSpec (spec) where

import Test.Hspec
import TextUtils.Processing
import qualified Data.ByteString.Lazy as BS


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
      splitWords "shouldn't don't" `shouldBe` ["shouldn't", "don't"]
    it "should ignore trailing and leading spaces and punctuation marks" $ do
      splitWords "  hello --" `shouldBe` ["hello"]
    it "should correctly split text with newlines" $ do
      splitWords "hello\nworld" `shouldBe` ["hello", "world"]

  describe "getPositions" $ do
    it "should return empty array if the input is empty" $ do
      getPositions "" `shouldBe` []
    it "should extract positions from an array" $ do
      let expected = [('a', [0, 1, 4]), ('b', [2, 7]), ('c', [3, 5, 6])]
      getPositions "aabcaccb" `shouldBe` expected

  describe "filterChars" $ do
    it "should convert to lowercase and replace punctuations with spaces" $ do
      filterChars "Hello, World!" `shouldBe` "hello  world "
    it "should drop apostrophy" $ do
      filterChars "a'b'c'defg" `shouldBe` "abcdefg"

  describe "hexEncode" $ do
    it "should convert empty bytestring to empty string" $ do
      hexEncode BS.empty `shouldBe` ""
    it "should convert multiple numbers" $ do
      hexEncode (BS.pack [0x45, 0x79, 0x61]) `shouldBe` "457961"
    it "should add leading zeros" $ do
      hexEncode (BS.pack [0x00, 0x05, 0x00]) `shouldBe` "000500"

  describe "hexDecode" $ do
    it "should convert empty string to empty bytestring" $ do
      hexDecode "" `shouldBe` Just BS.empty
    it "should parse hex string" $ do
      hexDecode "0123456789abcdefABCDEF" `shouldBe` Just (BS.pack [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xab, 0xcd, 0xef])
    it "should fail if unknown char is found" $ do
      hexDecode "0123m567" `shouldBe` Nothing
    it "should fail if the string is of odd length" $ do
      hexDecode "0123567" `shouldBe` Nothing
