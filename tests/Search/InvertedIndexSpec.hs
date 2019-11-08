module Search.InvertedIndexSpec(spec) where

import Test.Hspec
import Search.InvertedIndex
import Data.Binary
import Data.ByteString.Lazy


spec :: Spec
spec = do
  describe "InvIndexEntry Binary instance" $ do
    let byteString = pack [0, 0, 0x10, 0x20, 0, 0, 0x30, 0x40]
    it "should serialize index entry" $ do
      encode (InvIndexEntry 0x1020 0x3040) `shouldBe` byteString
    it "should deserialzie into an index entry" $ do
      decode byteString `shouldBe` InvIndexEntry 0x1020 0x3040
  describe "DocIndexEntry Binary instance" $ do
    let byteString = pack [0x10, 0x20, 0x30, 0x40, 0xC0, 0xD0, 0xE0, 0xF0, 0x00, 0x00, 0x00, 0x05]
    it "should serialize docs index entry" $ do
      encode (DocIndexEntry 0x10203040 0xC0D0E0F0 0x00000005) `shouldBe` byteString
    it "should deserialzie into a doc index entry" $ do
      decode byteString `shouldBe` DocIndexEntry 0x10203040 0xC0D0E0F0 0x00000005
