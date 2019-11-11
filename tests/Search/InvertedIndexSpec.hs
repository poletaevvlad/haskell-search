module Search.InvertedIndexSpec(spec) where

import Prelude hiding (writeFile)
import Test.Hspec
import Search.InvertedIndex
import Data.Binary
import Data.ByteString.Lazy hiding (map)
import System.IO.Temp (withSystemTempDirectory)


withIndex :: (InvertedIndex -> IO a) -> IO a
withIndex callback =
  withSystemTempDirectory "index" (\path -> do
    writeFile (path ++ "/docs.index") docIndex
    writeFile (path ++ "/positions.index") posIndex
    writeFile (path ++ "/inv.index") invIndex
    loadIndex path >>= callback)
  where
    posIndex = packWords32 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]
    docIndex = packWords32 [7, 1, 4,   13, 5, 6,   8, 11, 4,   14, 15, 2]
    invIndex = packWords32 [0, 0,   0, 1,   1, 3]

    packWords32 :: [Word32] -> ByteString
    packWords32 = mconcat . (map encode)


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

  describe "performTermSearch" $ do
    it "should return empty if fine not in index" $ do
      withIndex (\idx -> do
        res <- performTermSearch 0 idx
        res `shouldBe` [])
    it "should return results on term search" $ do
      withIndex (\idx -> do
        res <- performTermSearch 2 idx
        map dieDocId res `shouldBe` [13, 8, 14]
        map diePosOffset res `shouldBe` [5, 11, 15]
        map diePosCount res `shouldBe` [6, 4, 2])

  describe "getPositions" $ do
    it "should return indices" $ do
      withIndex (\idx -> do
        res <- getPositions (DocIndexEntry 7 1 4) idx
        res `shouldBe` [2, 3, 4, 5])
