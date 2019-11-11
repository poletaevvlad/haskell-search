module TextUtils.FileUtilsSpec(spec) where

import Test.Hspec
import TextUtils.FileUtils
import Data.ByteString
import System.IO
import System.IO.Temp (withSystemTempFile)
import Data.Word


spec :: Spec
spec = do
  describe "loadFromFile" $ do
    it "should read from file" $ do
      res <- withSystemTempFile "tmp" $ \_ handle -> do
        hSetBinaryMode handle True
        hPut handle $ pack [0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80, 0x90]
        hSeek handle AbsoluteSeek 0
        loadFromFile handle 2 4 :: IO [Word16]
      res `shouldBe` [0x1020, 0x3040, 0x5060, 0x7080]
    it "should return early if string is to short" $ do
      res <- withSystemTempFile "tmp" $ \_ handle -> do
        hSetBinaryMode handle True
        hPut handle $ pack [0x10, 0x20, 0x30, 0x40, 0x50]
        hSeek handle AbsoluteSeek 0
        loadFromFile handle 2 4 :: IO [Word16]
      res `shouldBe` [0x1020, 0x3040]
