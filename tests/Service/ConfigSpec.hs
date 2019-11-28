module Service.ConfigSpec(spec) where

import Test.Hspec
import Service.Config
import Data.Text


spec :: Spec
spec = do
  describe "parsePort" $ do
    it "should not accept non-number" $ do
      (parsePort $ pack "abc") `shouldBe` Left "Port number must be an integer"
    it "should not accept string ending with non-number" $ do
      (parsePort $ pack "8080abc") `shouldBe` Left "Port number must be an integer"
    it "should not accept empty string" $ do
      (parsePort $ pack "") `shouldBe` Left "Port number must be an integer"
    it "should not accept negative port number" $ do
      (parsePort $ pack "-15") `shouldBe` Left "Port number must be in range 1..65535"
    it "should not accept too large number" $ do
      (parsePort $ pack "8000000") `shouldBe` Left "Port number must be in range 1..65535"
    it "should accept port number" $ do
      (parsePort $ pack "8080") `shouldBe` Right 8080
