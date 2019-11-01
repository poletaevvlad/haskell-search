module Search.TermIndexSpec(spec) where

import Prelude hiding (lookup)
import Test.Hspec
import Search.TermIndex
import Control.Monad.State


spec :: Spec
spec = do
  describe "add & requestId" $ do
    context "when using State Monad interface" $ do
      it "should add new element" $ do
        evalState (do
          add "hello" 1
          add "world" 4
          i1 <- getId "hello"
          i2 <- getId "world"
          i3 <- getId "unknown"
          return (i1, i2, i3)
          ) new `shouldBe` (Just 1, Just 4, Nothing)
      it "should keep track of the next id" $ do
        evalState (do
          i1 <- requestId "hello"
          i2 <- requestId "world"
          add "new" 5
          i3 <- requestId "word"
          return (i1, i2, i3)
          ) new `shouldBe` (0, 1, 6)
      it "should return id from previous request" $ do
        evalState (do
          i1 <- requestId "hello"
          i2 <- requestId "hello"
          return (i1, i2)
          ) new `shouldBe` (0, 0)
      it "should return id from previous add" $ do
        evalState (do
          add "hello" 5
          i2 <- requestId "hello"
          return (i2)
          ) new `shouldBe` 5

    context "using non-monadic interface" $ do
      it "should add elements" $ do
        let index = add' "another" 3 $ add' "word" 5 new
        lookup "another" index `shouldBe` Just 3
        lookup "word" index `shouldBe` Just 5
        lookup "non-existant" index `shouldBe` Nothing

      it "should request id for elements" $ do
        let index = new
        let (i1, index2) = requestId' "hello" index
        let (i2, index3) = requestId' "world" index2
        let (i3, _) = requestId' "hello" index3
        i1 `shouldBe` 0
        i2 `shouldBe` 1
        i3 `shouldBe` 0

