module TextUtils.StateUtilsSpec(spec) where

import Test.Hspec
import TextUtils.StateUtils
import Control.Monad.State
import Data.Char

appendUpperSt :: String -> State String String
appendUpperSt val = do
  st <- get
  put $ st ++ val
  return $ (toUpper <$> val) ++ st


spec :: Spec
spec = do
  describe "appendUpperSt" $ do
    it "should compute value and new state" $ do
      runState (appendUpperSt "hello") "world" `shouldBe` ("HELLOworld", "worldhello")
  describe "accumState" $ do
    it "should return empty list if the source list is empty" $ do
      runState (accumState ([] :: [State String String])) "" `shouldBe` ([], "")
    it "should create a state with single element" $ do
      runState (accumState [appendUpperSt "b"]) "a" `shouldBe` (["Ba"], "ab")
    it "should create a state with multiple element list" $ do
      let states = [appendUpperSt "b", appendUpperSt "c", appendUpperSt "d"]
      runState (accumState states) "a" `shouldBe` (["Ba", "Cab", "Dabc"], "abcd")
