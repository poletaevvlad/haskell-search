module Search.IndexBuildingSpec(spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import Search.IndexBuilding
import qualified Data.ByteString.Lazy as B
import TextUtils.FileUtils
import qualified Data.Binary as Binary
import Control.Monad.State


spec :: Spec
spec = do
  describe "addDocument" $ do
    it "should add positions" $ do
      withSystemTempDirectory  "database" (\path -> do
        builder1 <- createIndexBuilder path
        builder2 <- execStateT (addDocument 7 [0, 0, 1, 2, 0, 2, 2, 1]) builder1
        commit builder2

        posContents <- B.readFile $ path ++ "/positions.index"
        let numbers = parseByteString 4 posContents :: [Binary.Word32]
        numbers `shouldBe` [0, 1, 4, 2, 7, 3, 5, 6])
