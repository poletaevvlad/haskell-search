module Search.IndexBuildingSpec(spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import Search.IndexBuilding
import Search.InvertedIndex


spec :: Spec
spec = do
  describe "addDocument" $ do
    it "should generate index" $ do
      (d1, d2, p2, p3) <- withSystemTempDirectory "database" (\path -> do
        withIndexBuilder path $ do
          addDocument 7 [0, 0, 1, 2, 0, 2, 2, 1]
          addDocument 16 [0, 1, 3, 3, 0, 0]

        index <- loadIndex path
        docs1 <- performTermSearch 4 index

        docs2 <- performTermSearch 2 index
        pos2 <- getPositions (head docs2) index

        docs3 <- performTermSearch 1 index
        pos3 <- mapM (\entry -> getPositions entry index) docs3
        return (docs1, map dieDocId docs2, pos2, pos3)
        )
      d1 `shouldBe` []
      d2 `shouldBe` [7]
      p2 `shouldBe` [3, 5, 6]
      p3 `shouldBe` [[2, 7], [1]]
