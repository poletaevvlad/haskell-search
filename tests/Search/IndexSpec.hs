module Search.IndexSpec (spec)  where

import Test.Hspec
import Search.Index
import Database.DocumentsDB
import Search.InvertedIndex
import qualified Search.TermIndex as TI
import System.IO.Temp(withSystemTempDirectory)
import System.Directory
import Database.Documents
import Data.Maybe

spec :: Spec
spec = do
  describe "buildIndex" $ do
    let words = ["hello", "world", "everyon", "welcom"]
    it "should generate index" $ do
      (terms, docs, d1, d2) <- withSystemTempDirectory "database" (\path -> do
        createDirectory (path ++ "/docs/")
        db <- loadDatabase path
        doc1 <- storeDocument db "doc_a" ["hello, world", "hello everyone"]
        doc2 <- storeDocument db "doc_b" ["welcome everyone"]

        (invIdx, termIdx) <- buildIndex path TI.new db
        closeDatabase db
        docs <- mapM (\str -> performTermSearch (fromJust $ TI.lookup str termIdx) invIdx) words
        return (termIdx, map (\dl -> map dieDocId dl) docs, getDocId doc1, getDocId doc2))

      map (flip TI.lookup terms) words `shouldBe` [Just 2, Just 3, Just 1, Just 0]
      docs `shouldBe` [[d1], [d1], [d2, d1], [d2]]
