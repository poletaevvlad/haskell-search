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
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "buildIndex" $ do
    let words = ["hello", "world", "everyon", "welcom"]
    it "should generate index" $ do
      (terms, docs, d1, d2) <- withSystemTempDirectory "database" (\path -> do
        createDirectory (path ++ "/docs/")
        db <- loadDatabase path
        doc1 <- storeDocument db "doc_a" ["hello, stopword world", "hello everyone"]
        doc2 <- storeDocument db "doc_b" ["welcome everyone anotherword"]

        let stopWords = Set.fromList ["stopword", "anotherword"]
        (invIdx, termIdx) <- buildIndex path stopWords TI.new db
        closeDatabase db
        docs <- mapM (\str -> performTermSearch (fromJust $ TI.lookup str termIdx) invIdx) words
        return (termIdx, map (\dl -> map dieDocId dl) docs, getDocId doc1, getDocId doc2))

      map (flip TI.lookup terms) words `shouldBe` [Just 2, Just 3, Just 1, Just 0]
      map (flip TI.lookup terms) ["stopword", "anotherword"] `shouldBe` [Nothing, Nothing]
      docs `shouldBe` [[d1], [d1], [d2, d1], [d2]]
  describe "loadStopWords" $ do
    it "should load words and ignore comments and newlines" $ do
      words <- loadStopWords
      Set.size words `shouldBe` 172
      "# List of stop words" `Set.member` words `shouldBe` False

