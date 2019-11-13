module Search.IndexSpec (spec)  where

import Test.Hspec
import Search.Index
import Database.DocumentsDB
import qualified Search.InvertedIndex as II
import qualified Search.TermIndex as TI
import System.IO.Temp(withSystemTempDirectory)
import System.Directory
import Database.Documents
import Data.Maybe
import qualified Data.Set as Set
import Control.Monad.State.Lazy (execState)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


withIndex :: (Index -> IO a) -> IO a
withIndex callback =
  withSystemTempDirectory "database" (\path -> do
    createDirectory (path ++ "/docs/")
    db <- loadDatabase path
    _ <- storeDocument db "doc_1" ["wa wb wc wc wc wd"]
    _ <- storeDocument db "doc_2" ["wb wd wd we"]
    _ <- storeDocument db "doc_3" ["wa wa wc"]
    _ <- storeDocument db "doc_3" ["wc"]
    let index = createIndex Set.empty path TI.new Nothing
    index <- buildIndex index db
    callback index)


transformReq :: IntMap [(Int, II.DocIndexEntry)] -> IntMap [(Int, Int, Int)]
transformReq = IntMap.map (map (\(tid, entry) -> (tid, II.dieDocId entry, II.diePosCount entry)))



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
        let index = createIndex stopWords path TI.new Nothing
        index <- buildIndex index db

        closeDatabase db
        let invIndex = fromJust $ indexInvIndex index
        docs <- mapM (\str -> II.performTermSearch (fromJust $ TI.lookup str (indexTermsIndex index)) invIndex) words
        return ((indexTermsIndex index), map (\dl -> map II.dieDocId dl) docs, getDocId doc1, getDocId doc2))

      map (flip TI.lookup terms) words `shouldBe` [Just 2, Just 3, Just 1, Just 0]
      map (flip TI.lookup terms) ["stopword", "anotherword"] `shouldBe` [Nothing, Nothing]
      docs `shouldBe` [[d1], [d1], [d2, d1], [d2]]

  describe "loadIndex" $ do
    it "should return empty index if no files are present" $ do
      index <- withSystemTempDirectory "database" loadIndex
      (Set.size $ indexStopWords index) `shouldBe` 172
      (TI.null $ indexTermsIndex index) `shouldBe` True
      (isNothing $ indexInvIndex index) `shouldBe` True

    it "should reopen built index" $ do
      (docCorrect, termIndex) <- withSystemTempDirectory "database" (\path -> do
        createDirectory (path ++ "/docs/")
        db <- loadDatabase path
        doc1 <- storeDocument db "doc_a" ["hello, world", "hello everyone"]
        index <- loadIndex path >>= flip buildIndex db
        closeDatabase db >> closeIndex index

        index2 <- loadIndex path
        docs <- II.performTermSearch 0 $ fromJust $ indexInvIndex index2
        return ((II.dieDocId $ head docs) == getDocId doc1, indexTermsIndex index2))
      docCorrect `shouldBe` True
      TI.lookup "hello" termIndex `shouldBe` Just 0

  describe "loadStopWords" $ do
    it "should load words and ignore comments and newlines" $ do
      words <- loadStopWords
      Set.size words `shouldBe` 172
      "# List of stop words" `Set.member` words `shouldBe` False

  describe "processQuery" $ do
    it "should transform words into their ids" $ do
      let termsIndex = execState (TI.add "a" 1 >> TI.add "b" 2 >> TI.add "c" 3) TI.new
      let index = createIndex Set.empty "" termsIndex Nothing

      processQuery "a b c, b, d, a" index `shouldBe` [1, 2, 3, 2, 1]

  describe "getRequestDocs" $ do
    it "should generate a map" $ do
      res <- transformReq <$> withIndex (\index -> getRequestDocs [1, 2] index)
      res `shouldBe` IntMap.fromList [
        (1, [(1, 1, 1), (2, 1, 1)]),
        (2, [(2, 2, 1)]),
        (3, [(1, 3, 2)])]
    it "should return empty result for empty request" $ do
      res <- transformReq <$> withIndex (\index -> getRequestDocs [] index)
      res `shouldBe` IntMap.empty
