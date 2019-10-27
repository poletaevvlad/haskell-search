{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDBSpec(spec) where

import Test.Hspec
import Database.DocumentsDB
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import qualified Database.SQLite.Simple as SQLite
import Documents
import Data.Maybe


spec :: Spec
spec = do
  describe "loadDatabase" $ do
    it "should create new database" $ do
      withSystemTempDirectory  "database" (\path -> do
        db <- loadDatabase path  
        closeDatabase db

        let dbName = path ++ "/index.sqlite"
        fileCreated <- doesFileExist dbName
        fileCreated `shouldBe` True
        
        conn <- SQLite.open dbName
        rows <- SQLite.query_ conn "SELECT type, name FROM sqlite_master" :: IO [(String, String)]
        rows `shouldBe` [("table", "documents"), ("index", "documents_url")]
        SQLite.close conn
        )

  let doc1 = ("Document name", "document-name", "Document excerpt.", 2, 45) :: (String, String, String, Int, Int)
  let doc2 = ("Another document", "another-document", "Second document", 5, 12) :: (String, String, String, Int, Int)
  let insertQuery = "INSERT INTO documents (name, url, excerpt, wordsCount, fileSize) VALUES (?, ?, ?, ?, ?)"

  describe "retreiving documents" $ do
    context "when document exisist in the database" $ do
      it "shoud retrieve document by its id" $ do      
        db <- _createInMemory ""
        let conn = _getRawConnection db  
        SQLite.executeMany conn insertQuery [doc1, doc2]
        doc2Id <- fromIntegral <$> SQLite.lastInsertRowId conn
        doc <- getDocumentById db doc2Id
        
        let d = fromJust doc
        getDocName d `shouldBe` "Another document"
        getDocUrl d `shouldBe` "another-document"
        getDocExcerpt d `shouldBe` "Second document"
        getDocFileSize d `shouldBe` 12
        getDocWordsCount d `shouldBe` 5
        closeDatabase db
    
    context "when no such document is present" $ do
      it "should return Nothing when searching by id" $ do
        db <- _createInMemory ""
        let conn = _getRawConnection db  
        SQLite.executeMany conn insertQuery [doc1, doc2]
        doc <- getDocumentById db (negate 4)
        isNothing doc `shouldBe` True
        closeDatabase db

