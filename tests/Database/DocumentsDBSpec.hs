{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDBSpec(spec) where

import Test.Hspec
import Database.DocumentsDB
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import qualified Database.SQLite.Simple as SQLite
import Documents
import Data.Maybe
import System.IO
import System.Directory


prepareDB :: [(String, String, String, Int, Int)] -> IO (Database, Int)
prepareDB docs =
  do
    db <- _createInMemory ""
    let conn = _getRawConnection db
    SQLite.executeMany conn insertQuery docs
    lastId <- fromIntegral <$> SQLite.lastInsertRowId conn
    return (db, lastId)
  where
    insertQuery = "INSERT INTO documents (name, url, excerpt, wordsCount, fileSize) \
                  \VALUES (?, ?, ?, ?, ?)"


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

  describe "retreiving documents" $ do
    context "when document exisist in the database" $ do
      it "shoud retrieve document by its id" $ do
        (db, doc2Id) <- prepareDB [doc1, doc2]
        doc <- getDocumentById db doc2Id

        let d = fromJust doc
        getDocName d `shouldBe` "Another document"
        getDocUrl d `shouldBe` "another-document"
        getDocExcerpt d `shouldBe` "Second document"
        getDocFileSize d `shouldBe` 12
        getDocWordsCount d `shouldBe` 5
        closeDatabase db

      it "should retrieve document by url" $ do
        (db, doc2Id) <- prepareDB [doc1, doc2]
        doc <- getDocumentByUrl db "another-document"
        let d = fromJust doc
        getDocId d `shouldBe` doc2Id
        getDocName d `shouldBe` "Another document"

    context "when no such document is present" $ do
      it "should return Nothing when searching by id" $ do
        (db, _) <- prepareDB [doc1, doc2]
        doc <- getDocumentById db (negate 4)
        isNothing doc `shouldBe` True
        closeDatabase db

      it "should return Nothing when searching by url" $ do
        (db, _) <- prepareDB [doc1, doc2]
        doc <- getDocumentByUrl db "non-existant-url"
        isNothing doc `shouldBe` True
        closeDatabase db

  describe "getDocumentContent" $ do
    it "should return the contents of a document" $ do
      withSystemTempDirectory  "database" (\path -> do
        createDirectory (path ++ "/docs/")
        writeFile (path ++ "/docs/73") "line one\nline two\nline three\n"

        db <- loadDatabase path
        let doc = Document{ getDocId = 73 }
        result <- getDocumentContent db doc
        result `shouldBe` ["line one", "line two", "line three"]
        closeDatabase db)

