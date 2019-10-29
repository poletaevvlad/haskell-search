{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDBSpec(spec) where

import Test.Hspec
import Database.DocumentsDB
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import qualified Database.SQLite.Simple as SQLite
import Documents
import Data.Maybe


type DocTuple = (String, String, String, Int, Int)

insertQuery :: SQLite.Query
insertQuery = "INSERT INTO documents (name, url, excerpt, wordsCount, fileSize) \
                  \VALUES (?, ?, ?, ?, ?)"


prepareDB :: [(String, String, String, Int, Int)] -> IO (Database, Int)
prepareDB docs =
  do
    db <- _createInMemory ""
    let conn = _getRawConnection db
    SQLite.executeMany conn insertQuery docs
    lastId <- fromIntegral <$> SQLite.lastInsertRowId conn
    return (db, lastId)


spec :: Spec
spec = do
  describe "loadDatabase" $ do
    it "should create new database" $ do
      withSystemTempDirectory  "database" (\path -> do
        db <- loadDatabase path
        closeDatabase db

        docsDirExists <- doesDirectoryExist (path ++ "/docs/")
        docsDirExists `shouldBe` True

        let dbName = path ++ "/index.sqlite"
        fileCreated <- doesFileExist dbName
        fileCreated `shouldBe` True

        conn <- SQLite.open dbName
        rows <- SQLite.query_ conn "SELECT type, name FROM sqlite_master" :: IO [(String, String)]
        rows `shouldBe` [("table", "documents"), ("index", "documents_url")]
        SQLite.close conn)

  let doc1 = ("Document name", "document-name", "Document excerpt.", 2, 45) :: DocTuple
  let doc2 = ("Another document", "another-document", "Second document", 5, 12) :: DocTuple

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

  describe "storeDocument" $ do
    it "should store document in the database and in the filesystem" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        doc <- storeDocument db "Document title" ["first line", "second line", "third line"]

        getDocName doc `shouldBe` "Document title"
        getDocUrl doc `shouldBe` "document-title"
        getDocExcerpt doc `shouldBe` "first line"
        getDocWordsCount doc `shouldBe` 6
        getDocFileSize doc `shouldBe` 34

        savedFile <- readFile (path ++ "/docs/" ++ (show $ getDocId doc))
        savedFile `shouldBe` "first line\nsecond line\nthird line\n"
        closeDatabase db

        conn <- SQLite.open (path ++ "/index.sqlite")
        let query = "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                    \FROM documents WHERE rowid = ?"
        res <- SQLite.query conn query $ SQLite.Only $ getDocId doc :: IO [Document]
        head res `shouldBe` doc)

    it "should generate unique urls" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path

        let conn = _getRawConnection db
        let docs = [("D1", "url", "D1.", 2, 45), ("D1", "url-1", "D1.", 2, 45),
                    ("D1", "url-3", "D1.", 2, 45)] :: [DocTuple]
        SQLite.executeMany conn insertQuery docs

        doc <- storeDocument db "URL" ["a", "b"]
        closeDatabase db
        getDocUrl doc `shouldBe` "url-2")

  describe "buildAlphaIndex" $ do
    it "should correctly generate alphabetical index" $ do
      (db, _) <- prepareDB [("A doc", "url-1", "D1.", 2, 45),
                            ("B doc", "url-2", "D1.", 2, 45),
                            ("B doc", "url-3", "D1.", 2, 45)]
      entries <- buildAlphaIndex db
      entries `shouldBe` [Character 'A', Character 'B']

    it "should covnert to upplercase letters" $ do
      (db, _) <- prepareDB [("a doc", "url-1", "D1.", 2, 45),
                            ("b doc", "url-2", "D1.", 2, 45),
                            ("b doc", "url-3", "D1.", 2, 45)]
      entries <- buildAlphaIndex db
      entries `shouldBe` [Character 'A', Character 'B']

    it "should generate symbols " $ do
      (db, _) <- prepareDB [("a doc", "url-1", "D1.", 2, 45),
                            ("# doc", "url-2", "D1.", 2, 45),
                            ("% doc", "url-3", "D1.", 2, 45)]
      entries <- buildAlphaIndex db
      entries `shouldBe` [Character 'A', Symbols]

  describe "queryDocuments" $ do
    let docs = [("A doc", "url-1", "D1.", 2, 45)
               ,("D doc", "url-2", "D1.", 2, 45)
               ,("B doc 1", "url-2", "D1.", 2, 45)
               ,("B doc 2", "url-2", "D1.", 2, 45)
               ,("B doc 3", "url-2", "D1.", 2, 45)
               ,("-- document --", "url-3", "D1.", 2, 45)
               ] :: [DocTuple]

    it "should return all documents" $ do
      (db, _) <- prepareDB docs
      res <- queryDocuments db All (Range 0 3)
      map getDocName res `shouldBe` ["-- document --", "A doc", "B doc 1"]

    it "should return documents with names starting with a specific character" $ do
      (db, _) <- prepareDB docs
      res <- queryDocuments db (Character 'B') (Range 1 2)
      map getDocName res `shouldBe` ["B doc 2", "B doc 3"]

    it "should return documents with names starting with symbols" $ do
      (db, _) <- prepareDB docs
      res <- queryDocuments db (Symbols) (Range 0 5)
      map getDocName res `shouldBe` ["-- document --"]

  describe "paginationRange" $ do
    it "should generate range for the first page" $ do
      paginationRange 20 1 `shouldBe` Range 0 20
    it "should generate range for next pages" $ do
      paginationRange 20 2 `shouldBe` Range 20 20
