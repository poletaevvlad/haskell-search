{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDBSpec(spec) where

import Test.Hspec
import Database.DocumentsDB
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import qualified Database.SQLite.Simple as SQLite
import Database.Documents
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
        getDocExcerpt doc `shouldBe` "first line\nsecond line\nthird line"
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

  describe "updateDocument" $ do
    it "should change document title in database and content on disk" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        doc <- storeDocument db "Document title" ["first line", "second line", "third line"]
        doc2 <- updateDocument db doc "Updated title" ["updated line", "updated second"]

        getDocName doc2 `shouldBe` "Updated title"
        getDocUrl doc2 `shouldBe` "updated-title"
        getDocExcerpt doc2 `shouldBe` "updated line\nupdated second"
        getDocWordsCount doc2 `shouldBe` 4
        getDocFileSize doc2 `shouldBe` 28

        savedFile <- readFile (path ++ "/docs/" ++ (show $ getDocId doc))
        savedFile `shouldBe` "updated line\nupdated second\n"
        closeDatabase db

        conn <- SQLite.open (path ++ "/index.sqlite")
        let query = "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                    \FROM documents WHERE rowid = ?"
        res <- SQLite.query conn query $ SQLite.Only $ getDocId doc :: IO [Document]
        head res `shouldBe` doc2)
    it "should generate new url on collision" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        doc1 <- storeDocument db "First document" ["a"]
        doc2 <- storeDocument db "Second document" ["b"]
        doc2_new <- updateDocument db doc2 "First document" ["c"]
        getDocUrl doc2_new `shouldBe` "first-document-2")
    it "should keep the same url on update" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        doc1 <- storeDocument db "First document" ["a"]
        doc1_new <- updateDocument db doc1 "First document" ["c"]
        getDocUrl doc1_new `shouldBe` "first-document")

  describe "deleteDocument" $ do
    it "should delete db record and file on disk" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        doc1 <- storeDocument db "First" ["a", "b"]
        doc2 <- storeDocument db "Second" ["a", "c"]
        let conn = _getRawConnection db

        d1Exist1 <- doesFileExist $ path ++ "/docs/" ++ (show $ getDocId doc1)
        d2Exist1 <- doesFileExist $ path ++ "/docs/" ++ (show $ getDocId doc2)
        (d1Exist1, d2Exist1) `shouldBe` (True, True)
        ids1 <- SQLite.query_ conn "SELECT rowid FROM documents" :: IO [SQLite.Only Int]
        map SQLite.fromOnly ids1 `shouldBe` [getDocId doc1, getDocId doc2]

        deleteDocument db doc1
        d1Exist2 <- doesFileExist $ path ++ "/docs/" ++ (show $ getDocId doc1)
        d2Exist2 <- doesFileExist $ path ++ "/docs/" ++ (show $ getDocId doc2)
        (d1Exist2, d2Exist2) `shouldBe` (False, True)
        ids2 <- SQLite.query_ conn "SELECT rowid FROM documents" :: IO [SQLite.Only Int]
        map SQLite.fromOnly ids2 `shouldBe` [getDocId doc2]

        closeDatabase db)

    it "should fail silently if no such document exist" $ do
      withSystemTempDirectory "database" (\path -> do
        db <- loadDatabase path
        deleteDocument db $ Document { getDocId = 4 }
        closeDatabase db)

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

  let docs = [("A doc", "url-1", "D1.", 2, 45)
             ,("D doc", "url-2", "D1.", 2, 45)
             ,("B doc 1", "url-2", "D1.", 2, 45)
             ,("B doc 2", "url-2", "D1.", 2, 45)
             ,("B doc 3", "url-2", "D1.", 2, 45)
             ,("-- document --", "url-3", "D1.", 2, 45)
             ] :: [DocTuple]
  describe "queryDocuments" $ do
    it "should return all documents" $ do
      (db, _) <- prepareDB docs
      (res, count) <- queryDocuments db All (Range 0 3)
      map getDocName res `shouldBe` ["-- document --", "A doc", "B doc 1"]
      count `shouldBe` 6

    it "should return documents with names starting with a specific character" $ do
      (db, _) <- prepareDB docs
      (res, count) <- queryDocuments db (Character 'B') (Range 1 2)
      map getDocName res `shouldBe` ["B doc 2", "B doc 3"]
      count `shouldBe` 3

    it "should return documents with names starting with symbols" $ do
      (db, _) <- prepareDB docs
      (res, count) <- queryDocuments db (Symbols) (Range 0 5)
      map getDocName res `shouldBe` ["-- document --"]
      count `shouldBe` 1

  describe "paginationRange" $ do
    it "should generate range for the first page" $ do
      paginationRange 20 1 `shouldBe` Range 0 20
    it "should generate range for next pages" $ do
      paginationRange 20 2 `shouldBe` Range 20 20

  describe "queryAllTexts" $ do
    it "should return contents of every file in the database" $ do
      (results, d1, d2) <- withSystemTempDirectory  "database" (\path -> do
        createDirectory (path ++ "/docs/")
        db <- loadDatabase path
        doc1 <- storeDocument db "doc_a" ["a1", "a2", "a3"]
        doc2 <- storeDocument db "doc_b" ["b1", "b2"]

        result <- queryAllTexts db
        closeDatabase db
        return (result, getDocId doc1, getDocId doc2))
      results `shouldBe` [(d2, ["b1", "b2"]), (d1, ["a1", "a2", "a3"])]

  describe "getDocumentsByIds" $ do
    it "should return documents in the specified order" $ do
      (db, _) <- prepareDB docs
      res <- getDocumentsByIds db [2, 4, 3]
      map getDocName res `shouldBe` ["D doc", "B doc 2", "B doc 1"]
    it "should not return non-existent documents" $ do
      (db, _) <- prepareDB docs
      res <- getDocumentsByIds db [5, 12, 1]
      map getDocName res `shouldBe` ["B doc 3", "A doc"]
    it "should return nothing if no ids are passed" $ do
      (db, _) <- prepareDB docs
      res <- getDocumentsByIds db []
      map getDocName res `shouldBe` []
