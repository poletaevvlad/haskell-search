{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDB (Database, loadDatabase, closeDatabase,
  _createInMemory, _getRawConnection, getDocumentById, getDocumentByUrl,
  getDocumentContent, storeDocument, AlphaIndexEntry(All, Character, Symbols),
  buildAlphaIndex, Range(Range), paginationRange, queryDocuments, queryAllTexts,
  getDocumentsByIds, deleteDocument, updateDocument) where

import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (NamedParam((:=)))
import qualified Data.Text as Text
import Data.List.Split (endBy)
import Data.List
import Paths_webse
import Database.Documents
import System.Directory
import TextUtils.Processing
import Data.Text(pack)
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import System.IO (hGetContents, withFile, IOMode(ReadMode))
import Control.DeepSeq (($!!))

data Database = Database FilePath SQLite.Connection


initializeDB :: SQLite.Connection -> IO ()
initializeDB conn = do
  initSQLQuery <- getDataFileName "Init.sql" >>= readFile
  mapM_ (SQLite.execute_ conn) $ map SQLite.Query $ filter (not . Text.null) $ map (Text.strip . Text.pack) $ endBy ";" initSQLQuery


loadDatabase :: FilePath -> IO Database
loadDatabase path = do
  conn <- SQLite.open (path ++ "/index.sqlite")
  initializeDB conn
  createDirectoryIfMissing True (path ++ "/docs/")
  return $ Database path conn


_createInMemory :: FilePath -> IO Database
_createInMemory path = do
  conn <- SQLite.open ":memory:"
  initializeDB conn
  return $ Database path conn


_getRawConnection :: Database -> SQLite.Connection
_getRawConnection (Database _ conn) = conn


closeDatabase :: Database -> IO ()
closeDatabase (Database _ conn) = SQLite.close conn


maybeSingleResult :: [a] -> Maybe a
maybeSingleResult res
  | length res == 0 = Nothing
  | otherwise       = Just $ head res


getDocumentById :: Database -> Int -> IO (Maybe Document)
getDocumentById (Database _ conn) docId = maybeSingleResult <$> (
  SQLite.query conn "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                    \FROM documents WHERE rowid = ? LIMIT 1" (SQLite.Only docId) :: IO [Document])


getDocumentByUrl :: Database -> String -> IO (Maybe Document)
getDocumentByUrl (Database _ conn) url = maybeSingleResult <$> (
  SQLite.query conn "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                    \FROM documents WHERE url = ? LIMIT 1" (SQLite.Only url) :: IO [Document])


getDocumentsByIds :: Database -> [Int] -> IO ([Document])
getDocumentsByIds _ [] = return []
getDocumentsByIds (Database _ conn) ids = do
  let idArray = mconcat $ intersperse ", " $ map show ids
  let request = "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                \FROM documents WHERE rowid IN (" ++ idArray ++ ")"
  docs <- SQLite.query_ conn (SQLite.Query $ pack request) :: IO [Document]
  let dMap = IntMap.fromList $ map (\doc -> (getDocId doc, doc)) docs
  return $ map fromJust $ filter isJust $ map (flip IntMap.lookup dMap) ids


getDocumentContentById :: Database -> Int -> IO [String]
getDocumentContentById (Database path _) docId = do
  let filePath = path ++ "/docs/" ++ (show docId)
  withFile filePath ReadMode $ \handle -> do
    text <- hGetContents handle
    return $!! (lines text)


getDocumentContent :: Database -> Document -> IO [String]
getDocumentContent db doc = getDocumentContentById db $ getDocId doc


getUrl :: SQLite.Connection -> Int -> String -> IO String
getUrl conn ignoreId title = do
  let plainUrl = escapeFileName title
  let urlsQuery = "SELECT url FROM documents WHERE url LIKE ? AND rowid != ?"
  urls <- SQLite.query conn urlsQuery (plainUrl ++ "%", ignoreId) :: IO [SQLite.Only String]
  let urlStrings = map SQLite.fromOnly urls
  let candidateUrls = plainUrl:(map ((\x -> plainUrl ++ "-" ++ x) . show) ([2..] :: [Int]))
  return $ head $ filter (`notElem` urlStrings) candidateUrls


storeDocument :: Database -> String -> [String] -> IO Document
storeDocument (Database path conn) title contents = do
  let fileContents = mconcat $ intersperse "\n" contents
  url <- getUrl conn (-1) title
  let doc = Document { getDocId = 0
                     , getDocUrl = url
                     , getDocName = title
                     , getDocExcerpt = getExcerpt 50 fileContents
                     , getDocFileSize = foldl (\s x -> s + 1 + length x) 0 contents
                     , getDocWordsCount = length $ splitWords fileContents }

  SQLite.execute conn "INSERT INTO documents (url, name, excerpt, fileSize, wordsCount) \
                      \VALUES (?, ?, ?, ?, ?)" doc
  insertedId <- SQLite.lastInsertRowId conn

  writeFile (path ++ "/docs/" ++ show insertedId) (fileContents ++ "\n")
  return doc { getDocId = fromIntegral insertedId }


updateDocument :: Database -> Document -> String -> [String] -> IO Document
updateDocument (Database path conn) doc title contents = do
  let fileContents = mconcat $ intersperse "\n" contents
  newUrl <- getUrl conn (getDocId doc) title
  let newDoc = doc { getDocUrl = newUrl
                   , getDocName = title
                   , getDocExcerpt = getExcerpt 50 fileContents
                   , getDocFileSize = foldl (\s x -> s + 1 + length x) 0 contents
                   , getDocWordsCount = length $ splitWords fileContents }
  SQLite.execute conn "UPDATE documents SET url = ?, name = ?, excerpt = ?, fileSize = ?, wordsCount = ? \
                      \WHERE rowid = ?" ( getDocUrl newDoc, getDocName newDoc, getDocExcerpt newDoc
                                        , getDocFileSize newDoc, getDocWordsCount newDoc, getDocId newDoc)
  let fileName = path ++ "/docs/" ++ (show $ getDocId newDoc)
  let tempFileName = fileName ++ "-temp"
  writeFile tempFileName $ fileContents ++ "\n"
  renameFile tempFileName fileName
  return newDoc

deleteDocument :: Database -> Document -> IO ()
deleteDocument (Database path conn) doc =
  do
    SQLite.execute conn "DELETE FROM documents WHERE rowid=?" (SQLite.Only $ getDocId doc)
    let fileName = path ++ "/docs/" ++ (show $ getDocId doc)
    fileExist <- doesFileExist fileName
    when fileExist $ removeFile fileName


data AlphaIndexEntry = All | Character Char | Symbols
  deriving (Show, Eq)


buildAlphaIndex :: Database -> IO [AlphaIndexEntry]
buildAlphaIndex (Database _ conn) =
  map (entryFromChar . head . SQLite.fromOnly) <$> responce
  where
    responce :: IO [SQLite.Only String]
    responce = SQLite.query_ conn query

    query :: SQLite.Query
    query = "SELECT DISTINCT CASE \
            \  WHEN substr(name, 1, 1) BETWEEN 'A' AND 'Z' THEN substr(name, 1, 1) \
            \  WHEN substr(name, 1, 1) BETWEEN 'a' AND 'z' THEN upper(substr(name, 1, 1)) \
            \  ELSE '~' \
            \END AS letter \
            \FROM documents ORDER BY letter"

    entryFromChar :: Char -> AlphaIndexEntry
    entryFromChar '~' = Symbols
    entryFromChar c = Character c


data Range = Range Int Int deriving (Show, Eq)


paginationRange :: Int -> Int -> Range
paginationRange pageSize pageNumber =
  Range ((pageNumber - 1) * pageSize) pageSize


queryDocuments :: Database -> AlphaIndexEntry -> Range -> IO ([Document], Int)
queryDocuments (Database _ conn) entry (Range skip count) =
  do
    docs <- SQLite.queryNamed conn query ([":skip" := skip, ":count" := count] ++ params)
    docsCount <- SQLite.queryNamed conn countQuery params
    return (docs, SQLite.fromOnly $ head docsCount)
  where
    query = SQLite.Query $ pack ("SELECT rowid, url, name, excerpt, fileSize, wordsCount FROM documents " ++ whereClause ++ " ORDER BY name LIMIT :skip, :count")
    countQuery = SQLite.Query $ pack ("SELECT COUNT(rowid) FROM documents " ++ whereClause)
    (whereClause, params) = case entry of
      All -> ("", [])
      Character c -> ("WHERE upper(substr(name, 1, 1)) = :char", [":char" := [c]])
      Symbols -> ("WHERE upper(substr(name, 1, 1)) NOT BETWEEN 'A' AND 'Z'", [])


queryAllTexts :: Database -> IO [(Int, [String])]
queryAllTexts db@(Database _ conn) =
  SQLite.fold_ conn "SELECT rowid FROM documents" [] add
  where
    add :: [(Int, [String])] -> SQLite.Only Int -> IO [(Int, [String])]
    add prev (SQLite.Only docId) =
      (\text -> (docId, text):prev) <$> getDocumentContentById db docId

