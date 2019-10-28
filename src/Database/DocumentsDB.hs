{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDB (
  Database,
  loadDatabase,
  closeDatabase,
  _createInMemory,
  _getRawConnection,
  getDocumentById,
  getDocumentByUrl,
  getDocumentContent,
  storeDocument) where

import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as Text
import Data.List.Split (endBy)
import Data.List
import Paths_webse
import Documents
import System.Directory
import TextUtils.Processing

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


getDocumentContent :: Database -> Document -> IO [String]
getDocumentContent (Database path _) doc = do
  let filePath = path ++ "/docs/" ++ (show $ getDocId doc)
  lines <$> (readFile filePath)


storeDocument :: Database -> String -> [String] -> IO Document
storeDocument (Database path conn) title contents = do
  let fileContents = mconcat $ intersperse "\n" contents
  let plainUrl = escapeFileName title

  let urlsQuery = "SELECT url FROM documents WHERE url LIKE ?"
  urls <- SQLite.query conn urlsQuery (SQLite.Only (plainUrl ++ "%")) :: IO [SQLite.Only String]
  let urlStrings = map SQLite.fromOnly urls
  let candidateUrls = plainUrl:(map ((\x -> plainUrl ++ "-" ++ x) . show) ([2..] :: [Int]))
  let url = head $ filter (`notElem` urlStrings) candidateUrls

  let doc = Document { getDocId = 0
                     , getDocUrl = url
                     , getDocName = title
                     , getDocExcerpt = getExcerpt 30 $ head contents
                     , getDocFileSize = foldl (\s x -> s + 1 + length x) 0 contents
                     , getDocWordsCount = length $ splitWords fileContents }

  SQLite.execute conn "INSERT INTO documents (url, name, excerpt, fileSize, wordsCount) \
                      \VALUES (?, ?, ?, ?, ?)" doc
  insertedId <- SQLite.lastInsertRowId conn

  writeFile (path ++ "/docs/" ++ show insertedId) (fileContents ++ "\n")
  return doc { getDocId = fromIntegral insertedId }
