{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDB (
  Database,
  loadDatabase, 
  closeDatabase, 
  _createInMemory, 
  _getRawConnection,
  getDocumentById,
  getDocumentByUrl) where 

import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as Text
import Data.List.Split (endBy)
import Paths_webse
import Documents

data Database = Database FilePath SQLite.Connection


initializeDB :: SQLite.Connection -> IO ()
initializeDB conn = do
  initSQLQuery <- getDataFileName "Init.sql" >>= readFile
  mapM_ (SQLite.execute_ conn) $ map SQLite.Query $ filter (not . Text.null) $ map (Text.strip . Text.pack) $ endBy ";" initSQLQuery


loadDatabase :: FilePath -> IO Database
loadDatabase path = do
  conn <- SQLite.open (path ++ "/index.sqlite")
  initializeDB conn
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
