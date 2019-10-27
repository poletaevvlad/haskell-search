{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDB (
  loadDatabase, 
  closeDatabase, 
  _createInMemory, 
  _getRawConnection,
  getDocumentById) where 

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


getDocumentById :: Database -> Int -> IO (Maybe Document)
getDocumentById (Database _ conn) docId = do
  res <- SQLite.query conn "SELECT rowid, url, name, excerpt, fileSize, wordsCount \
                           \FROM documents WHERE rowid = ? LIMIT 1" (SQLite.Only docId) :: IO [Document]
  if length res == 0
    then return Nothing
    else return $ Just $ head res
