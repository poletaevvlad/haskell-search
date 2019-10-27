module Database.DocumentsDB (loadDatabase, closeDatabase) where 

import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as Text
import Data.List.Split (endBy)
import Paths_webse

data Database = Database FilePath SQLite.Connection


loadDatabase :: FilePath -> IO Database
loadDatabase path = do
  conn <- SQLite.open (path ++ "/index.sqlite")
  initSQLQuery <- getDataFileName "Init.sql" >>= readFile
  
  mapM_ (SQLite.execute_ conn) $ map SQLite.Query $ filter (not . Text.null) $ map (Text.strip . Text.pack) $ endBy ";" initSQLQuery
  return $ Database path conn


closeDatabase :: Database -> IO ()
closeDatabase (Database _ conn) = SQLite.close conn


