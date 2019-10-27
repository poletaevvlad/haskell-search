{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentsDBSpec(spec) where

import Test.Hspec
import Database.DocumentsDB
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import qualified Database.SQLite.Simple as SQLite


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
