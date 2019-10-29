{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (
    nullConf,
    simpleHTTP,
    serveDirectory,
    dir,
    Browsing(DisableBrowsing))

import Pages.DocumentPage
import Database.DocumentsDB(loadDatabase)


main :: IO ()
main = do
  putStrLn "Launchig server"
  static_dir <- getDataFileName ""
  db <- loadDatabase "/data/text-db"

  simpleHTTP nullConf $ msum [
    documentPageHandler db,
    dir "static" $ serveDirectory DisableBrowsing [] static_dir]
