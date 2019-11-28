module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (simpleHTTP, serveDirectory, dir, askRq, guardRq,
  Browsing(DisableBrowsing), trailingSlash, toResponse, rqUri, rqQuery,
  ServerPart, Response, tempRedirect)
import Pages.DocumentPage(documentPageHandler)
import Pages.DocumentsIndex(documentsIndexHandler)
import Pages.AdminPages(adminHandler)
import Database.DocumentsDB(loadDatabase)
import Search.Index(loadIndex)
import Data.IORef
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), createDirectoryIfMissing)
import Service.Config (loadConfig)


removeTrailingSlash :: ServerPart Response
removeTrailingSlash =
  trailingSlash >> askRq >>= \req ->
    let url = (init $ rqUri req) ++ (rqQuery req)
    in do
      guardRq (\_ -> (rqUri req) /= "/")
      tempRedirect url $ toResponse ""


main :: IO ()
main = do
  configPath <- getXdgDirectory XdgConfig "webse.ini"
  config <- loadConfig configPath
  case config of
    Left errorMessage -> do
      putStrLn $ "Configuration error:"
      putStrLn $ "  config file location: " ++ configPath
      putStrLn $ "  " ++ errorMessage
    Right (netConfig, authConf, path) -> do
      staticDir <- getDataFileName "Static"
      createDirectoryIfMissing True path
      db <- loadDatabase path
      index <- loadIndex path
      indexRef <- newIORef $ Just index
      simpleHTTP netConfig $ msum [
        removeTrailingSlash,
        documentsIndexHandler db indexRef,
        documentPageHandler db,
        adminHandler authConf db indexRef,
        dir "static" $ serveDirectory DisableBrowsing [] staticDir]
