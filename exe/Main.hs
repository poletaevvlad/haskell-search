module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (nullConf, simpleHTTP, serveDirectory, dir, askRq,
                         Browsing(DisableBrowsing), trailingSlash, toResponse,
                         rqUri, rqQuery, ServerPart, Response, tempRedirect,
                         guardRq)
import Pages.DocumentPage(documentPageHandler)
import Pages.DocumentsIndex(documentsIndexHandler)
import Database.DocumentsDB(loadDatabase)
import Search.Index(loadIndex, buildIndex)
import Data.IORef


removeTrailingSlash :: ServerPart Response
removeTrailingSlash =
  trailingSlash >> askRq >>= \req ->
    let url = (init $ rqUri req) ++ (rqQuery req)
    in do
      guardRq (\_ -> (rqUri req) /= "/")
      tempRedirect url $ toResponse ""


main :: IO ()
main = do
  putStrLn "Launchig server"
  static_dir <- getDataFileName "Static"
  db <- loadDatabase "/data/text-db"
  index <- loadIndex "/data/text-db"
  indexRef <- newIORef $ Just index

  simpleHTTP nullConf $ msum [
    removeTrailingSlash,
    documentsIndexHandler db indexRef,
    documentPageHandler db,
    dir "static" $ serveDirectory DisableBrowsing [] static_dir]
