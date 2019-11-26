module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (nullConf, simpleHTTP, serveDirectory, dir, askRq,
                         Browsing(DisableBrowsing), trailingSlash, toResponse,
                         rqUri, rqQuery, ServerPart, Response, tempRedirect,
                         guardRq)
import Pages.DocumentPage(documentPageHandler)
import Pages.DocumentsIndex(documentsIndexHandler)
import Pages.AdminPages(adminHandler)
import Database.DocumentsDB(loadDatabase)
import Search.Index(loadIndex)
import Data.IORef
import Pages.Auth (AuthConf(..))
import qualified Data.ByteString.Lazy as BS


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

  let hash = [0x5e, 0x88, 0x48, 0x98, 0xda, 0x28, 0x04, 0x71,
              0x51, 0xd0, 0xe5, 0x6f, 0x8d, 0xc6, 0x29, 0x27,
              0x73, 0x60, 0x3d, 0x0d, 0x6a, 0xab, 0xbd, 0xd6,
              0x2a, 0x11, 0xef, 0x72, 0x1d, 0x15, 0x42, 0xd8]
  let authConf = AuthConf { auConfTimeOut = 24 * 60 * 60
                          , auConfSecret = BS.pack [0..31]
                          , auConfPasswordHash = BS.pack hash }

  simpleHTTP nullConf $ msum [
    removeTrailingSlash,
    documentsIndexHandler db indexRef,
    documentPageHandler db,
    adminHandler authConf db,
    dir "static" $ serveDirectory DisableBrowsing [] static_dir]
