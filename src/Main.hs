module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (
    nullConf, 
    simpleHTTP, 
    toResponse, 
    ok, 
    dir, 
    serveDirectory, 
    Browsing(DisableBrowsing))


main :: IO ()
main = do
    static_dir <- getDataFileName ""
    simpleHTTP nullConf $ msum [
        dir "static" $ serveDirectory DisableBrowsing [] static_dir]
    