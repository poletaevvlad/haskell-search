{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Paths_webse
import Happstack.Server (
    nullConf, 
    simpleHTTP, 
    toResponse, 
    ok, 
    dir, 
    nullDir,
    serveDirectory, 
    Browsing(DisableBrowsing))

import Presentation.Layout(appLayout)


main :: IO ()
main = do
    putStrLn "Launchig server"
    static_dir <- getDataFileName ""
    simpleHTTP nullConf $ msum [
        do nullDir 
           ok $ toResponse $ appLayout "Hello world" "123",
        dir "static" $ serveDirectory DisableBrowsing [] static_dir]
    