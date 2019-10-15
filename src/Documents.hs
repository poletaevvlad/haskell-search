{-# LANGUAGE OverloadedStrings #-}

module Documents(formatFileSize) where

import Numeric(showFFloat, showInt)

-- data Document = Document { 
--     url :: String, 
--     name :: String, 
--     excerpt :: String, 
--     fileSize :: Int, 
--     wordsCount :: Int }


formatFileSize :: Int -> String
formatFileSize val 
  | val == 0 = "empty"
  | val == 1 = "1 byte"
  | val < 1024 = show val ++ " bytes"
  | val < 1024 * 1024 = showBytes (valF / 1024.0) " KiB"
  | otherwise = showBytes (valF / 1024.0 / 1024.0) " MiB"

  where
    valF = fromIntegral val
    showBytes :: Float -> ShowS
    showBytes bytes
      | bytes < 10 = showFFloat (Just 1) bytes
      | otherwise = showInt $ (round bytes :: Int)