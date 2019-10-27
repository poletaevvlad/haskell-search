{-# LANGUAGE OverloadedStrings #-}

module Documents(
  Document(Document), 
  getDocId, 
  getDocUrl, 
  getDocName, 
  getDocExcerpt, 
  getDocFileSize,
  getDocWordsCount,
  formatFileSize, 
  escapeFileName) where

import Numeric(showFFloat, showInt)
import Data.Char(toLower, isNumber, isLetter, isSeparator)
import Database.SQLite.Simple.FromRow(FromRow(fromRow), field)
import Data.Text(Text)


data Document = 
  Document 
    { getDocId :: Int
    , getDocUrl :: Text
    , getDocName :: Text
    , getDocExcerpt :: Text
    , getDocFileSize :: Int
    , getDocWordsCount :: Int
    } deriving (Read, Show)


instance FromRow Document where
  fromRow = Document <$> field <*> field <*> field <*> field <*> field <*> field


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


escapeFileName :: String -> String
escapeFileName fileName = reverse $ foldl processLetter "" fileName 
  where
    shouldKeep c = isLetter c || isNumber c
    shouldSubstitute = isSeparator

    processLetter :: String -> Char -> String
    processLetter [] curr 
      | shouldKeep curr = [toLower curr]
      | otherwise       = []

    processLetter prev@(p:_) curr 
      | shouldKeep curr       = toLower curr : prev
      | shouldSubstitute curr = if p == '-' then prev else '-' : prev
      | otherwise             = prev
