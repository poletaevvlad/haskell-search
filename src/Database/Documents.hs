{-# LANGUAGE OverloadedStrings #-}

module Database.Documents(Document(..), formatFileSize, escapeFileName) where

import Numeric(showFFloat, showInt)
import Data.Char(toLower, isNumber, isLetter, isSeparator)
import Database.SQLite.Simple.FromRow(FromRow(fromRow), field)
import Database.SQLite.Simple.ToRow(ToRow(toRow))


data Document =
  Document
    { getDocId :: Int
    , getDocUrl :: String
    , getDocName :: String
    , getDocExcerpt :: String
    , getDocFileSize :: Int
    , getDocWordsCount :: Int
    } deriving (Read, Show, Eq)


instance FromRow Document where
  fromRow = Document <$> field <*> field <*> field <*> field <*> field <*> field


instance ToRow Document where
  toRow doc = toRow (getDocUrl doc, getDocName doc, getDocExcerpt doc,
                     getDocFileSize doc, getDocWordsCount doc)


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
