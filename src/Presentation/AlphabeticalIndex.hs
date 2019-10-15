{-# LANGUAGE OverloadedStrings #-}

module Presentation.AlphabeticalIndex(
  alphabeticalIndex, 
  IndexEntry(All, Character, Symbols)
  ) where

import Control.Monad
import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
  

data IndexEntry = All | Character Char | Symbols 
  deriving (Show, Eq)


alphabeticalIndex :: (IndexEntry -> String) -> [IndexEntry] -> Maybe IndexEntry -> H.Html
alphabeticalIndex urlFactory entries currentEntry = 
  H.div ! A.class_ "index" $ do
    H.div "Index:" ! A.class_ "label"
    H.div ! A.class_ "index-items" $ do
      forM_ entries renderEntry
  
  where
    stringFor All = "All"
    stringFor (Character c) = [c]
    stringFor Symbols = "#"

    renderEntry :: IndexEntry -> H.Html
    renderEntry entry = 
      let html = H.a (H.toHtml $ stringFor entry) ! A.href (toValue $ urlFactory entry)
      in if Just entry == currentEntry then html ! A.class_ "current" else html
