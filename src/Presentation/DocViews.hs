{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocViews where

import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Documents


documentPreview :: Document -> H.Html
documentPreview doc = H.div ! A.class_ "doc" $ do
  H.div (H.toHtml meta) ! A.class_ "doc-meta"
  H.a (H.toHtml $ getDocName doc) ! A.class_ "doc-title" ! A.href (toValue $ getDocUrl doc)
  H.div (H.toHtml $ getDocExcerpt doc) ! A.class_ "doc-description"
  where 
    meta = (show wordsNum) ++ wordsSuffix ++ fileSizeStr
    wordsNum = getDocWordsCount doc
    wordsSuffix = if wordsNum == 1 then " word, " else " words, "
    fileSizeStr = formatFileSize $ getDocFileSize doc
