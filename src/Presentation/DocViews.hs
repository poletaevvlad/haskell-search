{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocViews (documentPreview, fullDocumentView) where

import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Database.Documents


documentPreview :: (Document -> String) -> Document -> H.Html
documentPreview urlFormatter doc = H.div ! A.class_ "doc" $ do
  H.div (H.toHtml meta) ! A.class_ "doc-meta"
  H.a (H.toHtml $ getDocName doc) ! A.class_ "doc-title" ! A.href (toValue $ urlFormatter doc)
  H.div (H.toHtml $ getDocExcerpt doc) ! A.class_ "doc-description"
  where
    meta = (show wordsNum) ++ wordsSuffix ++ fileSizeStr
    wordsNum = getDocWordsCount doc
    wordsSuffix = if wordsNum == 1 then " word, " else " words, "
    fileSizeStr = formatFileSize $ getDocFileSize doc


fullDocumentView :: Document -> [String] -> H.Html
fullDocumentView doc paragraphs = do
  H.h1 (H.toHtml $ getDocName doc)
  mapM_ (H.p . H.toHtml) paragraphs
