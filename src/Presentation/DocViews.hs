{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocViews (documentPreview, fullDocumentView,
  smallDocumentsList) where

import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Database.Documents


docMeta :: Document -> String
docMeta doc = (show wordsNum) ++ wordsSuffix ++ fileSizeStr
  where
    wordsNum = getDocWordsCount doc
    wordsSuffix = if wordsNum == 1 then " word, " else " words, "
    fileSizeStr = formatFileSize $ getDocFileSize doc


documentPreview :: (Document -> String) -> Document -> H.Html
documentPreview urlFormatter doc = H.div ! A.class_ "doc" $ do
  H.div (H.toHtml $ docMeta doc) ! A.class_ "doc-meta"
  H.a (H.toHtml $ getDocName doc) ! A.class_ "doc-title" ! A.href (toValue $ urlFormatter doc)
  H.div (H.toHtml $ getDocExcerpt doc) ! A.class_ "doc-description"


fullDocumentView :: Document -> [String] -> H.Html
fullDocumentView doc paragraphs = do
  H.h1 (H.toHtml $ getDocName doc)
  mapM_ (H.p . H.toHtml) paragraphs


smallDocumentsList :: (Document -> String) -> [Document] -> Int -> H.Html
smallDocumentsList urlFormatter docs start =
  do
    H.ol ! A.start (toValue $ show start) $ do
      mapM_ listEntry docs
  where
    listEntry :: Document -> H.Html
    listEntry doc = H.li $ do
      H.a (H.toHtml $ getDocName doc) ! A.href(toValue $ urlFormatter doc)
      H.div (H.toHtml $ docMeta doc) ! A.class_ "doc-meta"
