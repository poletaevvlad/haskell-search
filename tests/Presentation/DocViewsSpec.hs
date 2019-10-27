{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocViewsSpec (spec) where

import Test.Hspec
import Presentation.DocViews
import Documents
import Text.Blaze.Html.Renderer.String


spec :: Spec
spec = do
  describe "documentPreview" $ do
    it "must render html for a document" $ do
      let doc = Document{
        getDocId=5,
        getDocUrl="/doc/document",
        getDocName="Document's name", 
        getDocExcerpt="Document's excerpt", 
        getDocFileSize=500, 
        getDocWordsCount=24
      }
      renderHtml (documentPreview doc)  `shouldBe` "<div class=\"doc\"><div class=\"doc-meta\">24 words, 500 bytes</div><a class=\"doc-title\" href=\"/doc/document\">Document&#39;s name</a><div class=\"doc-description\">Document&#39;s excerpt</div></div>"

    it "must use singular 'word' when there is only one word in the document" $ do
      let doc = Document{
        getDocId=5,
        getDocUrl="/doc/document",
        getDocName="Document's name", 
        getDocExcerpt="Document's excerpt", 
        getDocFileSize=500, 
        getDocWordsCount=1
      }
      renderHtml (documentPreview doc) `shouldBe` "<div class=\"doc\"><div class=\"doc-meta\">1 word, 500 bytes</div><a class=\"doc-title\" href=\"/doc/document\">Document&#39;s name</a><div class=\"doc-description\">Document&#39;s excerpt</div></div>"
