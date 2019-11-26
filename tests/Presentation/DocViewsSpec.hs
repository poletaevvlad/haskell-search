{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocViewsSpec (spec) where

import Test.Hspec
import Presentation.DocViews
import Database.Documents
import Text.Blaze.Html.Renderer.String


doc :: Document
doc = Document { getDocId=5
               , getDocUrl="/doc/document"
               , getDocName="Document's name"
               , getDocExcerpt="Document's excerpt"
               , getDocFileSize=500
               , getDocWordsCount=24 }

anotherDoc :: Document
anotherDoc = Document { getDocId=7
                      , getDocUrl="/doc/another"
                      , getDocName="Another document"
                      , getDocExcerpt=""
                      , getDocFileSize=600
                      , getDocWordsCount=17 }


spec :: Spec
spec = do
  describe "documentPreview" $ do
    it "must render html for a document" $ do
      renderHtml (documentPreview getDocUrl doc)  `shouldBe` "<div class=\"doc\"><div class=\"doc-meta\">24 words, 500 bytes</div><a class=\"doc-title\" href=\"/doc/document\">Document&#39;s name</a><div class=\"doc-description\">Document&#39;s excerpt</div></div>"

    it "must use singular 'word' when there is only one word in the document" $ do
      let doc2 = doc{getDocWordsCount = 1}
      renderHtml (documentPreview getDocUrl doc2) `shouldBe` "<div class=\"doc\"><div class=\"doc-meta\">1 word, 500 bytes</div><a class=\"doc-title\" href=\"/doc/document\">Document&#39;s name</a><div class=\"doc-description\">Document&#39;s excerpt</div></div>"

  describe "fullDocumentView" $ do
    it "should generate html and escape special characters" $ do
      let expected = "<h1>Document&#39;s name</h1><p>paragraph 1</p><p>paragraph with &lt;b&gt;tags&lt;/b&gt;</p>"
      renderHtml (fullDocumentView doc ["paragraph 1", "paragraph with <b>tags</b>"]) `shouldBe` expected

  describe "smallDocumentsList" $ do
    it "should generate html" $ do
      let expected = "<ol start=\"2\">\
        \<li>\
          \<a href=\"5\">Document&#39;s name</a>\
          \<div class=\"doc-meta\">24 words, 500 bytes</div>\
        \</li>\
        \<li>\
          \<a href=\"7\">Another document</a>\
          \<div class=\"doc-meta\">17 words, 600 bytes</div>\
        \</li>\
      \</ol>"
      renderHtml (smallDocumentsList (show . getDocId) [doc, anotherDoc] 2) `shouldBe` expected

