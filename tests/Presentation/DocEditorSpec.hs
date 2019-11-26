module Presentation.DocEditorSpec(spec) where

import Test.Hspec
import Presentation.DocEditor
import Text.Blaze.Html.Renderer.String

spec :: Spec
spec = do
  describe "docEditor" $ do
    it "should generate form for document creation" $ do
      let expected = "<form class=\"edit-form\" method=\"POST\">\
          \<div class=\"form-row\">\
            \<label for=\"title\">Title:</label>\
            \<input type=\"text\" name=\"title\" value=\"\">\
          \</div>\
          \<textarea name=\"text\"></textarea>\
          \<input type=\"submit\" value=\"[ Publish ]\">\
        \</form>"
      renderHtml (docEditor Create "" "") `shouldBe` expected

    it "should generate form for document updating" $ do
      let expected = "<form class=\"edit-form\" method=\"POST\">\
          \<div class=\"form-row\">\
            \<label for=\"title\">Title:</label>\
            \<input type=\"text\" name=\"title\" value=\"Document Title\">\
          \</div>\
          \<textarea name=\"text\">Document content</textarea>\
          \<input type=\"submit\" value=\"[ Update ]\">\
        \</form>"
      renderHtml (docEditor Edit "Document Title" "Document content") `shouldBe` expected


