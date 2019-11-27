module Presentation.ToolbarSpec(spec) where

import Test.Hspec
import Presentation.Toolbar
import Text.Blaze.Html.Renderer.String


spec :: Spec
spec = do
  describe "toolbar" $ do
    it "should generate toolbar with multiple actions" $ do
      let html = toolbar [Action "/page1" "Page 1", Action "/page2" "Page 2", Action "/page3" "Page 3"]
      renderHtml html `shouldBe` "<div class=\"toolbar\">\
          \<a href=\"/page1\">Page 1</a> - \
          \<a href=\"/page2\">Page 2</a> - \
          \<a href=\"/page3\">Page 3</a>\
        \</div>"

    it "should generate confirm actions" $ do
      let html = toolbar [ConfirmAction "/page" "Page" "Are \"you\" sure?\\\""]
      renderHtml html `shouldBe` "<div class=\"toolbar\">\
          \<a href=\"/page\" onclick=\"return confirm(&quot;Are \\&quot;you\\&quot; sure?\\\\\\&quot;&quot;)\">Page</a>\
        \</div>"
