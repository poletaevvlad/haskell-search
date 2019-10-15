module LayoutSpec(spec) where

import Test.Hspec
import Layout(paginator)
import Text.Blaze.Html.Renderer.String


spec :: Spec
spec = do
  describe "paginator" $ do
    context "when there is a single page" $ do
      it "generates empty html" $ do
        renderHtml (paginator show 0 1) `shouldBe` ""
    context "when there are multiple pages" $ do
      it "generates for the first page" $ do
        renderHtml (paginator show 0 5) `shouldBe` "<div class=\"paginator\"> -- 1/5 -- <a href=\"1\">Next page</a></div>"

      it "generates for the last page" $ do
        renderHtml (paginator show 4 5) `shouldBe` "<div class=\"paginator\"><a href=\"3\">Previous page</a> -- 5/5 -- </div>"

      it "generates for the last page" $ do
        renderHtml (paginator show 2 5) `shouldBe` "<div class=\"paginator\"><a href=\"1\">Previous page</a> -- 3/5 -- <a href=\"3\">Next page</a></div>"
