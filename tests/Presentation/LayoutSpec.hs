module Presentation.LayoutSpec(spec) where

import Test.Hspec
import Presentation.Layout(paginator)
import Text.Blaze.Html.Renderer.String


spec :: Spec
spec = do
  describe "paginator" $ do
    context "when there is a single page" $ do
      it "generates empty html" $ do
        renderHtml (paginator show 0 1) `shouldBe` ""
    context "when there are multiple pages" $ do
      it "generates for the first page" $ do
        renderHtml (paginator show 1 5) `shouldBe` "<div class=\"paginator\"> -- 1/5 -- <a href=\"2\">Next page</a></div>"

      it "generates for the last page" $ do
        renderHtml (paginator show 5 5) `shouldBe` "<div class=\"paginator\"><a href=\"4\">Previous page</a> -- 5/5 -- </div>"

      it "generates for the last page" $ do
        renderHtml (paginator show 3 5) `shouldBe` "<div class=\"paginator\"><a href=\"2\">Previous page</a> -- 3/5 -- <a href=\"4\">Next page</a></div>"
