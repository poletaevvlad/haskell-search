module Presentation.AlphabeticalIndexSpec (spec) where

import Test.Hspec
import Text.Blaze.Html.Renderer.String
import Presentation.AlphabeticalIndex 

spec :: Spec
spec = do
  describe "alphabeticalIndex" $ do
    -- it "should generate nothing if no entries are presented" $ do
    --   renderHtml (alphabeticalIndex show [] Nothing) `shouldBe` ""
    -- it "should generate nothing if only one entry is presented" $ do
    --   renderHtml (alphabeticalIndex show [All] Nothing) `shouldBe` ""
    -- it "should generate nothing if only two entries are presented" $ do
    --   renderHtml (alphabeticalIndex show [All, Character 'A'] Nothing) `shouldBe` ""
    
    it "should generate index if many entries are presented" $ do
      let entries = [All, Character 'A', Character 'B', Symbols]
      renderHtml (alphabeticalIndex show entries Nothing) `shouldBe` "<div class=\"index\"><div class=\"label\">Index:</div><div class=\"index-items\"><a href=\"All\">All</a><a href=\"Character &#39;A&#39;\">A</a><a href=\"Character &#39;B&#39;\">B</a><a href=\"Symbols\">#</a></div></div>"

    it "should generate index if many entries are presented" $ do
      let entries = [All, Character 'A', Character 'B', Character 'C']
      renderHtml (alphabeticalIndex show entries (Just $ Character 'B')) `shouldBe` "<div class=\"index\"><div class=\"label\">Index:</div><div class=\"index-items\"><a href=\"All\">All</a><a href=\"Character &#39;A&#39;\">A</a><a href=\"Character &#39;B&#39;\" class=\"current\">B</a><a href=\"Character &#39;C&#39;\">C</a></div></div>"
  