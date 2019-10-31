module Pages.UrlUtilsSpec(spec) where

import Test.Hspec
import Pages.UrlUtils


spec :: Spec
spec = do
  describe "popUrlComponent" $ do
    it "should keep empty url" $ do
      popUrlComponent "/" `shouldBe` "/"
    it "should pop only component" $ do
      popUrlComponent "/component" `shouldBe` "/"
    it "should pop only component with trailing slash" $ do
      popUrlComponent "/component/" `shouldBe` "/"
    it "should pop components" $ do
      popUrlComponent "/a/b/c" `shouldBe` "/a/b"
    it "should pop components with trailing slash" $ do
      popUrlComponent "/a/b/c/" `shouldBe` "/a/b"
  describe "setPageNum" $ do
    it "should add page num to Url" $ do
      setPageNum "/path" 4 `shouldBe` "/path/page-4"
    it "should change page number" $ do
      setPageNum "/path/page-2" 3 `shouldBe` "/path/page-3"
    it "should remove page number from Url" $ do
      setPageNum "/path/page-2" 1 `shouldBe` "/path"
    it "should correctly add page number to root" $ do
      setPageNum "/" 2 `shouldBe` "/page-2"
    it "should correctly remove page number for root" $ do
      setPageNum "/page-3" 1 `shouldBe` "/"
    it "should ignore trailing slash when adding" $ do
      setPageNum "/page/" 2 `shouldBe` "/page/page-2"
    it "should ignore trailing slash when removing" $ do
      setPageNum "/page/page-3/" 1 `shouldBe` "/page"
    it "should ignore trailing slash when changing" $ do
      setPageNum "/page/page-3/" 4 `shouldBe` "/page/page-4"
