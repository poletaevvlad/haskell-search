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
