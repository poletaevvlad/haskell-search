module Presentation.LoginSpec (spec) where


import Test.Hspec
import Presentation.Login
import Text.Blaze.Html.Renderer.String


spec :: Spec
spec = do
  describe "loginForm" $ do
    it "generates login form without error message" $ do
      renderHtml (loginForm Nothing) `shouldBe` "<form action=\"/admin/login\" method=\"POST\">\
        \<div class=\"login-row\">\
          \<label for=\"password\">Password:</label>\
          \<input type=\"password\" name=\"password\" placeholder=\"Your password\">\
        \</div>\
        \<input type=\"submit\" value=\"[ Login ]\" class=\"login-submit\">\
      \</form>"
    it "generates login form with error message" $ do
      renderHtml (loginForm $ Just "An error message") `shouldBe` "<form action=\"/admin/login\" method=\"POST\">\
        \<div class=\"login-error\"><b>Error:</b> An error message</div>\
        \<div class=\"login-row\">\
          \<label for=\"password\">Password:</label>\
          \<input type=\"password\" name=\"password\" placeholder=\"Your password\">\
        \</div>\
        \<input type=\"submit\" value=\"[ Login ]\" class=\"login-submit\">\
      \</form>"
