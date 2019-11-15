{-# LANGUAGE OverloadedStrings #-}

module Presentation.Login (loginForm) where

import Text.Blaze ((!))
import Text.Blaze(string, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


loginForm :: Maybe String -> H.Html
loginForm error =
  H.form ! A.action "/admin/login" ! A.method "POST" $ do
    case error of
      Nothing -> mempty
      Just errorMessage -> do
        H.div ! A.class_ "login-error" $ do
          H.b "Error:"
          string $ ' ':errorMessage
    H.div ! A.class_ "login-row" $ do
      H.label "Password:" ! A.for "password"
      H.input ! A.type_ "password" ! A.name "password" ! A.placeholder "Your password"
    H.input ! A.type_ "submit" ! A.value "[ Login ]" ! A.class_ "login-submit"
