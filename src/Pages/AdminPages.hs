module Pages.AdminPages (adminHandler) where

import Control.Monad(msum)
import Happstack.Server (ok, ServerPart, Response, toResponse, tempRedirect,
  dir, askRq, require, look, getDataFn, rqMethod, decodeBody, Method(POST),
  defaultBodyPolicy, mkCookie, CookieLife(MaxAge), addCookie, expireCookie)
import Presentation.Layout(appLayout)
import Presentation.Login(loginForm)
import qualified Text.Blaze.Html5 as H
import Pages.Auth (AuthConf(auConfTimeOut), checkPassword, generateAuthSecret)
import TextUtils.Processing(hexEncode)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Pages.Auth (requireLogin)

handleLoginForm :: AuthConf -> ServerPart Response
handleLoginForm conf = do
  rq <- askRq
  if rqMethod rq /= POST
    then showLoginForm Nothing
    else do
      decodeBody $ defaultBodyPolicy "/tmp/" 0 1024 1024
      password <- getDataFn $ look "password"
      case password of
        (Left _ ) -> showLoginForm Nothing
        (Right pass) ->
          -- trying to log in
          if checkPassword conf pass
            then require (Just <$> generateAuthSecret conf) $ \bytes -> do
              let cookieLife = MaxAge $ floor $ nominalDiffTimeToSeconds $ auConfTimeOut conf
              addCookie cookieLife $ mkCookie "auth" $ hexEncode bytes
              tempRedirect "/admin" $ toResponse ""
            else showLoginForm $ Just "Your password is incorrect"
  where
    showLoginForm :: Maybe String -> ServerPart Response
    showLoginForm errorMessage =
      ok $ toResponse $ do
        appLayout "Login" "" $ do
          H.h1 (H.toHtml "Login")
          loginForm errorMessage


handleLogout :: ServerPart Response
handleLogout = do
  expireCookie "auth"
  tempRedirect "/" $ toResponse ""


handleAdminPage :: ServerPart Response
handleAdminPage = do
  ok $ toResponse $ "hello"



adminHandler :: AuthConf -> ServerPart Response
adminHandler authConf = msum [
  dir "admin" $ dir "login" $ handleLoginForm authConf,
  dir "admin" $ dir "logout" $ handleLogout,
  dir "admin" $ requireLogin authConf >> handleAdminPage
  ]
