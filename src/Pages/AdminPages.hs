module Pages.AdminPages (adminHandler) where

import Control.Monad(msum)
import Happstack.Server (ok, ServerPart, Response, toResponse, tempRedirect,
  dir, askRq, require, look, getDataFn, rqMethod, decodeBody, Method(POST),
  defaultBodyPolicy, mkCookie, CookieLife(MaxAge), addCookie, expireCookie,
  path, nullDir)
import Presentation.Layout(appLayout, paginator)
import Presentation.Login(loginForm)
import Presentation.DocViews(smallDocumentsList)
import qualified Text.Blaze.Html5 as H
import Pages.Auth (AuthConf(auConfTimeOut), checkPassword, generateAuthSecret)
import TextUtils.Processing(hexEncode)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Pages.Auth (requireLogin, isLoggedIn)
import Database.Documents(Document(getDocId, getDocName))
import Pages.DocumentsIndex(optPageNum, PageNumber(..))
import qualified Database.DocumentsDB as DB
import Presentation.DocEditor
import Data.Maybe
import TextUtils.Editing(toEditor)

handleLoginForm :: AuthConf -> ServerPart Response
handleLoginForm conf = do
  rq <- askRq
  loggedIn <- isLoggedIn conf
  if loggedIn then tempRedirect "/admin" $ toResponse ""
  else if rqMethod rq /= POST
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


handleAdminPage :: PageNumber -> DB.Database -> ServerPart Response
handleAdminPage pageNumber db =
  require (Just <$> DB.queryDocuments db DB.All range) $ \(docs, total) -> do
    ok $ toResponse $ appLayout "Admin" "" $ do
      smallDocumentsList urlFormatter docs (50 * (fromPageNumber pageNumber - 1) + 1)
      paginator pageUrlFormatter (fromPageNumber pageNumber) $ pagesCount total
  where
    urlFormatter doc = "/admin/edit/" ++ (show $ getDocId doc)
    pageUrlFormatter 1 = "/admin"
    pageUrlFormatter n = "/admin/page-" ++ show n
    range = DB.paginationRange 50 $ fromPageNumber pageNumber

    pagesCount :: Int -> Int
    pagesCount total = ceiling $ fromIntegral total / (50 :: Double)


handleEditPage :: Maybe (Document, [String]) -> ServerPart Response
handleEditPage mdoc = do
  let editorType = if isNothing mdoc then Create else Edit
  let (title, content) = if isNothing mdoc then ("", "") else let Just (doc, docContents) = mdoc in (getDocName doc, toEditor docContents 80)
  ok $ toResponse $ appLayout "Editing" "" $ do
    docEditor editorType title content


adminHandler :: AuthConf -> DB.Database -> ServerPart Response
adminHandler authConf db = msum [
  dir "admin" $ dir "login" $ handleLoginForm authConf,
  dir "admin" $ dir "logout" $ handleLogout,
  dir "admin" $ optPageNum $ \pageNum -> requireLogin authConf >> handleAdminPage pageNum db,
  dir "admin" $ dir "edit" $ path $ \docId -> require (loadDocForEditing docId) $ \doc -> requireLogin authConf >> handleEditPage (Just doc),
  dir "admin" $ dir "edit" $ nullDir >> requireLogin authConf >> handleEditPage Nothing
  ]

  where
    loadDocForEditing :: Int -> IO (Maybe (Document, [String]))
    loadDocForEditing docId = do
      mdoc <- DB.getDocumentById db docId
      case mdoc of
        Nothing -> return Nothing
        Just doc -> do
          content <- DB.getDocumentContent db doc
          return $ Just (doc, content)
