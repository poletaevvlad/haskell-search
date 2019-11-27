module Pages.AdminPages (adminHandler) where

import Control.Monad(msum)
import Happstack.Server (ok, ServerPart, Response, toResponse, tempRedirect,
  dir, askRq, require, look, getDataFn, rqMethod, decodeBody, Method(POST),
  defaultBodyPolicy, mkCookie, CookieLife(MaxAge), addCookie, expireCookie,
  path, nullDir, method, rqDataError, Errors(..))
import Presentation.Layout(appLayout, paginator)
import Presentation.Login(loginForm)
import Presentation.DocViews(smallDocumentsList)
import Text.Blaze(string, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Pages.Auth (AuthConf(auConfTimeOut), checkPassword, generateAuthSecret)
import TextUtils.Processing(hexEncode)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Pages.Auth (requireLogin, isLoggedIn)
import Database.Documents(Document(getDocId, getDocName, getDocUrl))
import Pages.DocumentsIndex(optPageNum, PageNumber(..))
import qualified Database.DocumentsDB as DB
import Presentation.DocEditor
import Presentation.Toolbar
import Data.Maybe
import TextUtils.Editing(toEditor, removeSpaces, fromEditor)
import Search.Index(Index)
import qualified Search.Index as Index
import Data.IORef(IORef, atomicModifyIORef)


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
      toolbar [Action "/admin/edit" "New document", Action "/admin/build-index" "Rebuild index", Action "/admin/logout" "Logout"]
      smallDocumentsList urlFormatter docs (50 * (fromPageNumber pageNumber - 1) + 1)
      paginator pageUrlFormatter (fromPageNumber pageNumber) $ pagesCount total
  where
    urlFormatter doc = "/admin/edit/" ++ (show $ getDocId doc)
    pageUrlFormatter 1 = "/admin"
    pageUrlFormatter n = "/admin/page-" ++ show n
    range = DB.paginationRange 50 $ fromPageNumber pageNumber

    pagesCount :: Int -> Int
    pagesCount total = ceiling $ fromIntegral total / (50 :: Double)


handleEditPage :: DB.Database -> Maybe (Document, [String]) -> ServerPart Response
handleEditPage db mdoc = do
  msum [ method POST >> decodeBody (defaultBodyPolicy "/tmp/" 0 10485760 1024) >> (do
      title <- removeSpaces <$> look "title"
      content <- look "text"
      let parsedContent = fromEditor content
      error <- case (title, parsedContent) of
        ("", _) -> return $ Just "Title must not be empty"
        (_, []) -> return $ Just "Document body must not be empty"
        _ -> return $ Nothing
      if isNothing error
        then
          let addDocument = (case mdoc of Nothing -> DB.storeDocument db title parsedContent
                                          Just (doc, _) -> DB.updateDocument db doc title parsedContent)
          in require (Just <$> addDocument) $ \newDoc -> tempRedirect ("/doc/" ++ (getDocUrl newDoc)) $ toResponse ""
        else renderEditPage error title content),
    (case mdoc of
      Nothing -> renderEditPage Nothing "" ""
      Just (doc, content) -> renderEditPage Nothing (getDocName doc) $ toEditor content 80)]
  where
    renderEditPage :: Maybe String -> String -> String -> ServerPart Response
    renderEditPage error title content =
      let editorType = if isNothing mdoc then Create else Edit
      in ok $ toResponse $ appLayout "Editing" "" $ do
        toolbar genToolbar
        case error of
          Nothing -> mempty
          Just error -> H.div ! A.class_ (toValue "form-error") $ do
            H.b (H.toHtml "Error:")
            string $ ' ':error
        docEditor editorType title content

    genToolbar :: [ToolbarAction]
    genToolbar = case mdoc of Nothing -> [allDocs]
                              Just (doc, _) -> [allDocs, viewDoc doc, deleteDoc doc]
      where
        allDocs = Action "/admin" "All documents"
        viewDoc doc = Action ("/doc/" ++ getDocUrl doc) "View document"
        deleteDoc doc = ConfirmAction ("/admin/edit/" ++ (show $ getDocId doc) ++ "/delete") "Delete" "Are you sure you want to delete this document? This action cannot be reversed."


handleNotFound :: AuthConf -> ServerPart Response
handleNotFound conf = do
  loggedIn <- isLoggedIn conf
  if loggedIn
    then rqDataError $ Errors []
    else tempRedirect "/admin/login" $ toResponse ""


adminHandler :: AuthConf -> DB.Database -> IORef (Maybe Index) -> ServerPart Response
adminHandler authConf db indexRef = dir "admin" $ msum [
  dir "login" $ nullDir >> handleLoginForm authConf,
  requireLogin authConf >> msum [
    dir "build-index" $ nullDir >> (require buildIndex $ \_ -> tempRedirect "/admin/" $ toResponse ""),
    dir "logout" $ nullDir >> handleLogout,
    optPageNum $ \pageNum -> nullDir >> handleAdminPage pageNum db,
    dir "edit" $ path $ \docId -> nullDir >> (require (loadDocForEditing docId) $ \doc -> handleEditPage db (Just doc)),
    dir "edit" $ nullDir >> handleEditPage db Nothing,
    dir "edit" $ path $ \docId -> dir "delete" $ nullDir >> (require (deleteDocument docId) $ \_ -> tempRedirect "/admin/" $ toResponse "")
    ],
  handleNotFound authConf
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

    deleteDocument :: Int -> IO (Maybe ())
    deleteDocument docId = do
      mdoc <- DB.getDocumentById db docId
      case mdoc of
        Nothing -> return Nothing
        Just doc -> DB.deleteDocument db doc >> (return $ Just ())

    buildIndex :: IO (Maybe ())
    buildIndex = do
      maybeIndex <- atomicModifyIORef indexRef (\index -> (Nothing, index))
      case maybeIndex of
        Nothing -> return Nothing
        Just index -> do
          newIndex <- Index.buildIndex index db
          atomicModifyIORef indexRef (\_ -> (Just newIndex, ()))
          return $ Just ()

