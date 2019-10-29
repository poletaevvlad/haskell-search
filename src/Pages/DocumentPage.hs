module Pages.DocumentPage(documentPageHandler) where

import Happstack.Server (FromReqURI(fromReqURI), ServerPart, Response,
                         toResponse, ok, dir, path, require)
import Data.Char
import Documents
import Database.DocumentsDB(Database, getDocumentByUrl, getDocumentContent)
import Presentation.Layout(appLayout)
import Presentation.DocViews(fullDocumentView)

data DocumentUrl =
  DocumentUrl { fromDocumentUrl :: String }


instance FromReqURI DocumentUrl where
  fromReqURI part
    | all charPredicate part = Just $ DocumentUrl part
    | otherwise = Nothing
    where
      charPredicate :: Char -> Bool
      charPredicate c = c == '-' || isLower c || isNumber c


renderDocumentPage :: (Document, [String]) -> ServerPart Response
renderDocumentPage (doc, content) =
  ok $ toResponse $ appLayout (getDocName doc) $ do
    fullDocumentView doc content


requestDocument :: Database -> String -> IO (Maybe (Document, [String]))
requestDocument db url = do
  maybeDoc <- getDocumentByUrl db url
  case maybeDoc of
    Nothing -> return Nothing
    Just doc -> do
      contents <- getDocumentContent db doc
      return $ Just (doc, contents)


documentPageHandler :: Database -> ServerPart Response
documentPageHandler db = dir "doc" $ path $ \url -> require (requestDocument db $ fromDocumentUrl url) $ renderDocumentPage
