module Pages.DocumentsIndex(documentsIndexHandler) where

import Text.Read(readMaybe)
import Control.Monad
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Happstack.Server (FromReqURI(fromReqURI), path, ok, ServerPart, Response,
                         toResponse, nullDir, tempRedirect, dir)


data PageNumber = PageNumber { fromPageNumber :: Int } deriving (Show)

instance FromReqURI PageNumber where
  fromReqURI part
   | "page-" `isPrefixOf` part =
       let pageNumber = readMaybe $ drop 5 part :: Maybe Int
       in case pageNumber of
        Just number -> if number < 2
          then Nothing
          else Just $ PageNumber number
        Nothing -> Nothing
   | otherwise = Nothing


optPageNum :: (PageNumber -> ServerPart Response) -> ServerPart Response
optPageNum handler = msum [
  dir "page-1" $ do
    tempRedirect "." $ toResponse "",
  path handler,
  nullDir >> (handler $ PageNumber 1)]


data QueryType = All | Character Char | Symbol deriving (Show)

instance FromReqURI QueryType where
  fromReqURI "symbol" = Just Symbol
  fromReqURI part
   | not $ null $ tail part = Nothing
   | char `elem` ['a'..'z'] = Just $ Character $ toUpper char
   | otherwise = Nothing
   where char = toLower $ head part


renderDocumentsIndex :: QueryType -> PageNumber -> ServerPart Response
renderDocumentsIndex queryType pageNum =
  ok $ toResponse (show queryType ++ "\n" ++ show pageNum)


documentsIndexHandler :: ServerPart Response
documentsIndexHandler = msum [
  optPageNum $ renderDocumentsIndex All,
  path $ \queryType -> optPageNum $ renderDocumentsIndex queryType]
