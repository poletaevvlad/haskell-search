module Pages.DocumentsIndex(documentsIndexHandler, optPageNum, PageNumber(..)) where

import Text.Read(readMaybe)
import Control.Monad
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Happstack.Server (FromReqURI(fromReqURI), path, ok, ServerPart, Response,
  toResponse, nullDir, tempRedirect, dir, askRq, rqUri, rqQuery, require, look,
  getDataFn)
import Pages.UrlUtils(popUrlComponent, setPageNum)
import Presentation.Layout(appLayout, paginator)
import Presentation.AlphabeticalIndex(alphabeticalIndex)
import Presentation.DocViews(documentPreview)
import Database.DocumentsDB(Database, AlphaIndexEntry(..), buildAlphaIndex,
  queryDocuments, paginationRange, Range(Range), getDocumentsByIds)
import Database.Documents(Document(getDocUrl))
import Text.Blaze ((!))
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Search.Index(Index, getRequestDocIds, processQuery)
import Data.IORef
import Data.Maybe(isNothing, fromJust)


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
    req <- askRq
    let newUrl = (popUrlComponent $ rqUri req) ++ (rqQuery req)
    tempRedirect newUrl $ toResponse "",
  path handler,
  nullDir >> (handler $ PageNumber 1)]


data QueryType = IndexQuery AlphaIndexEntry
               | SearchQuery String

instance FromReqURI AlphaIndexEntry where
  fromReqURI "symbol" = Just Symbols
  fromReqURI part
   | not $ null $ tail part = Nothing
   | char `elem` ['a'..'z'] = Just $ Character $ toUpper char
   | otherwise = Nothing
   where char = toLower $ head part


renderAlphaIndex :: [AlphaIndexEntry] -> Maybe AlphaIndexEntry -> H.Html
renderAlphaIndex =
  alphabeticalIndex makeIndexUrl
  where
    makeIndexUrl :: AlphaIndexEntry -> String
    makeIndexUrl All = "/"
    makeIndexUrl (Character c) = ['/', toLower c]
    makeIndexUrl Symbols = "/symbol"


renderDocumentPreview :: Document -> H.Html
renderDocumentPreview = documentPreview (\doc -> "/doc/" ++ getDocUrl doc)


renderDocumentsIndex :: QueryType -> PageNumber -> ([AlphaIndexEntry], [Document], Int) -> ServerPart Response
renderDocumentsIndex queryType pageNum (alphaIndex, documents, totalPages) = do
  req <- askRq
  ok $ toResponse $
    appLayout pageTitle searchQuery $ do
      H.h1 (H.toHtml pageTitle)
      renderAlphaIndex alphaIndex indexEntry
      if null documents
        then H.div ! A.class_ (Blaze.toValue "no-results") $ H.toHtml "Nothing is found"
        else mapM_ renderDocumentPreview documents
      paginator (\num -> setPageNum (rqUri req) num ++ rqQuery req) (fromPageNumber pageNum) totalPages
    where
      (pageTitle, indexEntry) = case queryType of
        IndexQuery All -> ("All documents", Just All)
        IndexQuery (Character char) -> ("All documents (letter " ++ [char] ++ ")", Just $ Character char)
        IndexQuery Symbols -> ("All documents (non latin character)", Just Symbols)
        SearchQuery _ -> ("Search results", Nothing)
      searchQuery = case queryType of
        (SearchQuery query) -> query
        _ -> ""


documentsIndexHandler :: Database -> (IORef (Maybe Index)) -> ServerPart Response
documentsIndexHandler db indexRef =
  msum [
    optPageNum $ docsIndex $ IndexQuery All,
    path $ \index -> optPageNum $ docsIndex $ IndexQuery index,
    dir "search" $ optPageNum $ \pageNum -> msum [
      handleSearch pageNum,
      tempRedirect "/" $ toResponse ""]
    ]
  where
    docsIndex :: QueryType -> PageNumber -> ServerPart Response
    docsIndex queryType pageNum =
      require (loadValues queryType pageNum) $ renderDocumentsIndex queryType pageNum

    loadValues :: QueryType -> PageNumber -> IO (Maybe ([AlphaIndexEntry], [Document], Int))
    loadValues queryType (PageNumber pageNum) = do
      alphaIndex <- buildAlphaIndex db
      (documents, docsCount) <- case queryType of
        IndexQuery index -> queryDocuments db index $ paginationRange 20 pageNum
        SearchQuery query -> do
          index <- readIORef indexRef
          if isNothing index then return ([], 0)
          else do
            let request = processQuery query $ fromJust index
            docIds <- getRequestDocIds request $ fromJust index
            let docsCount = length docIds
            let (Range skip count) = paginationRange 20 pageNum
            docs <- getDocumentsByIds db $ take count $ drop skip docIds
            return (docs, docsCount)
      let pagesNum = ceiling $ fromIntegral docsCount / (20 :: Double)
      return $ Just (All:alphaIndex, documents, pagesNum)

    handleSearch :: PageNumber -> ServerPart Response
    handleSearch pageNum = do
      query <- getDataFn $ look "q"
      case query of
        (Left _) -> mzero
        (Right query) ->
          if null query then mzero
          else docsIndex (SearchQuery query) pageNum
