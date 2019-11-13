module Search.Index(buildIndex, loadStopWords, Index(..), loadIndex,
  createIndex, closeIndex) where

import Control.Monad.State.Lazy
import Database.DocumentsDB (Database, queryAllTexts)
import qualified Search.InvertedIndex as II
import Search.TermIndex (TermIndex, requestId)
import qualified Search.TermIndex as TI
import Search.InvertedIndex (InvertedIndex)
import Search.IndexBuilding (IndexBuilder, withIndexBuilder, addDocument)
import Search.Porter (porter)
import TextUtils.Processing (filterChars, splitWords)
import TextUtils.StateUtils(accumState)
import Data.Set(Set)
import qualified Data.Set as Set
import Paths_webse
import System.Directory (doesFileExist)


data Index =
  Index { indexStopWords :: Set String
        , indexLocation :: FilePath
        , indexTermsIndex :: TermIndex
        , indexInvIndex :: Maybe InvertedIndex
        }


loadIndex :: FilePath -> IO Index
loadIndex path = do
  stopWords <- loadStopWords
  termIndex <- TI.loadIndex path

  indexPresent <- doesFileExist $ path ++ "/inv.index"
  invIndex <- if indexPresent then Just <$> II.loadIndex path
                              else return Nothing

  return Index { indexStopWords = stopWords
               , indexLocation = path
               , indexTermsIndex = termIndex
               , indexInvIndex = invIndex
               }

createIndex :: Set String -> FilePath -> TermIndex -> Maybe InvertedIndex -> Index
createIndex = Index


loadStopWords :: IO (Set String)
loadStopWords = do
  file <- getDataFileName "StopWords" >>= readFile
  return $ Set.fromAscList $ filter keepLine $ lines file
  where
    keepLine :: String -> Bool
    keepLine "" = False
    keepLine text = head text /= '#'


closeIndex :: Index -> IO ()
closeIndex index =
  case indexInvIndex index of
    Nothing -> return ()
    Just invIndex -> II.closeIndex invIndex


buildIndex :: Index -> Database -> IO Index
buildIndex index database = do
  closeIndex index

  termIndex <- withIndexBuilder (indexLocation index) $ do
    documents <- lift $ queryAllTexts database
    addToInvertedIndex (indexTermsIndex index) documents
  invIndex <- II.loadIndex $ indexLocation index
  TI.saveIndex (indexLocation index) termIndex
  return $ index { indexTermsIndex = termIndex, indexInvIndex = Just invIndex }
  where
    addToInvertedIndex :: TermIndex -> [(Int, [String])] -> StateT IndexBuilder IO TermIndex
    addToInvertedIndex termIdx []  = return termIdx
    addToInvertedIndex termIdx ((docId, lines):others) = do
      let words = map porter $ mconcat $ map strToWords lines
      let (ids, newTermIdx) = runState (accumState $ map requestId words) termIdx
      addDocument docId ids
      addToInvertedIndex newTermIdx others

    strToWords :: String -> [String]
    strToWords = filter (`Set.notMember` (indexStopWords index)) . splitWords . filterChars
