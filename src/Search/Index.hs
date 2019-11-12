module Search.Index(buildIndex) where

import Control.Monad.State.Lazy
import Database.DocumentsDB (Database, queryAllTexts)
import Search.InvertedIndex (loadIndex)
import Search.TermIndex (TermIndex, requestId,)
import Search.InvertedIndex (InvertedIndex)
import Search.IndexBuilding (IndexBuilder, withIndexBuilder, addDocument)
import Search.Porter (porter)
import TextUtils.Processing (filterChars, splitWords)
import TextUtils.StateUtils(accumState)


buildIndex :: FilePath -> TermIndex -> Database -> IO (InvertedIndex, TermIndex)
buildIndex path termIndex database = do
  termIndex <- withIndexBuilder path $ do
    documents <- lift $ queryAllTexts database
    addToInvertedIndex termIndex documents
  index <- loadIndex path
  return (index, termIndex)
  where
    addToInvertedIndex :: TermIndex -> [(Int, [String])] -> StateT IndexBuilder IO TermIndex
    addToInvertedIndex termIdx []  = return termIdx
    addToInvertedIndex termIdx ((docId, lines):others) = do
      let words = map porter $ mconcat $ map (splitWords . filterChars) lines
      let (ids, newTermIdx) = runState (accumState $ map requestId words) termIdx
      addDocument docId ids
      addToInvertedIndex newTermIdx others
