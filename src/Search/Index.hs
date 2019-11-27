module Search.Index(buildIndex, loadStopWords, Index(..), loadIndex,
  createIndex, closeIndex, processQuery, getRequestDocs, getRequestDocIds,
  computeDocumentRank) where

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
import Data.Maybe (fromJust, isJust, isNothing)
import Data.IntMap(IntMap, (!))
import qualified Data.IntMap as IntMap
import TextUtils.ListUtils


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


processQuery :: String -> Index -> [Int]
processQuery text index =
  map fromJust $ filter isJust $ map toId $ strToWords text
  where
    toId :: String -> Maybe Int
    toId str = TI.lookup str $ indexTermsIndex index

    strToWords :: String -> [String]
    strToWords = map porter . splitWords . filterChars


computeDocumentRank :: [Int] -> [(Int, [Int])] -> (Float, Float)
computeDocumentRank request entries =
  if null $ tail entries
    then (1, 1.0 / (fromIntegral $ length $ snd $ head entries))
    else let entriesMap = IntMap.fromList entries
             termPairs = pairs $ IntMap.keys entriesMap
             distances = map (\(t1, t2) -> let termDist = minTermDistance (entriesMap ! t1) (entriesMap ! t2)
                                               queryDist = minQueryDistance request t1 t2
                                           in fromIntegral termDist * fromIntegral queryDist) termPairs
         in (1.0 / (fromIntegral $ IntMap.size entriesMap), sum distances)


getRequestDocIds :: [Int] -> Index -> IO [Int]
getRequestDocIds request index = do
  let requiredCount = round $ (fromIntegral $ length request) * (0.75 :: Double)
  reqDocs <- IntMap.filter (\x -> length x >= requiredCount) <$> getRequestDocs request index :: IO (IntMap [(Int, II.DocIndexEntry)])
  docsWithPos <- sequence $ IntMap.map loadPositions reqDocs :: IO (IntMap [(Int, [Int])])
  return $ sortByKey (\docId -> computeDocumentRank request $ docsWithPos ! docId) $ IntMap.keys docsWithPos
  where
    loadPositions :: [(Int, II.DocIndexEntry)] -> IO [(Int, [Int])]
    loadPositions entries = sequence $ map (\(termId, entry) ->
      do
        positions <- II.getPositions entry (fromJust $ indexInvIndex index)
        return (termId, positions)) entries


getRequestDocs :: [Int] -> Index -> IO (IntMap [(Int, II.DocIndexEntry)])
getRequestDocs request index =
  if isNothing $ indexInvIndex index
  then return IntMap.empty
  else do
    let invIndex = fromJust $ indexInvIndex index
    docEntries <- mapM (flip II.performTermSearch invIndex) request
    return $ addAllTerms $ zip request docEntries

  where
    addTermDocs :: Int -> [II.DocIndexEntry] -> IntMap [(Int, II.DocIndexEntry)] -> IntMap [(Int, II.DocIndexEntry)]
    addTermDocs _ [] m = m
    addTermDocs termId (i:rest) m =
      let m2 = addTermDocs termId rest m
          list = IntMap.findWithDefault [] (II.dieDocId i) m2
      in IntMap.insert (II.dieDocId i) ((termId, i):list) m2

    addAllTerms :: [(Int, [II.DocIndexEntry])] -> IntMap [(Int, II.DocIndexEntry)]
    addAllTerms [] = IntMap.empty
    addAllTerms ((i, docs):rest) = addTermDocs i docs $ addAllTerms rest
