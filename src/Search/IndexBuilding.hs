module Search.IndexBuilding(IndexBuilder, createIndexBuilder, addDocument,
  commit) where

import Search.InvertedIndex(DocIndexEntry(..))
import Data.ByteString.Lazy(hPut)
import qualified Data.Binary as B
import Control.Monad.State.Lazy
import System.IO
import TextUtils.Processing(getPositions)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import TextUtils.StateUtils (accumStateT)
import Data.Maybe


data PosIndexBuilder = PosIndexBuilder Handle Int


addPositions :: Int -> [Int] -> StateT PosIndexBuilder IO DocIndexEntry
addPositions docId positions =
  do
    let len = length positions
    PosIndexBuilder handle offset <- get
    put $ PosIndexBuilder handle (offset + len)

    lift $ do
      let d = mconcat $ map B.encode (map fromIntegral positions :: [B.Word32])
      hPut handle d
      return $ DocIndexEntry docId offset len


data IndexBuilder =
  IndexBuilder { ibPosIndexBuilder :: PosIndexBuilder
               , ibLocation :: FilePath
               , ibDocIndices :: IntMap [DocIndexEntry]}


createIndexBuilder :: FilePath -> IO IndexBuilder
createIndexBuilder path = do
  posHandle <- openBinaryFile (path ++ "/positions.index") WriteMode
  return IndexBuilder { ibPosIndexBuilder = PosIndexBuilder posHandle 0
                      , ibLocation = path
                      , ibDocIndices = IntMap.empty}


addDocument :: Int -> [Int] -> StateT IndexBuilder IO ()
addDocument docId termIds =
  do builder <- get
     newState <- lift $ execStateT (accumStateT $ map addItemEntry $ getPositions termIds) builder
     put newState
     return ()
  where
    addItemEntry :: (Int, [Int]) -> StateT IndexBuilder IO ()
    addItemEntry (termId, positions) = do
      builder <- get
      (docIndexEntry, posBuilder) <- lift $ runStateT (addPositions docId positions) $ ibPosIndexBuilder builder
      let newMap = IntMap.alter (prependDocIndex docIndexEntry) termId $ ibDocIndices builder
      put builder{ ibDocIndices = newMap, ibPosIndexBuilder = posBuilder }

    prependDocIndex :: DocIndexEntry -> Maybe [DocIndexEntry] -> Maybe [DocIndexEntry]
    prependDocIndex entry maybeList = Just $ entry:(fromMaybe [] maybeList)


commit :: IndexBuilder -> IO ()
commit builder = do
  let PosIndexBuilder handle _ = ibPosIndexBuilder builder
  hClose handle
