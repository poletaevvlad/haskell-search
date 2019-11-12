module Search.IndexBuilding(IndexBuilder, withIndexBuilder, addDocument) where

import Search.InvertedIndex(DocIndexEntry(..), InvIndexEntry(..), InvertedIndex,
  loadIndex)
import qualified Data.Binary as B
import Control.Monad.State.Lazy
import System.IO
import TextUtils.Processing(getPositions)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import TextUtils.StateUtils (accumStateT)
import Data.Maybe
import Data.ByteString.Lazy (hPut)


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


withIndexBuilder :: FilePath -> StateT IndexBuilder IO () -> IO InvertedIndex
withIndexBuilder path callbackState =
  withBinaryFile (path ++ "/positions.index") WriteMode (\posHandle -> do
    let builder = IndexBuilder { ibPosIndexBuilder = PosIndexBuilder posHandle 0
                               , ibLocation = path
                               , ibDocIndices = IntMap.empty}
    resultBuilder <- execStateT callbackState builder
    commit resultBuilder
    )


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


commit :: IndexBuilder -> IO InvertedIndex
commit builder =
  do
    let PosIndexBuilder handle _ = ibPosIndexBuilder builder
    hClose handle

    let path = ibLocation builder
    _ <- withBinaryFile (path ++ "/docs.index") WriteMode (\docsHandle ->
      withBinaryFile (path ++ "/inv.index") WriteMode (\invHandle ->
        writeToFiles 0 (IntMap.toAscList $ ibDocIndices builder) 0 invHandle docsHandle))
    loadIndex $ ibLocation builder
  where
    writeToFiles :: Int -> [(Int, [DocIndexEntry])] -> Int -> Handle -> Handle -> IO ()
    writeToFiles _ [] _ _ _ = return ()
    writeToFiles currId allEntries@((termId, docEntries):entries) docsOffset invHandle docsHandle
      | currId < termId = do
          hPut invHandle $ B.encode $ InvIndexEntry 0 0
          writeToFiles (currId + 1) allEntries docsOffset invHandle docsHandle
      | otherwise = do
          let docsCount = length docEntries
          hPut invHandle $ B.encode $ InvIndexEntry docsOffset docsCount
          hPut docsHandle $ mconcat $ map B.encode $ reverse docEntries
          writeToFiles (currId + 1) entries (docsOffset + docsCount) invHandle docsHandle
