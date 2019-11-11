module Search.InvertedIndex(InvIndexEntry(..), DocIndexEntry(..), InvertedIndex,
  loadIndex, performTermSearch, TermSearchResults) where

import qualified Data.Binary as B
import Data.Word(Word32)
import System.IO
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import TextUtils.FileUtils(loadFromFile)

putWord32 :: Int -> B.Put
putWord32 = B.put . (fromIntegral :: Int -> Word32)

getWord32 :: B.Get Int
getWord32 = fromIntegral <$> (B.get :: B.Get Word32)


data InvIndexEntry =
  InvIndexEntry { iieOffset :: Int
                , iieCount :: Int
                } deriving (Show, Eq)

instance B.Binary InvIndexEntry where
  put (InvIndexEntry offset count) = putWord32 offset >> putWord32 count
  get = getWord32 >>= \offset -> getWord32 >>= \count -> return $ InvIndexEntry offset count

sizeOfInvIndexEntry :: Int
sizeOfInvIndexEntry = 8


data DocIndexEntry =
  DocIndexEntry { dieDocId :: Int
                , diePosOffset :: Int
                , diePosCount :: Int
                } deriving (Show, Eq)

instance B.Binary DocIndexEntry where
  put (DocIndexEntry docId offset count) = putWord32 docId >> putWord32 offset >> putWord32 count
  get = do
    docId <- getWord32
    offset <- getWord32
    count <- getWord32
    return $ DocIndexEntry docId offset count

sizeOfDocIndexEntry :: Int
sizeOfDocIndexEntry = 12


data InvertedIndex =
  InvertedIndex { idxLocation :: FilePath
                , idxInvIndex :: IntMap InvIndexEntry
                , idxDocsIndexH :: Handle
                , idxPosIndexH :: Handle }


loadIndex :: FilePath -> IO InvertedIndex
loadIndex path = do
  invIndex <- withBinaryFile (path ++ "/inv.index") ReadMode $ \handle -> do
    fileSize <- fromIntegral <$> hFileSize handle :: IO Int
    let count = fileSize `div` sizeOfInvIndexEntry
    entries <- loadFromFile handle sizeOfInvIndexEntry count :: IO [InvIndexEntry]
    return $ IntMap.fromList $ zip [0..] entries

  docsIndex <- openBinaryFile (path ++ "/docs.index") ReadWriteMode
  posIndex <- openBinaryFile (path ++ "/positions.index") ReadWriteMode
  return InvertedIndex { idxLocation = path
                       , idxInvIndex = invIndex
                       , idxDocsIndexH = docsIndex
                       , idxPosIndexH = posIndex }


type TermSearchResults = [DocIndexEntry]

performTermSearch :: Int -> InvertedIndex -> IO TermSearchResults
performTermSearch termId index =
  let res = IntMap.lookup termId $ idxInvIndex index
      handle = idxDocsIndexH index
  in case res of
    Just (InvIndexEntry offset count) -> do
      hSeek handle AbsoluteSeek $ fromIntegral (offset * sizeOfDocIndexEntry)
      loadFromFile handle sizeOfDocIndexEntry count
    Nothing -> return []
