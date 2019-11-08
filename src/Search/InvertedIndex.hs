module Search.InvertedIndex(InvIndexEntry(..), DocIndexEntry(..)) where

import qualified Data.Binary as B
import Data.Word(Word32)
import System.IO
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.ByteString as BS

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

sizeOfDocIndexEntry = 12


data InvertedIndex =
  InvertedIndex { idxLocation :: FilePath
                , idxInvIndex :: IntMap InvIndexEntry
                , idxDocsIndexH :: Handle
                , idxPosIndexH :: Handle }
