{-# LANGUAGE ScopedTypeVariables #-}

module TextUtils.FileUtils(loadFromFile, parseByteString) where

import System.IO(Handle)
import qualified Data.Binary as B
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS


parseByteString :: B.Binary b => Int -> ByteString -> [b]
parseByteString size bytes
 | BS.length bytes < fromIntegral size = []
 | otherwise = let rest = parseByteString size $ BS.drop (fromIntegral size) bytes
               in B.decode bytes : rest


loadFromFile :: B.Binary b => Handle -> Int -> Int -> IO [b]
loadFromFile handle size count =
  parseByteString size <$> BS.hGet handle (size * count)
