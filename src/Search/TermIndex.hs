module Search.TermIndex(TermIndex, new, add, add', requestId, requestId', getId,
  lookup, null, loadIndex, saveIndex) where

import Prelude hiding (lookup, null)
import Control.Monad.State
import Data.StringMap(StringMap)
import qualified Data.Binary as B
import qualified Data.StringMap as SM
import qualified Data.ByteString.Lazy as BS
import System.Directory (doesFileExist)

data TermIndex = TermIndex Int (StringMap Int)

instance B.Binary TermIndex where
  put (TermIndex _ strMap) = B.put strMap
  get = do
    strMap <- B.get :: B.Get (StringMap Int)
    return $ TermIndex (foldl max (-1) strMap + 1) strMap


new :: TermIndex
new = TermIndex 0 SM.empty


add :: String -> Int -> State TermIndex ()
add key value = do
  TermIndex nextId strMap <- get
  put $ TermIndex (max nextId $ value + 1) $ SM.insert key value strMap
  return ()


add' :: String -> Int -> TermIndex -> TermIndex
add' key value = execState $ add key value


requestId :: String -> State TermIndex Int
requestId key = do
  TermIndex nextId strMap <- get
  if key `SM.member` strMap
    then return $ strMap SM.! key
    else do
      put $ TermIndex (nextId + 1) (SM.insert key nextId strMap)
      return nextId

requestId' :: String -> TermIndex -> (Int, TermIndex)
requestId' key = runState $ requestId key


getId :: String -> State TermIndex (Maybe Int)
getId key = get >>= \(TermIndex _ strMap) -> return $ SM.lookup key strMap

lookup :: String -> TermIndex -> Maybe Int
lookup key index = evalState (getId key) index


null :: TermIndex -> Bool
null (TermIndex _ map) = SM.null map


loadIndex :: FilePath -> IO TermIndex
loadIndex path = do
  let termsFile = path ++ "/terms.index"
  fileExist <- doesFileExist termsFile
  if fileExist
     then B.decode <$> BS.readFile termsFile
     else return new

saveIndex :: FilePath -> TermIndex -> IO ()
saveIndex path index = BS.writeFile (path ++ "/terms.index") $ B.encode index
