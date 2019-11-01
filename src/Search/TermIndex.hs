module Search.TermIndex(new, add, add', requestId, requestId', getId, lookup) where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.StringMap(StringMap)
import qualified Data.StringMap as SM

data TermIndex = TermIndex Int (StringMap Int)


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
