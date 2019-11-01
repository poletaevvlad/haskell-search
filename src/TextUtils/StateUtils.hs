module TextUtils.StateUtils(accumState) where

import Control.Monad.State


accumState :: [State s a] -> State s [a]
accumState [] = return []
accumState (x:xs) = do
  st1 <- get
  let (firstVal, st2) = runState x st1
  let (otherVals, st3) = runState (accumState xs) st2
  put st3
  return (firstVal: otherVals)
