module TextUtils.StateUtils(accumState, accumStateT) where

import Control.Monad.State


accumState :: [State s a] -> State s [a]
accumState [] = return []
accumState (x:xs) = do
  st1 <- get
  let (firstVal, st2) = runState x st1
  let (otherVals, st3) = runState (accumState xs) st2
  put st3
  return (firstVal: otherVals)


accumStateT :: (Monad m) => [StateT s m a] -> StateT s m [a]
accumStateT [] = return []
accumStateT (x: xs) = do
  st1 <- get
  (vals, st3) <- lift $ runStateT x st1 >>= \(firstVal, st2) -> do
    (otherVals, st3) <- runStateT (accumStateT xs) st2
    return (firstVal:otherVals, st3)
  put st3
  return vals
