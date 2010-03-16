{-# OPTIONS -O2 -Wall #-}
module TakeWhile(takeWhileM) where
import Control.Monad(liftM)

takeWhileM :: Monad m => (a -> m Bool) -> m a -> m [a]
takeWhileM cond element = do
  value <- element
  shouldTake <- cond value
  if shouldTake
    then liftM (value:) (takeWhileM cond element)
    else return []
