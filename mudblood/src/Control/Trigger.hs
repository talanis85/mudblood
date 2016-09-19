module Control.Trigger
  ( module Control.Trigger.Core
  , module Control.Trigger.Iteration
  , module Control.Trigger.Parser
  , module Control.Trigger.Lift

  , stack
  , guardFirstOf
  , manyTill
  , yieldWhileJust
  ) where

import Control.Trigger.Core
import Control.Trigger.Iteration
import Control.Trigger.Parser
import Control.Trigger.Lift

stack :: (Monad m) => (a -> m b) -> a -> m (a, b)
stack f x = f x >>= \y -> return (x, y)

guardFirstOf :: (MonadPlus m) => [a -> m b] -> a -> m b
guardFirstOf m x = msum (map ($ x) m)

manyTill :: (Monad m, Alternative m) => m r1 -> m r2 -> m [r1]
manyTill p1 p2 = do
    r <- (p2 >> return Nothing) <|> fmap Just p1
    case r of
        Just x -> do
            rest <- manyTill p1 p2
            return (x : rest)
        Nothing -> return []

yieldWhileJust :: (Triggering a b m) => m (Maybe b) -> m ()
yieldWhileJust t = do
  r <- t
  case r of
    Nothing -> return ()
    Just r' -> yield r' >> yieldWhileJust t

