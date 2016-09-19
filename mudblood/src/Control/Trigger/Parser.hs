{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Trigger.Parser
  ( P, Parser
  , fetch, fetchS
  , feedbackP
  , liftP
  , parseLA
  , parse, parse'
  , (<?>), (<?+>)
  , cases, cases', (-->), (==>)
  , yieldP
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free

import Control.Trigger.Core
import Control.Trigger.Iteration

import Debug.Trace

data PF t a =
    PFail
  | PFetch (t -> a)

instance Functor (PF t) where
  fmap f PFail = PFail
  fmap f (PFetch g) = PFetch (fmap f g)

newtype P a m r = P { unP :: FreeT (PF a) (StateT ([a], [a]) (Iteration a m)) r }
  deriving (Monad, Applicative, Functor, MonadFree (PF a))

type Parser a = P a

instance MonadTrans (P t) where
  lift m = P $ lift $ lift $ lift m

liftP :: (Monad m) => Iteration a m r -> P a m r
liftP = P . lift . lift

yieldP :: (Monad m) => a -> P a m ()
yieldP = liftP . yield

getLA = P $ lift get
putLA la ru = P $ lift $ put (la, ru)

instance (Monad m) => MonadPlus (P t m) where
  mzero = liftF PFail
  a `mplus` b = do
    (la, ru) <- getLA
    putLA [] ru
    r <- try a
    case r of
      Nothing -> do
        (la', ru') <- getLA
        putLA la (ru' ++ la')
        b
      Just r -> return r

instance (Monad m) => Alternative (P t m) where
  empty = mzero
  (<|>) = mplus

fetch :: (Monad m) => P t m t
fetch = P $ liftF $ PFetch id

fetchS :: (MonadState [t] m) => P t m t
fetchS = do
  s <- lift get
  case s of
    [] -> fetch
    (x:xs) -> do
      lift $ put xs
      pushLA x
      return x

try :: (Monad m) => P a m r -> P a m (Maybe r)
try p = P $ try' (unP p)
  where
    try' p = do
      r <- lift $ runFreeT p
      case r of
        Pure r -> return (Just r)
        Free PFail -> return Nothing
        Free (PFetch f) -> FreeT (return $ Free (PFetch (\x -> try' $ f x)))

pushLA :: (Monad m) => a -> P a m ()
pushLA x = P $ lift $ modify $ \(la, ru) -> (la, ru ++ [x])

flushLA :: (Monad m) => P a m [a]
flushLA = do
  (la, ru) <- P $ lift get
  P $ lift $ put (la, [])
  return ru

feedbackP :: (MonadState [t] m) => P t m ()
feedbackP = do
  (la, ru) <- P $ lift get
  P $ lift $ put (la, [])
  lift $ modify (++ ru)
  return ()

parseLA :: (Monad m) => P a m r -> Iteration a m (r, [a])
parseLA p =
  let parse__ = parse_ False [] [] (unP p)
      parse_ consumed la ru p' = do
        (r, (la', ru')) <- runStateT (runFreeT p') (la, ru)
        case r of
          Pure r     -> mapM_ pushback ru' >> return (r, la')
          Free PFail -> if consumed then mapM_ yield la' >> mapM_ yield ru' >> parse__
                                    else fail "Parser failed without consuming anything"
          Free (PFetch f) -> case ru' of
                               [] -> await >>= \x -> parse_ True (la' ++ [x]) ru' (f x)
                               (y:ys) -> parse_ True (la' ++ [y]) ys (f y)
  in parse__

parse :: (Monad m) => P a m r -> Iteration a m r
parse p = parseLA p >>= \(r, la) -> return r

parse' :: (Monad m) => P a m r -> Iteration a m r
parse' p = parseLA p >>= \(r, la) -> mapM_ pushback la >> return r

(<?>) :: (Monad m) => (r -> a) -> P a m r -> T a a m ()
f <?> p = f <&> parse p

(<?+>) :: (Monad m) => (r -> a) -> P a m r -> T a a m ()
f <?+> p = f <&> parse' p

cases :: (Monad m) => P a m (Iteration a m r) -> Iteration a m r
cases p = join $ parse p

cases' :: (Monad m) => P a m (Iteration a m r) -> Iteration a m r
cases' p = join $ parse' p

infixl 4 ==>
(==>) :: (Monad m, Monad n) => m r -> (r -> n r') -> m (n r')
a ==> b = a >>= return . b

infixl 4 -->
(-->) :: (Monad m, Monad n) => m r -> (n r') -> m (n r')
a --> b = a >> return b
