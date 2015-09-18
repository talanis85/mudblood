{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Control.Trigger.Parser
  ( P, Parser
  , fetch
  , flushLA
  , parse, parse'
  , parseU, parseU'
  , (<?>), (<?+>)
  , cases, cases', (-->), (==>)
  , yieldP
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free

import Control.Trigger.Core
import Control.Trigger.Iteration

data PF t a =
    PFail
  | PFetch (t -> a)

instance Functor (PF t) where
    fmap f PFail = PFail
    fmap f (PFetch g) = PFetch (fmap f g)

newtype P t m r = P { unP :: FreeT (PF t) (StateT ([t], [t]) (Iteration t m)) r }
  deriving (Monad, Applicative, Functor, MonadFree (PF t))

type Parser = P

instance MonadTrans (P t) where
  lift m = P $ lift $ lift $ lift m

yieldP x = P $ lift $ lift $ yield x

instance (Monad m) => MonadPlus (P t m) where
    mzero = liftF PFail
    a `mplus` b = do
      r <- tryMaybe (try a)
      case r of
        Nothing -> b
        Just r -> return r

instance (Monad m) => Alternative (P t m) where
    empty = mzero
    (<|>) = mplus

fetch :: (Monad m) => P t m t
fetch = P $ liftF $ PFetch id

try :: (Monad m) => P a m r -> P a m r
try p = do
    (la, ru) <- getLA
    putLA [] ru
    r <- tryMaybe p
    (la', ru') <- getLA
    case r of
      Nothing -> putLA la (ru' ++ la') >> mzero
      Just r  -> putLA (la ++ la') ru' >> return r
  where
    getLA = P $ lift get
    putLA la ru = P $ lift $ put (la, ru)

tryMaybe :: (Monad m) => P a m r -> P a m (Maybe r)
tryMaybe p = P $ tryMaybe' (unP p)
  where
    tryMaybe' p = do
      r <- lift $ runFreeT p
      case r of
        Pure r -> return (Just r)
        Free PFail -> return Nothing
        Free (PFetch f) -> FreeT (return $ Free (PFetch (\x -> tryMaybe' $ f x)))

flushLA :: (Monad m) => P a m r -> P a m (r, [a])
flushLA p = do
  r <- p
  (la, ru) <- P $ lift get
  P $ lift $ put (la, [])
  return (r, ru)

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

parseU = parse
parseU' = parse'

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
