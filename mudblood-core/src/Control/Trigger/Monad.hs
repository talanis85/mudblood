{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

module Control.Trigger.Monad
    ( Trigger (..), runTrigger
    , Void
    , EndoTrigger, FailingTrigger, FailingEndoTrigger
    , Transformer
    , Handler, EndoHandler, FailingHandler, FailingEndoHandler
    , failing
    , MonadTrigger (..)
    , done, feed, keep
    , trig, trig'
    , collate, filterLeft, filterRight
    , (>--->), (>--?>), (>?-->), (>?-?>)
    , (>===>), (>==?>), (>?==>), (>?=?>), (>===*>), (>==?*>), (>?==*>), (>?=?*>)
    , combine
    , (<|||>)
    , Fallible
    , flop, try, try', tryWith

    , mapYield, mapYieldMaybe, mapAwait, mapAwaitMaybe
    ) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.Monad.Error

-----------------------------------------------------------------------------

-- | The trigger monad. Essentially a stream processor that does one
--   element at a time.
data Trigger a b m r =
    Pure  (r)
  | M     (m   (Trigger a b m r))
  | Yield (b,   Trigger a b m r )
  | Await (a -> Trigger a b m r )

-----------------------------------------------------------------------------

-- | Feed a trigger with a list of input values. Returns a list of results and
--   the remaining trigger.
runTrigger :: (Monad m) => Trigger a b m () -> [a] -> m ([b], Trigger a b m ())
runTrigger t i = case t of
    Pure r       -> return ([], t)
    M m          -> m >>= flip runTrigger i
    Yield (x, f) -> do
        (r', t') <- runTrigger f i
        return (x : r', t')
    Await f      -> case i of
        []     -> return ([], Await f)
        (i:is) -> runTrigger (f i) is

-----------------------------------------------------------------------------

trig :: (Monad m) => (a -> b) -> Trigger a b m ()
trig f = await >>= yield . f

trig' :: (Monad m) => (a -> b) -> Trigger a b m r
trig' = forever . trig

-----------------------------------------------------------------------------

data Void

type EndoTrigger a = Trigger a a
type FailingTrigger a b = Trigger a (Either a b)
type FailingEndoTrigger a = Trigger a (Either a a)

type Transformer a b = FailingTrigger a (a,b)

type Handler a b m r = a -> Trigger Void b m r
type EndoHandler a m r = a -> Trigger Void a m r
type FailingHandler a b m r = a -> FailingTrigger Void b m r
type FailingEndoHandler a m r = a -> FailingTrigger Void a m r

-----------------------------------------------------------------------------

instance (Monad m) => Functor (Trigger a b m) where
    fmap f c = case c of
        Pure r        -> Pure  (f r)
        M mc          -> M     (liftM (fmap f) mc)
        Yield fc      -> Yield (fmap  (fmap f) fc)
        Await fc      -> Await (fmap  (fmap f) fc)

instance (Show b, Show r) => Show (Trigger a b m r) where
    show (Pure r) = "Pure " ++ show r
    show (M _) = "M"
    show (Yield (x, _)) = "Yield"
    show (Await _) = "Await"

instance (Monad m) => Applicative (Trigger a b m) where
    pure = Pure
    f <*> x = case f of
        Pure r    -> fmap r x
        M mc      -> M     $ liftM (<*> x) mc
        Yield fc  -> Yield $ fmap  (<*> x) fc
        Await fc  -> Await $ fmap  (<*> x) fc

instance (Monad m) => Monad (Trigger a b m) where
    return = Pure
    m >>= f = case m of
        Pure r      -> f r
        M mc        -> M     $ liftM (>>= f) mc
        Yield fc    -> Yield $ fmap  (>>= f) fc
        Await fc    -> Await $ fmap  (>>= f) fc

instance MonadTrans (Trigger a b) where lift = M . liftM Pure

-----------------------------------------------------------------------------

-- | MTL style transformer class.
class (Monad m) => MonadTrigger a b m | m -> a b where
    await :: m a
    yield :: b -> m ()

instance (Monad m) => MonadTrigger a b (Trigger a b m) where
    yield x = Yield (x, return ())
    await = Await $ return . id

-----------------------------------------------------------------------------

-- | Map a function over the yielded values.
mapYield :: (Monad m) => (b -> c) -> Trigger a b m r -> Trigger a c m r
mapYield f x = case x of
    Pure r       -> Pure r
    M m          -> M (liftM (mapYield f) m)
    Yield (x, g) -> Yield (f x, mapYield f g)
    Await g      -> Await (mapYield f . g)

mapYieldMaybe :: (Monad m) => (b -> Maybe c) -> Trigger a b m r -> Trigger a c m r
mapYieldMaybe f x = case x of
    Pure r       -> Pure r
    M m          -> M (liftM (mapYieldMaybe f) m)
    Yield (x, g) -> case f x of Nothing -> mapYieldMaybe f g
                                Just x  -> Yield (x, mapYieldMaybe f g)
    Await g      -> Await (mapYieldMaybe f . g)

-- | Map a function over the awaited values.
mapAwait :: (Monad m) => (c -> a) -> Trigger a b m r -> Trigger c b m r
mapAwait f x = case x of
    Pure r       -> Pure r
    M m          -> M (liftM (mapAwait f) m)
    Yield (x, g) -> Yield (x, mapAwait f g)
    Await g      -> Await (mapAwait f . g . f)

-- | This is broken.
mapAwaitMaybe :: (Monad m) => (c -> Maybe a) -> Trigger a b m r -> Trigger c b m r
mapAwaitMaybe f x = case x of
    Pure r       -> Pure r
    M m          -> M (liftM (mapAwaitMaybe f) m)
    Yield (x, g) -> Yield (x, mapAwaitMaybe f g)
    Await g      -> let g' x = case f x of
                                Nothing -> Await g'
                                Just x  -> mapAwaitMaybe f $ g x
                    in Await g'

-----------------------------------------------------------------------------

failing :: (Monad m) => Trigger a b m r -> Trigger a (Either c b) m r
failing = mapYield Right

-----------------------------------------------------------------------------

infixr 1 >--->, >===>, >===*>
infixr 2 >?-->, >?==>, >--?>, >==?>, >?==*>, >==?*>
infixr 3 >?-?>, >?=?>, >?=?*>

-- | Chain two triggers. Output from the first is fed to the second.
(>--->) :: (Monad m) => EndoTrigger a m r -> EndoTrigger a m r -> EndoTrigger a m r
a >---> b = filterRight $ failing a >?-?> failing b

(>?-->) :: (Monad m) => FailingEndoTrigger a m r -> EndoTrigger a m r -> FailingEndoTrigger a m r
a >?--> b = a >?-?> failing b

(>--?>) :: (Monad m) => EndoTrigger a m r -> FailingEndoTrigger a m r -> FailingEndoTrigger a m r
a >--?> b = failing a >?-?> b

(>?-?>) :: (Monad m) => FailingEndoTrigger a m r -> FailingEndoTrigger a m r -> FailingEndoTrigger a m r
a >?-?> b = case (a, b) of
    ( Pure r,        b             ) -> b
    ( a,             Pure r        ) -> a
    ( Yield (x, f),  Await g       ) -> case x of Left x  -> Yield (Left x, f >?-?> b)
                                                  Right x -> f >?-?> g x
    ( a,             Yield (x, f)  ) -> Yield (x, a >?-?> f)
    ( M m,           b             ) -> lift m >>= (\x -> x >?-?> b)
    ( a,             M m           ) -> lift m >>= (\x -> a >?-?> x)
    ( Await g,       b             ) -> Await (\x -> g x >?-?> b)

collate :: (Monad m) => FailingEndoTrigger a m r -> EndoTrigger a m r
collate = mapYield (either id id)

filterRight :: (Monad m) => Trigger a (Either b c) m r -> Trigger a c m r
filterRight = mapYieldMaybe $ either (const Nothing) Just

filterLeft :: (Monad m) => Trigger a (Either b c) m r -> Trigger a b m r
filterLeft = mapYieldMaybe $ either Just (const Nothing)

-- | Chain two triggers. Output from the first is fed to the second. This is the categorical
--   composition of triggers with the identity being:
--
--   @ forever (await >>= yield) @
(>===>) :: (Monad m) => Trigger a b m r -> Trigger b c m r -> Trigger a c m r
a >===> b = filterRight $ failing a >?=?> failing b

(>?==>) :: (Monad m) => Trigger a (Either a b) m r -> Trigger b c m r -> Trigger a (Either a c) m r
a >?==> b = a >?=?> failing b

(>==?>) :: (Monad m) => Trigger a b m r -> Trigger b (Either a c) m r -> Trigger a (Either a c) m r
a >==?> b = failing a >?=?> b

(>?=?>) :: (Monad m) => Trigger a (Either a b) m r -> Trigger b (Either a c) m r -> Trigger a (Either a c) m r
a >?=?> b = case (a, b) of
    ( a,             Yield (x, f)  ) -> Yield (x, a >?=?> f)
    ( a,             M m           ) -> lift m >>= (\x -> a >?=?> x)
    ( M m,           b             ) -> lift m >>= (\x -> x >?=?> b)
    ( Yield (x, f),  Await g       ) -> case x of Left x'  -> Yield (Left x', f >?=?> b)
                                                  Right x' -> f >?=?> g x'
    ( a,             Pure r        ) -> Pure r
    ( Await g,       b             ) -> Await (\x -> g x >?=?> b)
    ( Pure r,        b             ) -> Pure r

(>===*>) :: (Monad m) => Trigger a b m r -> Trigger b c m r -> Trigger a c m r
a >===*> b = a >===> forever b

(>?==*>) :: (Monad m) => Trigger a (Either a b) m r -> Trigger b c m r -> Trigger a (Either a c) m r
a >?==*> b = a >?==> forever b

(>==?*>) :: (Monad m) => Trigger a b m r -> Trigger b (Either a c) m r -> Trigger a (Either a c) m r
a >==?*> b = a >==?> forever b

(>?=?*>) :: (Monad m) => Trigger a (Either a b) m r -> Trigger b (Either a c) m r -> Trigger a (Either a c) m r
a >?=?*> b = a >?=?> forever b

feed :: (Monad m) => a -> Trigger a b m r -> Trigger a b m r
feed x t = case t of
    Pure r       -> Pure r
    M m          -> lift m >>= feed x
    Yield (y, g) -> Yield (y, feed x g)
    Await g      -> g x

-----------------------------------------------------------------------------

(<|||>) :: (Monad m) => Trigger a b m r -> Trigger a b m r -> Trigger a b m r
a <|||> b = case (a, b) of
    ( Await g,      Await g'     ) -> await >>= (\x -> g x <|||> g' x)
    ( Pure r,       b            ) -> b
    ( M m,          b            ) -> lift m >>= (\x -> x <|||> b)
    ( Yield (x, f), b            ) -> yield x >> (f <|||> b)
    ( a,            Pure r       ) -> a
    ( a,            M m          ) -> lift m >>= (\x -> a <|||> x)
    ( a,            Yield (x, f) ) -> yield x >> (a <|||> f)

combine :: (Monad m) => Trigger a c m r -> Trigger b c m r -> Trigger (a, b) c m r
combine a b = case (a, b) of
    ( M m,          b            ) -> lift m >>= (\x -> combine x b)
    ( a,            M m          ) -> lift m >>= (\x -> combine a x)
    ( Yield (x, g), b            ) -> yield x >> combine g b
    ( a,            Yield (x, g) ) -> yield x >> combine a g
    ( Await f,      Await g      ) -> await >>= (\(x,y) -> combine (f x) (g y))
    ( a,            Pure r       ) -> Pure r
    ( Pure r,       b            ) -> Pure r

-----------------------------------------------------------------------------

instance (Monad m, Monoid r) => Monoid (Trigger a a m r) where
    mempty = Pure mempty
    mappend = (>--->)

-----------------------------------------------------------------------------

-- | Fallible triggers. This is actually the same as MaybeT with 'fail' removed.
newtype Fallible m a = Fallible { runFallible :: m (Maybe a) }

instance (Functor m) => Functor (Fallible m) where
    fmap f = Fallible . fmap (fmap f) . runFallible

instance (Monad m) => Monad (Fallible m) where
    return = lift . return
    x >>= f = Fallible (runFallible x >>= maybe (return Nothing) (runFallible . f))

-- | Fail in a fallible trigger.
flop :: (Monad m) => Fallible m a
flop = Fallible $ return Nothing

-- | A synonym for @return ()@.
done :: (Monad m) => m ()
done = return ()

instance (Monad m) => MonadPlus (Fallible m) where
    mzero = Fallible $ return Nothing
    mplus x y = Fallible $ do v <- runFallible x
                              case v of
                                  Nothing -> runFallible y
                                  Just _  -> return v


instance MonadTrans Fallible where
    lift x = Fallible $ liftM Just x

instance (MonadTrigger a b m) => MonadTrigger a b (Fallible m) where
    yield x = lift $ yield x
    await = lift await

keep :: (MonadTrigger a a m) => (a -> m r) -> (a -> m r)
keep f x = do
    r <- f x
    yield x
    return r

-- | @try f@ awaits a value and runs @f@. If that fails, yield
--   Nothing and retry.
try :: (Monad m) => (a -> Fallible (Trigger a b m) r) -> FailingTrigger a b m r
try f = do
    x <- await
    mapYield Right (runFallible (f x)) >>= maybe (yield (Left x) >> try f) return

try' :: (Monad m) => (a -> Fallible (Trigger a r m) r) -> FailingTrigger a r m ()
try' f = try f >>= yield . Right

tryWith :: (Monad m) => (a -> Bool) -> FailingTrigger a b m a
tryWith f = try $ \x -> guard (f x) >> return x
