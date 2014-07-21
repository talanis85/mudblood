module Control.Trigger.Arrow
    ( Trigger, trig
    , EndoTrigger
    , runEndoTrigger
    , (>:>)
    , static
    ) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category

import Control.Trigger.Monad
import Control.Trigger.Aux

newtype Trigger m a b = Trigger { unTrigger :: a -> TriggerM a b m () }

-- | Construct a trigger from a TriggerM function.
trig f = Trigger $ fmap void f

instance (Monad m) => Category (Trigger m) where
    id = Trigger $ let f = yield >=> f
                   in f
    b . a = Trigger $ \x -> do
        r1 <- lift $ runTriggerM $ (unTrigger a) x
        case r1 of
            Right () -> return ()
            Left (Yield x g) -> do
                r2 <- lift $ runTriggerM $ (unTrigger b) x
                case r2 of
                    Right () -> return ()
                    Left (Yield x g') -> yield x >>= (unTrigger $ Trigger g' . Trigger g)
                    Left Flop -> flop
            Left Flop -> flop

{-
instance (Monad m) => Arrow (Trigger m) where
    arr g = Trigger $ let f = yield . singleton . g >=> f
                      in f
    -- TODO
-}

-----------------------------------------------------------------------------

-- | An EndoTrigger is a special type of trigger with output type = [input type].
type EndoTrigger t m = Trigger m t [t]
type EndoTriggerR t m r = TriggerR t [t] m r

-- | Run an EndoTrigger. Returns a tuple (result, remaining EndoTrigger)
runEndoTrigger :: (Monad m) => EndoTrigger t m -> t -> m ([t], EndoTrigger t m)
runEndoTrigger t x = do
    r <- runTriggerM $ unTrigger t x
    case r of
        Right () -> return ([x], idTrigger)
        Left (Yield x g) -> return (x, Trigger g)
        Left Flop -> return ([x], t)
  where
    idTrigger = Trigger $ let f = yield . singleton >=> f
                          in f

foldTrigger :: (Monad m) => EndoTrigger a m -> [a] -> m (EndoTriggerR a m ())
foldTrigger t l =
    let init = ([], Just t)
        combi (ar, at) x = case at of
            Nothing -> return (ar ++ [x], Nothing)
            Just at -> do
                r <- runTriggerM $ unTrigger at x
                case r of
                    Right ()         -> return (ar, Nothing)
                    Left (Yield y g) -> return (ar ++ y, Just (Trigger g))
                    Left Flop        -> return (ar ++ [x], Just at)

    in do
        (rr, rt) <- foldM combi init l
        case rt of
            Nothing -> return $ Right ()
            Just rt -> return $ Left (Yield rr (unTrigger rt))

-- | Chain EndoTriggers. Result is an EndoTrigger that runs the left trigger first
--   and then feeds each result value to the second trigger.
(>:>) :: (Monad m) => EndoTrigger a m -> EndoTrigger a m -> EndoTrigger a m
a >:> b = Trigger $ \x -> do
    r1 <- lift $ runTriggerM $ unTrigger a x
    case r1 of
        Right () -> unTrigger b x
        Left (Yield y g) -> do
            r2 <- lift $ foldTrigger b y
            case r2 of
                Right () -> yield y >>= g
                Left (Yield y' g') -> yield y' >>= (unTrigger $ Trigger g >:> Trigger g')
                -- Left Flop -> SHOULD NOT HAPPEN??
        Left Flop -> do
            r2 <- lift $ runTriggerM $ unTrigger b x
            case r2 of
                Right () -> yield [x] >>= unTrigger a
                Left (Yield y' g') -> yield y' >>= (unTrigger $ b >:> Trigger g')
                Left Flop -> flop

{-
(>:>) :: (Monad m) => EndoTrigger a m -> EndoTrigger a m -> EndoTrigger a m
a >:> b = Trigger $ \x -> do
    (r1, t1) <- lift $ runEndoTrigger a x
    r2 <- lift $ foldTrigger t1 r1
    case r2 of
        Right () -> yield r1 >>= 
-}

-- | Make an EndoTrigger repeat itself forever.
static :: (Monad m) => EndoTrigger a m -> EndoTrigger a m
static t' = Trigger $ static' t'
    where static' t' = oneIteration t'    
          oneIteration t = \x -> do
            r <- lift $ runTriggerM $ unTrigger t x
            case r of
                Right () -> static' t' x
                Left (Yield x g) -> yield x >>= oneIteration (Trigger g)
                Left Flop -> flop
