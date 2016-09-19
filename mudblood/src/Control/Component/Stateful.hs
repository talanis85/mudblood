{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Component.Stateful
  ( StatefulComponent (..)
  , StateEffect (..)
  , pureComponent
  , statefulComponent
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Monoid

-----------------------------------------------------------------------------

{-
data StatefulComponentT eff a b = StatefulComponentT
    { sctComponent :: c
    , sctLens ::
    }

instance (StateEffect eff) => StatefulComponentT eff u1 where
    seEmpty =
-}

{-
pushEffect :: a -> eff (a, u) -> StatefulComponent eff u (a, u)
pushEffect a eff = StatefulComponent
    { effect = eff
    , mkUserState = \u -> (a, u)
    , userState = _2
    }

popEffect :: (StateEffect eff) => StatefulComponent eff u (a, u) -> StatefulComponent eff u u
popEffect c = StatefulComponent
    { effect = seMap meanLens (effect c)
    , mkUserState = id
    , userState = id
    }
  where
    meanLens = lens (mkUserState c) (\x y -> snd y)
-}

-----------------------------------------------------------------------------

data StatefulComponent eff u1 u2 = StatefulComponent
  { effect      :: eff u2
  , mkUserState :: u1 -> u2
  , userState   :: Lens' u2 u1
  }

class StateEffect eff where
  seEmpty :: eff a
  seMap :: Lens' b a -> eff a -> eff b
  seCat :: eff a -> eff a -> eff a

-- This is somewhat missing in 'Control.Category'
instance (Category cat) => Monoid (cat a a) where
    mempty = id
    mappend = (>>>)

instance (StateEffect eff) => Category (StatefulComponent eff) where
  id = StatefulComponent
    { effect       = seEmpty
    , mkUserState  = id
    , userState    = id
    }
  b . a = StatefulComponent
    { effect = (seMap (userState b) $ effect a) `seCat` (effect b)
    , mkUserState = mkUserState b . mkUserState a
    , userState = userState b . userState a
    }

pureComponent :: (StateEffect eff) => eff u -> StatefulComponent eff u u
pureComponent eff = StatefulComponent
    { effect = eff
    , mkUserState = id
    , userState = id
    }

statefulComponent :: (u1 -> u2) -> Lens' u2 u1 -> eff u2 -> StatefulComponent eff u1 u2
statefulComponent mkus us eff = StatefulComponent
    { effect = eff
    , mkUserState = mkus
    , userState = us
    }
