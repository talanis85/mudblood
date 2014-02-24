{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Mudblood.Screen
    ( ScreenClass (..)
    , mb_
    ) where

import Mudblood

class (Monad m) => ScreenClass u m | m -> u where
    mb          :: MB u a -> m (Maybe a)
    bind        :: [Key] -> m () -> m ()
    menu        :: String -> KeyMenu Key (m ()) -> m ()
    prompt      :: String -> (String -> m ()) -> m ()
    setStatus   :: MB u String -> m ()

mb_ :: (ScreenClass u m) => MB u a -> m ()
mb_ x = mb x >> return ()
