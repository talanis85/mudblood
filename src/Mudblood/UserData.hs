{-# LANGUAGE MultiParamTypeClasses #-}

module Mudblood.UserData
    ( UserData (getUserDataDynamic, modifyUserDataDynamic, putUserDataDynamic,
                getUserData, modifyUserData, putUserData)
    ) where

import Data.Typeable
import Data.Dynamic

-- | Type class for monads that can read and write dynamic user data.
--
-- Minimal complete definition:
--
-- * getUserDataDynamic
--
-- * Either modifyUserDataDynamic or putUserDataDynamic
class (Monad m) => UserData m where
    getUserDataDynamic :: m Dynamic

    modifyUserDataDynamic :: (Dynamic -> Dynamic) -> m ()
    modifyUserDataDynamic f = getUserDataDynamic >>= putUserDataDynamic . f

    putUserDataDynamic :: Dynamic -> m ()
    putUserDataDynamic d = modifyUserDataDynamic (\_ -> d)

    getUserData :: (Typeable a) => m a
    getUserData = do
        ud <- getUserDataDynamic
        case fromDynamic ud of
            Just ud' -> return ud'
            Nothing  -> error "THIS IS A BUG: User data corrupted."

    putUserData :: (Typeable a) => a -> m ()
    putUserData d = putUserDataDynamic (toDyn d)

    modifyUserData :: (Typeable a) => (a -> a) -> m ()
    modifyUserData f = getUserData >>= putUserData . f
