module Mudblood.Contrib.MG.Mapper.UserData
    ( setUserDataHere
    , getUserDataHere
    ) where

import Data.Carte
import qualified Data.Map as M

import Control.Lens
import Control.Monad.State

import Mudblood.Contrib.MG.Mapper.State

import Mudblood

setUserDataHere :: (MonadState (Fix r) m, R :@: r) => String -> Maybe String -> m ()
setUserDataHere key hp = rec . currentRoomData %= updater
  where updater = case hp of
            Nothing -> M.delete key
            Just hp -> M.insert key (UserValueString hp)

getUserDataHere :: (MonadState (Fix r) m, R :@: r) => String -> m (Maybe String)
getUserDataHere key = do
    rd <- use $ rec . currentRoomData
    return $ userValueToString $ lookupUserValue key rd
