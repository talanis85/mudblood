{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Mudblood.Contrib.Settings
    ( R_Settings (..), mkSettings
    , getSetting, modifySetting
    , guardSetting, guardSettingTrue
    ) where

import Data.Has
import qualified Data.Map as M

import Mudblood

data R_Settings = R_Settings
type instance TypeOf R_Settings = UserData

mkSettings = M.empty

getSettings :: (Has R_Settings u, MBMonad m u) => m UserData
getSettings = getUserData >>= (\x -> return (R_Settings ^. x))

getSetting :: (Has R_Settings u, MBMonad m u) => String -> m UserValue
getSetting key = getSettings >>= return . lookupUserValue key

modifySettings :: (Has R_Settings u, MBMonad m u) => (UserData -> UserData) -> m ()
modifySettings f = modifyUserData $ R_Settings ^: f

modifySetting :: (Has R_Settings u, MBMonad m u) => String -> (UserValue -> UserValue) -> m ()
modifySetting key f = modifySettings $ M.alter (Just . f . nothingToNull) key
    where nothingToNull x = case x of
            Nothing -> UserValueNull
            Just x -> x


guardSetting :: (Has R_Settings u) => String -> (UserValue -> Bool) -> MBTrigger u ()
guardSetting key f = do
    s <- getSetting key
    if f s then return () else failT

guardSettingTrue :: (Has R_Settings u) => String -> MBTrigger u ()
guardSettingTrue key = guardSetting key (== UserValueBool True)
