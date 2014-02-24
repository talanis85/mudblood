{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Mudblood.Contrib.Settings
    ( R_Settings (..), mkSettings
    , getSetting, modifySetting
    , guardSetting, guardSettingTrue
    , loadSettings
    ) where

import Data.Has
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

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

-----------------------------------------------------------------------------

loadSettings :: (Has R_Settings u) => String -> MB u ()
loadSettings f = do
    case parse pSettingsFile "" f of
        Left err -> throwError $ stackTrace "settings" $ "Error reading settings: " ++ show err
        Right a -> modifySettings $ const a

pSettingsFile = do
    settings <- many pSetting
    return $ M.fromList settings

pSetting = do
    many newline
    name <- many1 $ noneOf " =\n"
    spaces
    char '='
    spaces
    value <- choice [ pBoolValue, pIntValue, pStringValue ]
    many newline
    return (name, value)

pStringValue = fmap userValueFromString $ anyChar `manyTill` newline

pBoolValue = fmap UserValueBool $ choice
    [ string "yes"      >> return True
    , string "no"       >> return False
    , string "true"     >> return True
    , string "false"    >> return False
    ]

pIntValue = do
    d <- many1 digit
    case reads d of
        [] -> fail "Expected number"
        ((x,_):_) -> return $ userValueFromInt x
