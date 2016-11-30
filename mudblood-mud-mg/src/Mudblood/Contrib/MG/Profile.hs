module Mudblood.Contrib.MG.Profile
    ( MGProfile (..)
    , readProfile
    , mkMGProfile
    ) where

import Text.ParserCombinators.Parsec

import Mudblood

data MGProfile = MGProfile
    { profChar      :: Maybe String
    , profPassword  :: Maybe String
    , profLogfile   :: Maybe FilePath
    , profMap       :: Maybe FilePath
    }

mkMGProfile = MGProfile
    { profChar      = Nothing
    , profPassword  = Nothing
    , profLogfile   = Nothing
    , profMap       = Nothing
    }

readProfile :: String -> Either String MGProfile
readProfile f = do
    case parse pFile "" f of
        Left err -> Left $ "Error reading profile: " ++ show err
        Right a -> Right a

pFile :: Parser MGProfile
pFile = do
    settings <- many pSetting
    eof
    return $ foldr ($) mkMGProfile settings

pSetting :: Parser (MGProfile -> MGProfile)
pSetting = choice
    [ pStringSetting "char" >>= \x -> return $ \p -> p { profChar = Just x }
    , pStringSetting "password" >>= \x -> return $ \p -> p { profPassword = Just x }
    , pStringSetting "logfile" >>= \x -> return $ \p -> p { profLogfile = Just x }
    , pStringSetting "map" >>= \x -> return $ \p -> p { profMap = Just x }
    ]

pStringSetting :: String -> Parser String
pStringSetting name = do
    string name
    spaces
    anyChar `manyTill` newline
