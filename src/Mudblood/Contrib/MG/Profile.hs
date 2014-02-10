module Mudblood.Contrib.MG.Profile
    ( MGProfile (..)
    , readProfile
    ) where

import Text.ParserCombinators.Parsec

import Mudblood

data MGProfile = MGProfile
    { profChar      :: String
    , profPassword  :: String
    }

mkMGProfile = MGProfile
    { profChar      = ""
    , profPassword  = ""
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
    [ pStringSetting "char" >>= \x -> return $ \p -> p { profChar = x }
    , pStringSetting "password" >>= \x -> return $ \p -> p { profPassword = x }
    ]

pStringSetting :: String -> Parser String
pStringSetting name = do
    string name
    spaces
    anyChar `manyTill` newline
