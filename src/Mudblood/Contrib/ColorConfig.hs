module Mudblood.Contrib.ColorConfig
    ( readColorConfig
    ) where

import Text.ParserCombinators.Parsec

import Mudblood

readColorConfig :: String -> IO (MB u ())
readColorConfig f = do
    case parse pFile "" f of
        Left err -> return $ echoE $ "Error reading color config: " ++ show err
        Right a -> return a

pFile :: Parser (MB u ())
pFile = do
    settings <- many pSetting
    eof
    return $ sequence_ settings

pSetting :: Parser (MB u ())
pSetting = do
    name <- anyChar `manyTill` char ':'
    spaces
    col <- anyChar `manyTill` newline
    case name of
        "bg" -> return $ ui $ UISetBgColor col
        _ -> case nameToColor name of
                Just name' -> return $ ui $ UISetColor name' col
                Nothing -> fail "Invalid color id"
