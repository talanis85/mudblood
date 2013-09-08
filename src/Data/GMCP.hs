module Data.GMCP
    ( GMCP (..)
    , parseGMCP
    , dumpGMCP
    , getStringField, getIntField
    , module Text.JSON.Types
    ) where

import Text.JSON
import Text.JSON.Types

data GMCP = GMCP
    { gmcpModule :: String
    , gmcpData :: JSValue
    }
  deriving (Eq)

instance Show GMCP where
    show (GMCP mod dat) = "GMCP [" ++ mod ++ "]: " ++ (show dat)

splitModule :: String -> Maybe (String, String)
splitModule str = splitModule' "" str
    where
        splitModule' _ [] = Nothing
        splitModule' mod (' ':xs) = Just (mod, xs)
        splitModule' mod (x:xs) = splitModule' (mod ++ [x]) xs

parseGMCP :: String -> Maybe GMCP
parseGMCP str =
    case splitModule str of
        Nothing -> Nothing
        Just (mod, dat) -> case decode dat of
            Error _ -> Nothing
            Ok dat' -> Just $ GMCP { gmcpModule = mod, gmcpData = dat' }

dumpGMCP :: GMCP -> String
dumpGMCP (GMCP mod dat) = mod ++ " " ++ (encode dat)

-- helpers

getStringField :: String -> GMCP -> Maybe String
getStringField key gmcp =
    case gmcpData gmcp of
        JSObject ob -> case get_field ob key of
            Just (JSString s) -> Just $ fromJSString s
            _ -> Nothing
        _ -> Nothing

getIntField :: String -> GMCP -> Maybe Int
getIntField key gmcp =
    case gmcpData gmcp of
        JSObject ob -> case get_field ob key of
            Just (JSRational _ v) -> Just $ round v
            _ -> Nothing
        _ -> Nothing
