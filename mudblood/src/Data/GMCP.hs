{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.GMCP
    ( GMCP (..)
    , parseGMCP
    , dumpGMCP
    , getStringField, getIntField
    , gmcpCoreHello
    , gmcpCoreSupportsSet
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.String

-- import GHC.Generics
-- import Data.Serialize hiding (encode, decode)

data GMCP = GMCP
    { gmcpModule :: String
    , gmcpData :: Value
    }
  deriving (Eq)

-- instance Serialize GMCP
-- instance Serialize Value
-- instance Serialize Key
-- instance Serialize Object

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
        Just (mod, dat) -> case decode (BL8.pack dat) of
            Nothing -> Nothing
            Just dat' -> Just $ GMCP { gmcpModule = mod, gmcpData = dat' }

dumpGMCP :: GMCP -> String
dumpGMCP (GMCP mod dat) = mod ++ " " ++ (BL8.unpack $ encode dat)

-- standard messages

gmcpCoreHello :: String -> String -> GMCP
gmcpCoreHello client version = GMCP
  { gmcpModule = "Core.Hello"
  , gmcpData = object
      [ "client" .= toJSON client
      , "version" .= toJSON version
      ]
  }

gmcpCoreSupportsSet :: [String] -> GMCP
gmcpCoreSupportsSet supports = GMCP
  { gmcpModule = "Core.Supports.Set"
  , gmcpData = toJSONList $ map toJSON supports
  }

-- helpers

getStringField :: String -> GMCP -> Maybe String
getStringField key gmcp =
    case gmcpData gmcp of
        Object ob -> case KM.lookup (fromString key) ob of
            Just (String s) -> Just $ T.unpack s
            _ -> Nothing
        _ -> Nothing

getIntField :: String -> GMCP -> Maybe Int
getIntField key gmcp =
    case gmcpData gmcp of
        Object ob -> case KM.lookup (fromString key) ob of
            Just (Number v) -> Just $ round v
            _ -> Nothing
        _ -> Nothing
