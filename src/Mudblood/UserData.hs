module Mudblood.UserData
    ( UserData
    , lookupUserValue
    , userValueToInt, userValueFromInt
    , userValueToString, userValueFromString
    , userValueToStrinArray, userValueFromStringArray
    ) where

import qualified Data.Map as M

type UserData = M.Map String UserValue

data UserValue = UserValueNull
               | UserValueBool Bool
               | UserValueRational Rational
               | UserValueString String
               | UserValueArray [UserValue]
    deriving (Eq, Ord)

instance Show UserValue where
    show UserValueNull = "<null>"
    show (UserValueBool v) = show v
    show (UserValueRational v) = show v
    show (UserValueString v) = v
    show (UserValueArray v) = concat $ intersperse "," (map show v)

lookupUserValue :: String -> UserData -> UserValue
lookupUserValue = M.findWithDefault UserValueNull

userValueToInt :: UserValue -> Maybe Int
userValueToInt (UserValueRational v) = Just $ round v
userValueToInt _ = Nothing

userValueFromInt :: Int -> UserValue
userValueFromInt = UserValueRational . fromIntegral

userValueToString :: UserValue -> Maybe String
userValueToString (UserValueString v) = Just v
userValueToString _ = Nothing

userValueFromString :: String -> UserValue
userValueFromString = UserValueString

userValueToStringArray :: UserValue -> Maybe [String]
userValueToStringArray (UserValueArray a) = Just $ mapMaybe userValueToString a

userValueFromStringArray :: [String] -> UserValue
userValueFromStringArray = UserValueArray . map UserValueString
