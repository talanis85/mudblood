module Mudblood.UserData
    ( UserData, UserValue (..)
    , lookupUserValue
    , userValueToInt, userValueFromInt
    , userValueToString, userValueFromString
    , userValueToStringArray, userValueFromStringArray
    , userValue, userFlag, stringAsStringValue, intAsStringValue
    , stringAsStringArray
    , userValueToggle
    , JSUserData (..), userDataFromString, userDataToString
    ) where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Text.Read

import Text.JSON
import Text.JSON.Types

import Control.Lens

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

newtype JSUserData = JSUserData { getJSUserData :: UserData }

instance JSON JSUserData where
    readJSON (JSObject o) = return $ JSUserData $ M.map toUserValue $ M.fromList $ fromJSObject o

    readJSON _ = fail "Expected object"

    showJSON d = showJSON $ toJSObject $ M.toList $ ((M.map fromUserValue (getJSUserData d)) :: M.Map String JSValue)

userDataFromString :: String -> Maybe UserData
userDataFromString str = case decodeStrict str of
    Ok ud -> Just $ getJSUserData ud
    Error _ -> Nothing

userDataToString :: UserData -> String
userDataToString m = encode $ JSUserData m

------------------------------------------------------------------------------

toUserValue JSNull = UserValueNull
toUserValue (JSBool v) = UserValueBool v
toUserValue (JSRational _ v) = UserValueRational v
toUserValue (JSString v) = UserValueString $ fromJSString v
toUserValue (JSArray v) = UserValueArray $ map toUserValue v
toUserValue _ = UserValueNull

fromUserValue UserValueNull = JSNull
fromUserValue (UserValueBool v) = JSBool v
fromUserValue (UserValueRational v) = JSRational True v
fromUserValue (UserValueString v) = JSString $ toJSString v
fromUserValue (UserValueArray v) = JSArray $ map fromUserValue v

------------------------------------------------------------------------------

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
userValueFromString x = if x == "" then UserValueNull else  UserValueString x

userValueToStringArray :: UserValue -> Maybe [String]
userValueToStringArray (UserValueArray a) = Just $ mapMaybe userValueToString a
userValueToStringArray _ = Nothing

userValueFromStringArray :: [String] -> UserValue
userValueFromStringArray = UserValueArray . map UserValueString

userValueToggle :: UserValue -> UserValue
userValueToggle v = case v of
                      UserValueBool v' -> UserValueBool (not v')
                      UserValueNull -> UserValueBool True
                      _ -> v

userValue :: String -> Lens' UserData UserValue
userValue key = lens (lookupUserValue key) (\x y -> M.insert key y x)

userFlag :: String -> Lens' UserData Bool
userFlag key = lens ((== UserValueBool True) . lookupUserValue key) (\x y -> if y then M.insert key (UserValueBool True) x else M.delete key x)

boolAsBoolValue :: Lens' UserValue Bool
boolAsBoolValue = lens (== UserValueBool True) (\_ y -> UserValueBool y)

stringAsStringValue :: Lens' UserValue String
stringAsStringValue = lens (fromMaybe "" . userValueToString) (\_ y -> userValueFromString y)

intAsStringValue :: Lens' UserValue String
intAsStringValue = lens (show . userValueToInt) (\_ y -> userValueFromInt (fromMaybe 0 (readMaybe y)))

stringAsStringArray :: Lens' UserValue [String]
stringAsStringArray = lens (fromMaybe [] . userValueToStringArray) (\_ y -> userValueFromStringArray y)
