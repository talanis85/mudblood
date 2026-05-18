module Mudblood.UserData
    ( UserData, UserValue (..)
    , lookupUserValue
    , userValueToInt, userValueFromInt
    , userValueToString, userValueFromString
    , userValueToStringArray, userValueFromStringArray
    , userValue, userValueDeleteNull, userFlag, stringAsStringValue, intAsStringValue
    , stringAsStringArray
    , maybeStringAsString
    , userValueToggle
    , JSUserData (..) -- , userDataFromString, userDataToString
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Data.String

import Control.Lens hiding ((.=))

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

{-
instance JSON JSUserData where
    readJSON (JSObject o) = return $ JSUserData $ M.map toUserValue $ M.fromList $ fromJSObject o

    readJSON _ = fail "Expected object"

    showJSON d = showJSON $ toJSObject $ M.toList $ ((M.map fromUserValue (getJSUserData d)) :: M.Map String JSValue)
-}

instance FromJSON JSUserData where
  parseJSON (Object o) = JSUserData <$> M.fromList <$> mapM f (KM.toList o)
    where
      f (k, v) = case toJSON k of
                   String t -> return (T.unpack t, toUserValue v)
                   _ -> fail "Invalid key"

instance ToJSON JSUserData where
  toJSON (JSUserData x) = Object $ KM.fromList $ map f $ M.toList x
    where
      f (k, v) = (fromString k, fromUserValue v)

{-
userDataFromString :: String -> Maybe UserData
userDataFromString str = case decodeStrict str of
    Left ud -> Just $ getJSUserData ud
    Error _ -> Nothing

userDataToString :: UserData -> String
userDataToString m = encode $ JSUserData m
-}

------------------------------------------------------------------------------

toUserValue Null = UserValueNull
toUserValue (Bool v) = UserValueBool v
toUserValue (Number v) = UserValueRational $ toRational v
toUserValue (String v) = UserValueString $ T.unpack v
toUserValue (Array v) = UserValueArray $ map toUserValue $ V.toList v
toUserValue _ = UserValueNull

fromUserValue UserValueNull = Null
fromUserValue (UserValueBool v) = Bool v
fromUserValue (UserValueRational v) = Number $ fromRational v
fromUserValue (UserValueString v) = String $ T.pack v
fromUserValue (UserValueArray v) = Array $ V.fromList $ map fromUserValue v

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

userValueDeleteNull :: String -> Lens' UserData UserValue
userValueDeleteNull key = lens (lookupUserValue key) $ \x y -> case y of
  UserValueNull -> M.delete key x
  _ -> M.insert key y x

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

maybeStringAsString :: Lens' (Maybe String) String
maybeStringAsString = lens to from
  where
    to Nothing = ""
    to (Just x) = x
    from _ "" = Nothing
    from _ x = Just x
