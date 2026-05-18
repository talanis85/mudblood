{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Mudblood.Mapper.Json
  ( mapFromString, mapFromFile
  , mapToString
  ) where

import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Graph.Inductive

import Mudblood.Mapper.Map
import Mudblood.UserData

newtype JSRoom a = JSRoom { getJSRoom :: LNode (RoomData a) }
newtype JSExit a = JSExit { getJSExit :: LEdge (ExitData a) }

-- | Load a map from a string in JSON format.
mapFromString :: (FromJSON r, FromJSON e) => String -> Maybe (Map r e)
mapFromString str = case decode (BL8.pack str) of
    Just map -> Just map
    Nothing -> Nothing

-- | Load a map from a file in JSON format.
mapFromFile :: (FromJSON r, FromJSON e) => FilePath -> IO (Maybe (Map r e))
mapFromFile path = catch (readFile path >>= return . mapFromString) errH
    where errH :: IOException -> IO (Maybe (Map r e))
          errH = const $ return Nothing

-----------------------------------------------------------------------------
-- SAVING
-----------------------------------------------------------------------------

mapToString :: (ToJSON r, ToJSON e) => Map r e -> String
mapToString m = BL8.unpack $ encode m

-----------------------------------------------------------------------------
-- JSON ENCODE / DECODE
-----------------------------------------------------------------------------

instance (FromJSON r, FromJSON e) => FromJSON (Map r e) where
  parseJSON (Object o) = mkGraph
    <$> (map getJSRoom <$> o .: "rooms")
    <*> (map getJSExit <$> o .: "exits")
  parseJSON x = prependFailure "Map" (typeMismatch "Object" x)

instance (ToJSON r, ToJSON e) => ToJSON (Map r e) where
  toJSON x = object
    [ "rooms" .= toJSON (map JSRoom (labNodes x))
    , "exits" .= toJSON (map JSExit (labEdges x))
    ]

instance (FromJSON a) => FromJSON (JSRoom a) where
  parseJSON (Object o) = do
    id <- o .: "id"
    userdata <- getJSUserData <$> o .: "userdata"
    value <- o .: "data"
    tag <- o .:? "tag"

    return $ JSRoom (id, RoomData
      { _roomUserData = userdata
      , _roomValue = value
      , _roomTag = tag
      })

  parseJSON x = prependFailure "JSRoom" (typeMismatch "Object" x)

instance (ToJSON a) => ToJSON (JSRoom a) where
  toJSON x =
    let (id, roomdata) = getJSRoom x
    in object
         [ "id" .= toJSON id
         , "userdata" .= toJSON (JSUserData (roomdata ^. roomUserData))
         , "data" .= toJSON (roomdata ^. roomValue)
         , "tag" .= toJSON (roomdata ^. roomTag)
         ]

instance (FromJSON a) => FromJSON (JSExit a) where
  parseJSON (Object o) = do
    src <- o .: "src"
    dest <- o .: "dest"
    layer <- o .: "layer"
    userdata <- getJSUserData <$> o .: "userdata"
    value <- o .: "data"
    key <- o .: "key"
    split <- o .: "split"

    return $ JSExit (src, dest, ExitData
      { _exitLayer = layer
      , _exitUserData = userdata
      , _exitValue = value
      , _exitKey = key
      , _exitSplit = split
      , _exitProvisional = False
      })

  parseJSON x = prependFailure "JSExit" (typeMismatch "Object" x)

instance (ToJSON a) => ToJSON (JSExit a) where
  toJSON x =
    let (src, dest, exitdata) = getJSExit x
    in object
         [ "src" .= toJSON src
         , "dest" .= toJSON dest
         , "layer" .= toJSON (exitdata ^. exitLayer)
         , "userdata" .= toJSON (JSUserData (exitdata ^. exitUserData))
         , "data" .= toJSON (exitdata ^. exitValue)
         , "key" .= toJSON (exitdata ^. exitKey)
         , "split" .= toJSON (exitdata ^. exitSplit)
         ]
