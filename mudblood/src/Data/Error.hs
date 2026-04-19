{-# LANGUAGE FlexibleInstances #-}
module Data.Error where

class Error a where
    -- | Creates an exception without a message.
    -- The default implementation is @'strMsg' \"\"@.
    noMsg  :: a
    -- | Creates an exception with a message.
    -- The default implementation of @'strMsg' s@ is 'noMsg'.
    strMsg :: String -> a

    noMsg    = strMsg ""
    strMsg _ = noMsg

instance Error [Char] where
    strMsg s = s
