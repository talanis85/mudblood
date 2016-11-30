module Mudblood.Contrib.MG.Settings
  ( R, base, component
  , save, load
  , getStringSetting
  {-
  , echoAs, yieldLineAs
  , cmdSettings
  -}
  ) where

import Data.Maybe
import Data.Monoid
import Data.List
import Data.Vinyl.Open
import qualified Data.Map as Map
import qualified Data.String.Utils as Str

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Lens

import Mudblood

------------------------------------------------------------------------------

data R_Settings = R_Settings
type instance TypeOf R_Settings = UserData

base = olens (Proxy :: Proxy R_Settings)

mkSettings :: UserData
mkSettings = Map.empty

------------------------------------------------------------------------------

saveSettings :: (MonadIO m, MonadState (ORec r) m, Has R_Settings r) => String -> m ()
saveSettings fp = do
  settings <- use base
  liftIO $ writeFile fp $ userDataToString settings

loadSettings :: (Screen s, MonadIO s, Has R_Settings r) => String -> MB (ORec r) s ()
loadSettings fp = do
  f <- liftIO (try (readFile fp) :: IO (Either SomeException String))
  case f of
    Left err -> echoError $ stackTrace "settings" (show err)
    Right f -> case userDataFromString f of
      Nothing -> return ()
      Just ud -> base .= ud

prefixKeys :: String -> [String]
prefixKeys key = map (Str.join ".") $ tail $ inits $ Str.split "." key

getStringSetting :: (MonadState (ORec r) m, Has R_Settings r) => String -> String -> m String
getStringSetting key def = do
  s <- use $ base
  let match = getFirst $ mconcat $ map tryMatch $ reverse $ prefixKeys key
      tryMatch k = First $ Map.lookup k s >>= userValueToString
  return $ fromMaybe def match

{-
echoAs :: (Has R_Settings r, Screen s) => String -> String -> MB (ORec r) s ()
echoAs t s = do
  colorString <- getStringSetting ("colors." ++ t) "default"
  case parseColor colorString of
    Nothing -> echo $ toAS s
    Just c  -> echo $ setFg c $ toAS s

yieldLineAs :: (MonadState (ORec r) m, Has R_Settings r, LineEvent :<: a) => String -> String -> Trigger (Ev a) m ()
yieldLineAs t s = do
  colorString <- lift $ getStringSetting ("colors." ++ t) "default"
  case parseColor colorString of
    Nothing -> yieldLine $ toAS s
    Just c  -> yieldLine $ setFg c $ toAS s

-- cmdSettings :: (MonadState (ORec r) m, Has R_Settings r, SendEvent :<: a, LineEvent :<: a) => Trigger (Ev a) m ()
cmdSettings = permanent cmdSet0 >--> permanent cmdSet1 >--> permanent cmdSet2 >--> permanent cmdUnset
  where
    cmdSet0 = do
      parse $ fetchSendRegex "^#set$"
      s <- lift $ use base
      forM_ (Map.toAscList s) $ \(k, v) -> yieldLine $ toAS $ k ++ " = " ++ show v
    cmdSet1 = do
      key <- parse $ fetchSendRegex1 "^#set ([^ ]+)$"
      s <- lift $ use base
      yieldLine $ toAS $ key ++ " = " ++ show (lookupUserValue key s)
    cmdSet2 = do
      (key, value) <- parse $ fetchSendRegex2 "^#set ([^ ]+) (.+)$"
      lift $ base %= Map.insert key (UserValueString value)
    cmdUnset = do
      key <- parse $ fetchSendRegex1 "^#unset ([^ ]+)$"
      s <- lift $ use base
      lift $ base %= Map.delete key
-}
