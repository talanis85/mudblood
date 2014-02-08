{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Mudblood.Contrib.MG.State
    ( R_Common (..), MGCommonState (..)
    , MGGuild (..)
    , mkMGCommonState
    , getU, getU', modifyU
    , (??~)

    , mgGuild, mgFocus, mgProfile
    , mgCharName, mgCharRace, mgCharPresay, mgCharTitle
    , mgCharWizlevel, mgCharLevel, mgCharGuildLevel, mgCharGuildTitle
    , mgStatLP, mgStatMLP, mgStatKP, mgStatMKP, mgStatVO
    , mgStatFR, mgStatG, mgStatB, mgStatT, mgStatF
    , mgShieldName
    ) where

import Data.Has
import Control.Lens hiding ((^.), (^=))
import qualified Control.Lens as L
import Control.Monad.State hiding (state)

import Mudblood

------------------------------------------------------------------------------

data R_Common = R_Common
type instance TypeOf R_Common = MGCommonState

data MGGuild = MGGuildTanjian | MGGuildZauberer | MGGuildAbenteurer
    deriving (Eq)

data MGCommonState = MGCommonState
    { _mgGuild         :: MGGuild
    , _mgFocus         :: Maybe String
    , _mgProfile       :: String

    , _mgCharName        :: String
    , _mgCharRace        :: String
    , _mgCharPresay      :: String
    , _mgCharTitle       :: String
    , _mgCharWizlevel    :: Int
    , _mgCharLevel       :: Int
    , _mgCharGuildLevel  :: Int
    , _mgCharGuildTitle  :: String

    , _mgStatLP      :: Int
    , _mgStatMLP     :: Int
    , _mgStatKP      :: Int
    , _mgStatMKP     :: Int
    , _mgStatVO      :: Int
    , _mgStatFR      :: String
    , _mgStatG       :: Int
    , _mgStatB       :: Bool
    , _mgStatT       :: Bool
    , _mgStatF       :: Bool

    , _mgShieldName  :: String
    }

mkMGCommonState = MGCommonState
    { _mgGuild           = MGGuildAbenteurer
    , _mgFocus           = Nothing
    , _mgProfile         = ""

    , _mgCharName        = "Jemand"
    , _mgCharRace        = "Etwas"
    , _mgCharPresay      = ""
    , _mgCharTitle       = ""
    , _mgCharWizlevel    = 0
    , _mgCharLevel       = 0
    , _mgCharGuildLevel  = 0
    , _mgCharGuildTitle  = ""

    , _mgStatLP      = 0
    , _mgStatMLP     = 0
    , _mgStatKP      = 0
    , _mgStatMKP     = 0
    , _mgStatVO      = 0
    , _mgStatFR      = ""
    , _mgStatG       = 0
    , _mgStatB       = False
    , _mgStatT       = False
    , _mgStatF       = False

    , _mgShieldName  = "schild"
    }

mgGuild             :: Lens' MGCommonState MGGuild
mgFocus             :: Lens' MGCommonState (Maybe String)
mgProfile           :: Lens' MGCommonState String
mgCharName          :: Lens' MGCommonState String
mgCharRace          :: Lens' MGCommonState String
mgCharPresay        :: Lens' MGCommonState String
mgCharTitle         :: Lens' MGCommonState String
mgCharWizlevel      :: Lens' MGCommonState Int
mgCharLevel         :: Lens' MGCommonState Int
mgCharGuildLevel    :: Lens' MGCommonState Int
mgCharGuildTitle    :: Lens' MGCommonState String
mgStatLP            :: Lens' MGCommonState Int
mgStatMLP           :: Lens' MGCommonState Int
mgStatKP            :: Lens' MGCommonState Int
mgStatMKP           :: Lens' MGCommonState Int
mgStatVO            :: Lens' MGCommonState Int
mgStatFR            :: Lens' MGCommonState String
mgStatG             :: Lens' MGCommonState Int
mgStatB             :: Lens' MGCommonState Bool
mgStatT             :: Lens' MGCommonState Bool
mgStatF             :: Lens' MGCommonState Bool
mgShieldName        :: Lens' MGCommonState String

mgGuild             = lens _mgGuild             $ \s v -> s { _mgGuild = v }
mgFocus             = lens _mgFocus             $ \s v -> s { _mgFocus = v }
mgProfile           = lens _mgProfile           $ \s v -> s { _mgProfile = v }
mgCharName          = lens _mgCharName          $ \s v -> s { _mgCharName = v }
mgCharRace          = lens _mgCharRace          $ \s v -> s { _mgCharRace = v }
mgCharPresay        = lens _mgCharPresay        $ \s v -> s { _mgCharPresay = v }
mgCharTitle         = lens _mgCharTitle         $ \s v -> s { _mgCharTitle = v }
mgCharWizlevel      = lens _mgCharWizlevel      $ \s v -> s { _mgCharWizlevel = v }
mgCharLevel         = lens _mgCharLevel         $ \s v -> s { _mgCharLevel = v }
mgCharGuildLevel    = lens _mgCharGuildLevel    $ \s v -> s { _mgCharGuildLevel = v }
mgCharGuildTitle    = lens _mgCharGuildTitle    $ \s v -> s { _mgCharGuildTitle = v }
mgStatLP            = lens _mgStatLP            $ \s v -> s { _mgStatLP = v }
mgStatMLP           = lens _mgStatMLP           $ \s v -> s { _mgStatMLP = v }
mgStatKP            = lens _mgStatKP            $ \s v -> s { _mgStatKP = v }
mgStatMKP           = lens _mgStatMKP           $ \s v -> s { _mgStatMKP = v }
mgStatVO            = lens _mgStatVO            $ \s v -> s { _mgStatVO = v }
mgStatFR            = lens _mgStatFR            $ \s v -> s { _mgStatFR = v }
mgStatG             = lens _mgStatG             $ \s v -> s { _mgStatG = v }
mgStatB             = lens _mgStatB             $ \s v -> s { _mgStatB = v }
mgStatT             = lens _mgStatT             $ \s v -> s { _mgStatT = v }
mgStatF             = lens _mgStatF             $ \s v -> s { _mgStatF = v }
mgShieldName        = lens _mgShieldName        $ \s v -> s { _mgShieldName = v }

------------------------------------------------------------------------------

-- | Get a specific field from userdata
getU :: (MBMonad m u, Knows a (TypeOf a) u) => a -> m (TypeOf a)
getU a = getUserData >>= (\x -> return (a ^. x))

-- | Get a lens of a field in userdata
getU' :: (MBMonad m u, Knows a (TypeOf a) u) => a -> Getting v (TypeOf a) v -> m v
getU' a l = getU a >>= return . (L.^. l)

-- | Modify a field in the userdata
modifyU :: (MBMonad m u, Knows a (TypeOf a) u) => a -> (TypeOf a -> TypeOf a) -> m ()
modifyU a f = modifyUserData $ a ^: f

-- | Modify a lens if the argument is a Just
l ??~ v = case v of
    Nothing -> id
    Just v  -> l .~ v
