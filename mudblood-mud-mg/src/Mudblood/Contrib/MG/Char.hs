{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Mudblood.Contrib.MG.Char
    ( R
    , component
    , connectC
    , Guild (..)

    , guild, profile
    , name, race, presay, title
    , wizLevel, level, guildLevel, guildTitle
    , lp, mlp, kp, mkp, wimpy, wimpyDir
    , poison, blind, deaf, frog

    , fetchLP
    ) where

import Control.Lens
import Control.Monad.State hiding (state)

import Mudblood

import Data.Carte
import Data.GMCP

import Text.Printf

------------------------------------------------------------------------------

data Guild = GuildTanjian | GuildZauberer | GuildAbenteurer
    deriving (Eq)

instance Show Guild where
    show GuildTanjian = "Tanjian"
    show GuildZauberer = "Zauberer"
    show GuildAbenteurer = "Abenteurer"

data R a = R
    { _guild         :: Guild
    , _profile       :: String

    , _name        :: String
    , _race        :: String
    , _presay      :: String
    , _title       :: String
    , _wizLevel    :: Int
    , _level       :: Int
    , _guildLevel  :: Int
    , _guildTitle  :: String

    , _lp     :: Int
    , _mlp     :: Int
    , _kp      :: Int
    , _mkp     :: Int
    , _wimpy      :: Int
    , _wimpyDir      :: String
    , _poison       :: Int
    , _blind       :: Bool
    , _deaf       :: Bool
    , _frog       :: Bool
    }
  deriving (Functor)

mkSt = R
    { _guild           = GuildAbenteurer
    , _profile         = ""

    , _name        = "Jemand"
    , _race        = "Etwas"
    , _presay      = ""
    , _title       = ""
    , _wizLevel    = 0
    , _level       = 0
    , _guildLevel  = 0
    , _guildTitle  = ""

    , _lp       = 0
    , _mlp      = 0
    , _kp       = 0
    , _mkp      = 0
    , _wimpy    = 0
    , _wimpyDir = ""
    , _poison   = 0
    , _blind    = False
    , _deaf     = False
    , _frog     = False
    }

makeLenses ''R

component :: (Functor l, Monad m, MonadFail m, GMCPEvent :<: e) => MBComponent m e (Fix l) (Fix (R :*: l))
component = describe "Mudblood.MG.Char" $
        stateC mkSt
    >>> triggerC 10 triggerGMCPStats
    >>> statusC status

connectC :: (MBEvent e, Screen m) => String -> String -> String -> String -> MBComponent m e u u
connectC host port username password = bootC $ do
    connect host port
    echo (toAS ("Connected to " ++ host))
    send username
    send password
    raise $ mkEv $ NetworkEvent NetworkConnect

status :: (MonadState (Fix r) m, R :@: r) => m String
status = do
    stat <- use rec
    let lp_  = stat ^. lp
        mlp_ = stat ^. mlp
        kp_  = stat ^. kp
        mkp_ = stat ^. mkp
        vo_  = stat ^. wimpy
        fr_  = stat ^. wimpyDir
        g_   = stat ^. poison
        b_   = if stat ^. blind then "B" else " "
        d_   = if stat ^. deaf then "T" else " "
        f_   = if stat ^. frog then "F" else " "
    return $ printf "%d / %d | %d / %d | v:%d (%s) | g:%d | %s%s%s" lp_ mlp_ kp_ mkp_ vo_ fr_ g_ b_ d_ f_

------------------------------------------------------------------------------

readGuild :: String -> Maybe Guild
readGuild "abenteurer"  = Just GuildAbenteurer
readGuild "tanjian"     = Just GuildTanjian
readGuild "zauberer"    = Just GuildZauberer
readGuild _             = Nothing

-- | Modify a lens if the argument is a Just
l ??~ v = case v of
    Nothing -> id
    Just v  -> l .~ v

triggerGMCPStats = permanent $ parse' $ do
    g <- fetchGMCP
    case gmcpModule g of
        "MG.char.base" ->
            let statfun =
                      (name      ??~ (getStringField "name" g))
                    . (race      ??~ (getStringField "race" g))
                    . (presay    ??~ (getStringField "presay" g))
                    . (title     ??~ (getStringField "title" g))
                    . (wizLevel  ??~ (getIntField "wizlevel" g))
                    . (guild     ??~ (getStringField "guild" g >>= readGuild))
            in lift $ rec %= statfun
        "MG.char.info" ->
            let statfun =
                      (level        ??~ (getIntField "level" g))
                    . (guildLevel   ??~ (getIntField "guild_level" g))
                    . (guildTitle   ??~ (getStringField "guild_title" g))
            in lift $ rec %= statfun
        "MG.char.maxvitals" ->
            let statfun =
                      (mlp     ??~ (getIntField "max_hp" g))
                    . (mkp     ??~ (getIntField "max_sp" g))
            in lift $ rec %= statfun
        "MG.char.vitals" ->
            let statfun =
                      (lp      ??~ (getIntField "hp" g))
                    . (kp      ??~ (getIntField "sp" g))
            in lift $ rec %= statfun
        "MG.char.wimpy" ->
            let statfun =
                      (wimpy    ??~ (getIntField "wimpy" g))
                    . (wimpyDir ??~ (getStringField "wimpy_dir" g))
            in lift $ rec %= statfun
        _ -> mzero

fetchLP = do
  g <- fetchGMCPModule "MG.char.vitals"
  maybe mzero return $ getIntField "hp" g
