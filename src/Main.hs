import Mudblood
import Mudblood.Screen.Gtk

import Mudblood.User.Regex

import Data.Array
import Data.Dynamic
import Data.GMCP

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad hiding (guard)

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG

import Mudblood.User.Trigger
import Mudblood.Mapper.Map
import Mudblood.Mapper.Walk

-- COMMANDS ------------------------------------------------------------

mgCommands = M.fromList
    [ ("echo", Command ["text"] $ do
        x <- popStringParam
        lift $ echoA $ fst $ decode x defaultAttr
        )
    , ("quit", Command [] $ do
        lift quit
        )
    , ("connect", Command ["host", "port"] $ do
        h <- popStringParam
        p <- popStringParam
        lift $ connect h p
        )
    , ("send", Command ["string"] $ do
        s <- popStringParam
        lift $ send s
        )
    , ("addprofile", Command ["name"] $ do
        popStringParam >>= lift . addProfile
        )
    , ("profile", Command ["name"] $ do
        popStringParam >>= lift . loadProfile
        )
    , ("guild", Command ["name"] $ popStringParam >>= lift . setGuild)
    , ("focus", Command ["name"] $ popStringParam >>= lift . setFocus)
    , ("walk", Command ["destination"] $ do
        map <- lift $ getMap
        dest <- popIntParam

        let walkerFun = const $ return WalkerContinue
        --let walkerFun ev = do
        --    if ev == SendTEvent "next"
        --        then return WalkerContinue            
        --        else return WalkerPause

        case shortestPath map (const 1) (mapCurrentId map) dest of
            []           -> fail "Path not found"
            (first:path) -> do
                            lift $ echo $ "Path is: " ++ show (first:path)
                            lift $ modifyTriggers $ fmap (:>>: (walker walkerFun path))
                            lift $ send first
                            lift $ modifyMap $ mapStep first
        )
    , ("loadmap", Command ["filename"] $ do
        filename <- popStringParam
        map <- lift $ io $ mapFromFile filename
        case map of
            Just map' -> lift $ putMap map'
            Nothing -> lift $ echo "Invalid map file"
        )
    , ("set", Command ["option", "value"] $ do
        option <- popStringParam
        settings <- lift $ getU mgSettings
        newSettings <- mgSettingsPut settings option
        lift $ setU mgSettings newSettings
      )
    ]

-- TRIGGERS -----------------------------------------------------------

gmcpTrigger = Permanent $ \ev -> do
    case ev of
        GMCPTEvent g -> do
            case gmcpModule g of
                "MG.char.base" -> do
                    updateMaybeU (mgChar . mgCharName)      $ getStringField "name" g
                    updateMaybeU (mgChar . mgCharRace)      $ getStringField "race" g
                    updateMaybeU (mgChar . mgCharPresay)    $ getStringField "presay" g
                    updateMaybeU (mgChar . mgCharTitle)     $ getStringField "title" g
                    updateMaybeU (mgChar . mgCharWizlevel)  $ getIntField "wizlevel" g
                "MG.char.info" -> do
                    updateMaybeU (mgChar . mgCharLevel)      $ getIntField "level" g
                    updateMaybeU (mgChar . mgCharGuildLevel) $ getIntField "guild_level" g
                    updateMaybeU (mgChar . mgCharGuildTitle) $ getStringField "guild_title" g
                "MG.char.maxvitals" -> do
                    updateMaybeU (mgStats . mgStatMLP)  $ getIntField "max_hp" g
                    updateMaybeU (mgStats . mgStatMKP)  $ getIntField "max_sp" g
                "MG.char.vitals" -> do
                    updateMaybeU (mgStats . mgStatLP)   $ getIntField "hp" g
                    updateMaybeU (mgStats . mgStatKP)   $ getIntField "sp" g
                "comm.channel" -> case getStringField "msg" g of
                    Just msg -> echoA $ setFg Blue $ toAttrString msg
                    Nothing -> return ()
                _ -> return ()
            return [ev]
        _ -> return [ev]


triggers = gmcpTrigger :>>: fightColorizer :>>: zaubererreportTrigger :>>: colorTriggers :>>: moveTrigger

tanjianBindings = [ ([KF1],  spell "meditation")
                  , ([KF2],  spell "kokoro")
                  , ([KF3],  spell "kami %f")
                  , ([KF4],  spell "kageodori")
                  , ([KF5],  spell "tegatana")
                  , ([KF6],  spell "omamori")
                  , ([KF7],  spell "hayai")
                  , ([KF8],  spell "akshara")
                  , ([KF9],  spell "kaminari %f")
                  , ([KF10], spell "arashi %f")
                  , ([KF11], spell "samusa %f")
                  , ([KF12], spell "kshira %f")
                  ]

boot :: Screen ()
boot =
    do
    bind [KEsc, KBS, KAscii '!', KAscii 'q'] $ mb quit
    bind [KEsc, KBS, KAscii '!', KAscii 'x', KEsc, KBS, KAscii 'q'] $ mb quit
    
    mapM_ (\(a,b) -> bind a (mb b)) tanjianBindings

    mb $ updateWidgetList

    mb $ connect "mg.mud.de" "4711"

    -- Read rc file
    mbpath <- mb $ io $ initUserPath []
    rcfile <- mb $ io $ readUserFile (mbpath </> "rc")
    case rcfile of
        Nothing -> return ()
        Just file -> mb $ commands file

    --mb $ initGMCP

    screen

main :: IO ()
main = execScreen (mkMBConfig
        { confCommands = mgCommands
        , confGMCPSupports = ["MG.char 1", "comm.channel 1", "MG.room 1"]
        }) (mkMBState (Just triggers) mkMGState) boot
