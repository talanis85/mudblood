import Mudblood
import Mudblood.Screen.Gtk

import Mudblood.User.Regex

import Data.Array
import Data.Dynamic

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad hiding (guard)

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG
import Mudblood.Paths

import Mudblood.User.Trigger
import Mudblood.Mapper.Map
import Mudblood.Mapper.Walk

-- COMMANDS ------------------------------------------------------------

commands = M.fromList
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
    , ("guild", cmdGuild)
    , ("focus", cmdFocus)
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
                            lift $ modifyMap $ step first
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
        settings <- lift getUserData >>= return . mgSettings
        newSettings <- mgSettingsPut settings option
        lift $ modifyUserData $ \s -> s { mgSettings = newSettings }
      )
    ]

-- TRIGGERS -----------------------------------------------------------

triggers = fightColorizer :>>: zaubererreportTrigger defaultZaubererStatus :>>: colorTriggers :>>: moveTrigger

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

    -- Read rc file
    mbpath <- mb $ io $ initUserPath []
    rcfile <- mb $ io $ readUserFile (mbpath </> "rc")
    case rcfile of
        Nothing -> return ()
        Just file -> forM_ (filter (/= "") (lines file)) $ mb . command

    screen

main :: IO ()
main = execScreen (MBConfig commands) (mkMBState (Just triggers) newMGState) boot
