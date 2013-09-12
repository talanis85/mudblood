import Mudblood
import Mudblood.Screen.Gtk

import Data.Array
import Data.Dynamic

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad hiding (guard)

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG

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
    , ("focus", Command ["name"] $ popStringParam >>= lift . setFocus)
    , ("fly", Command ["destination"] $ do
        map <- lift $ getMap
        dest <- popStringParam

        case findRoomsWith (\x -> getUserValue "tag" (roomUserData x) == dest) map of
            [] -> fail "Room not found"
            (destroom:_) -> lift $ modifyMap $ mapFly destroom
        )
    , ("walk", Command ["destination"] $ do
        map <- lift $ getMap
        dest <- popStringParam

        case findRoomsWith (\x -> getUserValue "tag" (roomUserData x) == dest) map of
            [] -> fail "Room not found"
            (destroom:_) -> do
                let walkerFun = const $ return WalkerContinue
                let weightfun edge = max (1 :: Int) (getUserValue "weight" (exitUserData edge))

                case shortestPath map weightfun (mapCurrentId map) destroom of
                    []           -> fail "Path not found"
                    (first:path) -> do
                                    lift $ echo $ "Path is: " ++ show (first:path)
                                    lift $ modifyTriggers $ fmap (:>>: (walker walkerFun path))
                                    lift $ send first
                                    lift $ modifyMap $ mapStep first
        )
    , ("setcolor", Command ["name", "value"] $ do
        name <- popStringParam
        value <- popStringParam
        case name of
            "bg" -> lift $ ui $ UISetBgColor value
            "default" -> lift $ ui $ UISetColor DefaultColor value
            _    -> case nameToColor name of
                        Nothing -> fail "Invalid color"
                        Just c -> lift $ ui $ UISetColor c value
        )
    , ("loadmap", Command ["filename"] $ do
        filename <- popStringParam
        map <- lift $ io $ mapFromFile filename
        case map of
            Just map' -> lift $ putMap map'
            Nothing -> lift $ echo "Invalid map file"
        )
    ]

-- TRIGGERS -----------------------------------------------------------

colorTriggers = (Permanent $ triggerRegexMultiline "^\\[[^\\]]+:[^\\]]+\\]" (colorize Blue) "^ " (colorize Blue))
           :>>: (Permanent $ triggerRegexLine "^<Tanjian>" >=> colorize Blue)
           :>>: (Permanent $ triggerRegexMultiline "^.+ teilt Dir mit:" (colorize Blue) "^ " (colorize Blue))
           :>>: (Permanent $ triggerRegexLine "^.+ aus der Ferne\\." >=> colorize Blue)
           :>>: (Permanent $ triggerRegexLine "^Balance " >=> colorize Blue)

triggers = Permanent gmcpTrigger
      :>>: Permanent colorFight
      :>>: Permanent reportTrigger
      :>>: guildTriggers
      :>>: colorTriggers
      :>>: Permanent moveTrigger

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
    --mb $ connect "openfish" "9999"

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
