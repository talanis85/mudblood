import Mudblood
import Mudblood.Screen.Gtk

import Data.Array
import Data.Dynamic
import Data.List
import Data.Maybe

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad hiding (guard)

import Language.DLisp.Core
import Mudblood.Language

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG

-- COMMANDS ------------------------------------------------------------

cmds :: [(String, Exp MB Value)]
cmds =
    [ ("addprofile", Function ["name"] $ do
        name <- getSymbol "name" >>= typeString
        liftL $ addProfile name
        return nil
      )
    , ("profile", Function ["name"] $ do
        name <- getSymbol "name" >>= typeString
        liftL $ loadProfile name
        return nil
      )
    , ("setcolor", Function ["name", "value"] $ do
        name <- getSymbol "name" >>= typeString
        value <- getSymbol "value" >>= typeString
        case name of
            "bg" -> liftL $ ui $ UISetBgColor value
            "default" -> liftL $ ui $ UISetColor DefaultColor value
            _    -> case nameToColor name of
                        Nothing -> throwError $ "Invalid color: " ++ name
                        Just c -> liftL $ ui $ UISetColor c value
        return nil
        )
    , ("focus", Function ["..."] $ do
        args <- getSymbol "..." >>= typeList
        case args of
            [] -> liftL (getU mgFocus) >>= return . mkStringValue . fromMaybe ""
            (x:[]) -> do
                f <- typeString x
                liftL $ setU mgFocus (if f == "" then Nothing else Just f)
                return $ mkStringValue f
      )
    , ("map.findTag", Function ["tag"] $ do
        tag <- getSymbol "tag" >>= typeString
        map <- liftL $ getMap
        case findRoomsWith (\x -> getUserValue "tag" (roomUserData x) == tag) map of
            [] -> return nil
            (destroom:_) -> return $ mkIntValue destroom
        )
    , ("map.findHash", Function ["hash"] $ do
        tag <- getSymbol "hash" >>= typeString
        map <- liftL $ getMap
        case findRoomsWith (\x -> getUserValue "hash" (roomUserData x) == tag) map of
            [] -> return nil
            (destroom:_) -> return $ mkIntValue destroom
        )
    , ("map.load", Function ["filename"] $ do
        filename <- getSymbol "filename" >>= typeString
        map <- liftL $ io $ mapFromFile filename
        case map of
            Just map' -> do
                         liftL $ putMap map'
            Nothing -> throwError "File not found"
        return nil
        )
    , ("map.fly", Function ["room"] $ do
        room <- getSymbol "room" >>= typeInt
        liftL $ modifyMap $ mapFly room
        return nil
      )
    , ("map.walk", Function ["destination"] $ do
        map <- liftL $ getMap
        dest <- getSymbol "destination" >>= typeInt

        let map' = mgPrepareMap map

        let walkerFun = const $ return WalkerContinue
        let weightfun edge = max (1 :: Int) (getUserValue "weight" (exitUserData edge))

        case shortestPath map' weightfun (mapCurrentId map) dest of
            []           -> throwError "Path not found"
            (first:path) -> do
                            liftL $ echo $ "Path is: " ++ show (first:path)
                            liftL $ modifyTriggers $ fmap (:>>: (walker map' walkerFun path))
                            liftL $ send first
                            liftL $ modifyMap $ mapStep first
        return nil
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
      :>>: Permanent (moveTrigger mgStepper)

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

    --mb $ connect "mg.mud.de" "4711"
    mb $ connect "openfish" "9999"

    -- Read rc file
    mbpath <- mb $ io $ initUserPath []
    rcfile <- mb $ io $ readUserFile (mbpath </> "rc")
    case rcfile of
        Nothing -> return ()
        Just file -> mb $ command file

    --mb $ initGMCP

    screen

main :: IO ()
main = execScreen (mkMBConfig
        { confGMCPSupports = ["MG.char 1", "comm.channel 1", "MG.room 1"]
        }) (mkMBState (Just triggers) mkMGState cmds) boot
