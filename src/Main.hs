{-# LANGUAGE TypeOperators #-}

import Mudblood
import Mudblood.Screen.Gtk

import Data.Array
import Data.Dynamic
import Data.List
import Data.Maybe

import Control.Arrow
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad hiding (guard)

import Language.DLisp.Core
import Mudblood.Language

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG.Prelude
import Mudblood.Contrib.MG.GMCP

import Data.Has

-- COMMANDS ------------------------------------------------------------

cmds :: [(String, Exp (MB u) Value)]
cmds = [
{-
      ("addprofile", Function ["name"] $ do
        name <- getSymbol "name" >>= typeString
        liftL $ addProfile name
        return nil
      )
    , ("profile", Function ["name"] $ do
        name <- getSymbol "name" >>= typeString
        liftL $ loadProfile name
        return nil
      )
      -}
      ("setcolor", Function ["name", "value"] $ do
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
      {-
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
        -}
    ]

-- TRIGGERS -----------------------------------------------------------

colorTriggers = (Permanent $ withLine >>> regex "^<Tanjian>" >>> marr (colorize Blue))
           :>>: (Permanent $ withLine >>> regex "^.+ aus der Ferne\\." >>> marr (colorize Blue))
           :>>: (Permanent $ withLine >>> regex "^Balance " >>> marr (colorize Blue))
           :>>: (Permanent $ triggerLoop (withLine >>> regex "^.+ teilt Dir mit:" >>> (marr $ colorize Blue))
                                         (withLine >>> regex "^ " >>> (marr $ colorize Blue)))
           :>>: (Permanent $ triggerLoop (withLine >>> regex "^\\[[^]]+:[^]]+]" >>> (marr $ colorize Blue))
                                         (withLine >>> regex "^ " >>> (marr $ colorize Blue)))

triggers = Permanent (keep $ withGMCP >>> triggerGmcpStat)
      :>>: Permanent (withLine >>> colorFight)
      :>>: guildTriggers
      :>>: colorTriggers
      :>>: Permanent (moveTrigger mgStepper)
      :>>: Permanent (keep $ withGMCP >>> triggerGmcpRoom)
      :>>: Permanent (keep $ withTelneg >>> triggerGmcpHello)

{-
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
                  -}

boot :: Screen MGState ()
boot =
    do
    bind [KEsc, KBS, KAscii '!', KAscii 'q'] $ mb quit
    bind [KEsc, KBS, KAscii '!', KAscii 'x', KEsc, KBS, KAscii 'q'] $ mb quit
    
    --mapM_ (\(a,b) -> bind a (mb b)) tanjianBindings

    --mb $ connect "mg.mud.de" "4711"
    mb $ connect "nase" "9999"

    -- Read rc file
    mbpath <- mb $ io $ initUserPath []
    rcfile <- mb $ io $ readUserFile (mbpath </> "rc")
    case rcfile of
        Nothing -> return ()
        Just file -> mb $ command file

    --mb $ mapM_ send $ gmcpHello ["MG.char 1", "comm.channel 1", "MG.room 1"]

    screen

type MGState = FieldOf R_Common
           :&: FieldOf R_Mapper
           :&: FieldOf R_Tanjian
           :&: FieldOf R_Zauberer

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
          & fieldOf mkMGMapperState
          & fieldOf mkMGTanjianStats
          & fieldOf mkMGZaubererStats

widgets = do
    com <- commonWidgets
    guild <- guildWidgets
    mapper <- mapperWidgets
    return $ [UIWidgetText "Hallo welt"] ++ com ++ guild ++ mapper

main :: IO ()
main = execScreen mkMBConfig (mkMBState (Just triggers) mkMGState (cmds ++ mgCommands)) widgets boot

