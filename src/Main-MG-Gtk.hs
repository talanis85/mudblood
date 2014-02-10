{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Main where

import Mudblood
import Mudblood.Screen.Gtk

import Data.Has (FieldOf, fieldOf, (:&:), (&))
import Control.Lens hiding ((&))
import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.Logfile
import Mudblood.Contrib.ColorConfig
import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG

import System.Environment

-----------------------------------------------------------------------------

type MGState = FieldOf R_Common
           :&: FieldOf R_Mapper

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
          & fieldOf mkMGMapperState

-----------------------------------------------------------------------------

triggers userpath = logfileTrigger (userpath </> "logfile")
               :>>: roomTriggers Nothing Nothing
               :>>: quantizeFitness
               :>>: colorizeCombat Green Red

boot userpath profilepath = do
    -- Mapper menu
    let mapperMenu = KeyMenu
            [ (KAscii 'm', ("Mode", KeyMenu
                [ (KAscii 'f', ("Fixed",    KeyAction $ mb $ modifyU R_Mapper $ mapperMode .~ ModeFixed))
                , (KAscii 'o', ("Off",      KeyAction $ mb $ modifyU R_Mapper $ mapperMode .~ ModeOff))
                , (KAscii 'a', ("Auto",     KeyAction $ mb $ modifyU R_Mapper $ mapperMode .~ ModeAuto))
                , (KAscii 'm', ("Manual",   KeyAction $ mb $ modifyU R_Mapper $ mapperMode .~ ModeManual))
                , (KAscii 'u', ("Update",   KeyAction $ mb $ modifyU R_Mapper $ mapperMode .~ ModeUpdate))
                ]
              ))
            , (KAscii 'x', ("Build", KeyMenu
                [ (KAscii 'n', ("New room", KeyAction $
                    prompt "Exit name" $ \exit -> mb $ do
                        m <- getMap
                        case mapAddRoom $ mapGraph m of
                            Nothing -> echoE "Could not create room"
                            Just (g, r) -> do
                                modifyMap $ mapModifyGraph $ const g
                                modifyMap $ mapModifyGraph $ mapAddExit (mapCurrentId m) exit r "base"
                  ))
                , (KAscii 'c', ("Connect", KeyMenu $
                    [ (KAscii 'n', ("Connect to room id", KeyAction $
                        prompt "Exit name" $ \exit -> prompt "Room id" $ \room -> mb $ do
                            m <- getMap
                            case reads room of
                                [] -> echoE "Invalid room id"
                                ((room', _):_) -> modifyMap $ mapModifyGraph $ mapAddExit (mapCurrentId m) exit room' "base"
                      ))
                    ]
                  ))
                ]
              ))
            , (KAscii 'w', ("Walk", KeyAction $ prompt "Walk" $ mb . mgWalkToTag))
            , (KAscii 'b', ("Walk back", KeyAction $ mb $ mgWalkBack))
            , (KAscii 'e', ("Edit exit properties", KeyAction $ prompt "Exit" $ \x -> mb $ do
                m <- getMap
                let cur = mapCurrentId m
                    currentActions = unparseExitSettings $ mapGetExitData cur x (mapGraph m)
                dialog (UITextDialogDescription currentActions) $ \(UITextDialogResult r) ->
                    case parseExitSettings r of
                        Right newsettings -> modifyMap $ mapModifyGraph $ mapModifyExitData cur x $ M.union newsettings
                        Left err -> echoE err
              ))
            ]

    bind [KAscii '\\'] $ menu "Commands" $ KeyMenu
        [ (KTab, ("Mapper", mapperMenu))
        -- , (KAscii 'n', ("NPCDB", npcdbMenu))
        ]

    mb $ do
        connect "mg.mud.de" "4711"

        case profilepath of
            Nothing -> return ()
            Just profilepath -> do
                profilefile <- io $ readUserFile $ profilepath </> "profile"
                case profilefile of
                    Nothing -> return ()
                    Just f -> do
                        case readProfile f of
                            Left err -> echoE err
                            Right p -> do
                                send $ profChar p
                                send $ profPassword p

        colorfile <- io $ readUserFile $ userpath </> "colors"
        case colorfile of
            Nothing -> return ()
            Just f -> do
                act <- io $ readColorConfig f
                act

    screen (return ())

main :: IO ()
main = do
    args <- getArgs
    userpath <- initUserPath []
    profpath <- case args of
        [profile] -> do
            profpath <- initUserPath ["mg", profile]
            return $ Just profpath
        _ -> return Nothing
    execScreen "res/gui.glade" mkMBConfig (mkMBState (Just $ triggers userpath) mkMGState) (boot userpath profpath)
