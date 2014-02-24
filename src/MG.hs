{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts #-}

module MG where

import Mudblood

import Data.Has (FieldOf, fieldOf, (:&:), (&))
import Control.Lens hiding ((&))
import qualified Data.Map as M
import Data.Foldable
import Control.Monad.Writer (Writer, tell, execWriter)

import Text.Printf

import Mudblood.Contrib.Logfile
import Mudblood.Contrib.ColorConfig
import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG
import Mudblood.Contrib.Settings
import Mudblood.Screen

-----------------------------------------------------------------------------

type MGState = FieldOf R_Common
           :&: FieldOf R_Mapper
           :&: FieldOf R_Settings

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
          & fieldOf mkMGMapperState
          & fieldOf mkSettings

-----------------------------------------------------------------------------

mapperMenu :: (Has R_Mapper u, ScreenClass u m) => KeyMenu Key (m ())
mapperMenu = KeyMenu
    [ (KAscii 'm', ("Mode", KeyMenu
        [ (KAscii 'f', ("Fixed",    KeyAction $ mb_ $ modifyU R_Mapper $ mapperMode .~ ModeFixed))
        , (KAscii 'o', ("Off",      KeyAction $ mb_ $ modifyU R_Mapper $ mapperMode .~ ModeOff))
        , (KAscii 'a', ("Auto",     KeyAction $ mb_ $ modifyU R_Mapper $ mapperMode .~ ModeAuto))
        , (KAscii 'm', ("Manual",   KeyAction $ mb_ $ modifyU R_Mapper $ mapperMode .~ ModeManual))
        , (KAscii 'u', ("Update",   KeyAction $ mb_ $ modifyU R_Mapper $ mapperMode .~ ModeUpdate))
        ]
      ))
    , (KAscii 'x', ("Build", KeyMenu
        [ (KAscii 'n', ("New room", KeyAction $
            prompt "Exit name" $ \exit -> mb_ $ do
                m <- getMap
                (g, r) <- maybeError (stackTrace "mapper" "Could not create room") $ mapAddRoom $ mapGraph m
                modifyMap $ mapModifyGraph $ const g
                modifyMap $ mapModifyGraph $ mapAddExit (mapCurrentId m) exit r "base"
          ))
        , (KAscii 'c', ("Connect", KeyMenu $
            [ (KAscii 'n', ("Connect to room id", KeyAction $
                prompt "Exit name" $ \exit -> prompt "Room id" $ \room -> mb_ $ do
                    m <- getMap
                    case reads room of
                        [] -> throwError $ stackTrace "mapper" "Invalid room id"
                        ((room', _):_) -> modifyMap $ mapModifyGraph $ mapAddExit (mapCurrentId m) exit room' "base"
              ))
            ]
          ))
        ]
      ))
    , (KAscii 'w', ("Walk", KeyAction $ prompt "Walk" $ mb_ . mgWalkToTag))
    , (KAscii 'b', ("Walk back", KeyAction $ mb_ mgWalkBack))
    , (KAscii 'e', ("Edit exit properties", KeyAction $ prompt "Exit" $ \x -> mb_ $ do
        m <- getMap
        let cur = mapCurrentId m
            currentActions = unparseExitSettings $ mapGetExitData cur x (mapGraph m)
        dialog (UITextDialogDescription currentActions) $ \(UITextDialogResult r) -> do
            newsettings <- eitherError $ mapLeft (stackTrace "mapper") $ parseExitSettings r
            modifyMap $ mapModifyGraph $ mapModifyExitData cur x $ M.union newsettings
      ))
    ]

loadProfile profilepath = do
    profilefile <- readUserFile $ profilepath </> "profile"
    case profilefile of
        Nothing -> return mkMGProfile
        Just f -> do
            case readProfile f of
                Left err -> return mkMGProfile
                Right p -> return p

whenJust x f = case x of
    Nothing -> return ()
    Just x' -> f x'

triggers :: MGProfile -> MBTriggerFlow MGState
triggers profile = execWriter $ do
    whenJust (profLogfile profile) $ tell . logfileTrigger

    tell $ Permanent (keep $ guardTelneg >=> triggerGmcpHello)
    tell $ Permanent (keep $ guardGMCP >=> triggerGmcpStat)
    tell $ Permanent (guardGMCP >=> triggerGmcpCommunication >=> colorize Blue)
    tell $ roomTriggers NoTrigger NoTrigger
    tell $ Permanent quantizeFitness
    tell $ Permanent (pass (guardSettingTrue "colors.combat") >=> colorizeCombat Green Red)

boot userpath profile = do
    -- Mapper menu
    bind [KAscii '\\'] $ menu "Commands" $ KeyMenu
        [ (KTab, ("Mapper", mapperMenu))
        -- , (KAscii 'n', ("NPCDB", npcdbMenu))
        ]

    mb_ $ do
        forM_ (profMap profile) $ \path -> do
            mapfile <- io $ readUserFile path
            m <- maybeError (stackTrace "mg" "Invalid map file") $ mapfile >>= mapFromString
            modifyMap $ const m
            updateHashIndex

    setStatus defaultStatus

    mb_ $ modifySetting "colors.combat" $ const $ UserValueBool True

    mb_ $ do
        colorfile <- io $ readUserFile $ userpath </> "colors"
        case colorfile of
            Nothing -> return ()
            Just f -> do
                act <- io $ readColorConfig f
                act

        -- Connect and send credentials
        connect "mg.mud.de" "4711"
        forM_ (profChar profile) send
        forM_ (profPassword profile) send
