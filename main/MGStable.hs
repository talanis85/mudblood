{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}

module MGStable where

import Mudblood

import Data.Has (FieldOf, fieldOf, (:&:), (&))
import Control.Lens hiding ((&))
import qualified Data.Map as M
import Data.Foldable
import Data.Monoid
import Control.Monad.Writer (Writer, tell, execWriterT)

import Text.Printf

import Mudblood.Contrib.Logfile
import Mudblood.Contrib.ColorConfig
import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG
import Mudblood.Contrib.Settings
import qualified Mudblood.Contrib.Lisp as L
import Mudblood.Screen

-----------------------------------------------------------------------------

type MGState = FieldOf R_Common
           :&: FieldOf R_Mapper
           :&: FieldOf R_Settings
           :&: FieldOf R_Tanjian
           :&: FieldOf R_Zauberer

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
          & fieldOf mkMGMapperState
          & fieldOf mkSettings
          & fieldOf mkMGTanjianState
          & fieldOf mkMGZaubererState

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

whenJust x f = case x of
    Nothing -> return ()
    Just x' -> f x'

triggers :: MB MGState (MBTriggerFlow MGState)
triggers = execWriterT $ do
    tell $ Permanent (keep $ guardTelneg >=> triggerGmcpHello)
    tell $ Permanent $ keep $ guardGMCP >=> triggerGmcpStat

    tell $ zaubererTriggers :>>: tanjianTriggers

    tell $ Permanent $ triggerCommunication $
         (pass (guardSettingTrue "colors.communication") >=> returnLine . setFg Blue) <||> returnLine

    tell $ roomTriggers NoTrigger NoTrigger
    tell $ Permanent quantizeFitness
    tell $ Permanent (pass (guardSettingTrue "colors.combat") >=> colorizeCombat Green Red)

    logfile <- lift $ fmap userValueToString $ getSetting "logfile"
    whenJust logfile $ tell . logfileTrigger

loadDefaultSettings :: (Has R_Settings u) => MB u ()
loadDefaultSettings = do
    modifySetting "colors.combat" $ const $ UserValueBool True
    modifySetting "colors.communication" $ const $ UserValueBool True

status :: MB MGState String
status = do
    guild <- getU' R_Common mgGuild
    case guild of
        MGGuildTanjian -> tanjianStatus
        MGGuildZauberer -> zaubererStatus
        _ -> defaultStatus

lispHandler :: L.Context (MB MGState) L.Value -> CommandHandler MGState
lispHandler ctx = CommandHandler $ \c -> do
    exp <- eitherError $ mapLeft (stackTrace "parse") $ L.parse L.parseValue c
    res <- L.run ctx exp >>= eitherError . (mapLeft $ stackTrace "run")
    case res of
        L.List [] -> return ()
        _         -> echo $ "-> " ++ show res
    return $ lispHandler ctx

boot :: (ScreenClass MGState s) => String -> String -> s ()
boot userpath profpath = do
    -- Mapper menu
    bind [KAscii '\\'] $ menu "Commands" $ KeyMenu
        [ (KTab, ("Mapper", mapperMenu))
        -- , (KAscii 'n', ("NPCDB", npcdbMenu))
        ]

    mb_ $ do
        setCommandHandler $ lispHandler (L.mbBuiltins <> L.coreBuiltins)
        initfile <- io $ readUserFile $ profpath </> "init"
        case initfile of
            Nothing -> return ()
            Just f -> commands f
    
    mb_ $ do
        loadDefaultSettings
        profile <- io $ readUserFile $ profpath </> "profile"
        forM_ profile $ loadSettings

    mb_ $ do
        trigs <- triggers
        modifyTriggers $ const trigs

    mb_ $ do
        mapfile <- fmap userValueToString $ getSetting "mapper.file"
        forM_ mapfile $ \path -> do
            mapfile <- io $ readUserFile path
            m <- maybeError (stackTrace "mg" "Invalid map file") $ mapfile >>= mapFromString
            modifyMap $ const m
            updateHashIndex

    setStatus status

    mb_ $ do
        colorfile <- io $ readUserFile $ userpath </> "colors"
        case colorfile of
            Nothing -> return ()
            Just f -> do
                act <- io $ readColorConfig f
                act

        -- Connect and send credentials
        connect "mg.mud.de" "4711"

        char <- fmap userValueToString $ getSetting "char.name"
        pw <- fmap userValueToString $ getSetting "char.password"

        forM_ char send
        forM_ pw send
