{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Main where

import Mudblood
import Mudblood.Screen.Vty

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
           :&: FieldOf R_Tanjian
           :&: FieldOf R_Zauberer

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
          & fieldOf mkMGMapperState
          & fieldOf mkMGTanjianState
          & fieldOf mkMGZaubererState

-----------------------------------------------------------------------------

tanjianBindings :: [([Key], MB MGState ())]
tanjianBindings = [ ([KF1],  spell "meditation")
                  , ([KF2],  spell "kokoro")
                  , ([KF3],  spell "kami %f")
                  , ([KF4],  hands 2 >> spell "kageodori"
                             >> trigger (on (guardLine >=> (     regexAS "^Du gibst Dich .+ dem Schattentanz hin"
                                                            <||> regexAS "^Du tanzt doch noch den Schattentanz"
                                                            <||> regexAS "^Du bist zu hastig"
                                                           )) unhands))
                  , ([KF5],  spell "tegatana")
                  , ([KF6],  spell "omamori")
                  , ([KF7],  spell "hayai")
                  , ([KF8],  hands 2 >> spell "akshara"
                             >> trigger (    (on (guardLine >=> (     regexAS "^Du kannst 'akshara' noch nicht wieder anwenden"
                                                                 <||> regexAS "^Du bist zu hastig"
                                                                )) unhands)
                                        <||> (keep $ guardLine >=> regexAS "^Du konzentrierst Dich voll auf Dich selbst")
                                        ))
                  , ([KF9],  spell "kaminari %f")
                  , ([KF10], spell "arashi %f")
                  , ([KF11], spell "samusa %f")
                  , ([KF12], spell "kshira %f")
                  ]

countLines :: MBTriggerFlow u
countLines = Volatile $ statefulT 0 $ keep $ countLines'
    where countLines' = guardLine >=> \_ -> do
                            n <- get
                            echo $ "This was line #" ++ show n
                            modify (+1)

colorWieBitte = Permanent $ guardLine >=> (keep1 $ regexAS "^Wie bitte\\?$") >=> returnLine . setFg Magenta

triggers userpath = logfileTrigger (userpath </> "logfile")
               -- :>>: countLines
               :>>: colorWieBitte
               :>>: roomTriggers Nothing Nothing
               :>>: quantizeFitness
               :>>: colorizeCombat Green Red
               :>>: (tanjianTriggers :>>: zaubererTriggers)

boot userpath host port = do
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

    mapM_ (\(a,b) -> bind a (mb b)) tanjianBindings

    setStatus $ do
        time <- getTime
        stat <- getU R_Common
        tstat <- getU R_Tanjian
        mapstat <- getU R_Mapper
        return $ printf "%d / %d | %d / %d | v:%d (%s) | g:%d | %s%s%s%s%s%s%s%s | Mapper: %s"
            (stat ^. mgStatLP)
            (stat ^. mgStatMLP)
            (stat ^. mgStatKP)
            (stat ^. mgStatMKP)
            (stat ^. mgStatVO)
            (stat ^. mgStatFR)
            (stat ^. mgStatG)
            (if stat ^. mgStatB then "B" else " ")
            (if stat ^. mgStatT then "T" else " ")
            (if stat ^. mgStatF then "F" else " ")
            (if tstat ^. tanjianStateHA then "HA" else "  ")
            (case tstat ^. tanjianStateTE of
                On -> "TE"
                Between -> "OM"
                Off -> "  "
            )
            (if tstat ^. tanjianStateKO then "KO" else "  ")
            (case tstat ^. tanjianStateM of
                On -> "M"
                Between -> "m"
                Off -> " "
            )
            (case tstat ^. tanjianStateAK of
                ak@(AksharaOn _ _) -> printf "AK(%s)" $ aksharaTime ak time
                ak@(AksharaBusy _) -> printf "ak(%s)" $ aksharaTime ak time
                AksharaReady -> "  "
            )
            (show $ mapstat ^. mapperMode)

    mb $ do
        connect host port
        colorfile <- io $ readUserFile $ userpath </> "colors"
        case colorfile of
            Nothing -> return ()
            Just f -> do
                act <- io $ readColorConfig f
                act

    screen

main :: IO ()
main = do
    args <- getArgs
    userpath <- initUserPath []
    case args of
        [host, port] -> execScreen mkMBConfig (mkMBState (Just $ triggers userpath) mkMGState) (boot userpath host port)
        _ -> putStrLn "Usage:\tmudblood <host> <port>"
