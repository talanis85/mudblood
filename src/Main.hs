import Mudblood
import Mudblood.Screen.Gtk

import Mudblood.User.Regex

import Data.Array
import Data.Dynamic

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM

import qualified Data.Map as M

import Text.Printf

import Mudblood.Contrib.MG

import Mudblood.User.Trigger

-- COMMANDS ------------------------------------------------------------

commands = M.fromList
    [ ("echo", Command ["text"] $ do
        x <- getStringParam 0
        lift $ outputAString $ fst $ decode x defaultAttr
        )
    , ("quit", Command [] $ do
        lift quit
        )
    , ("triggers", Command ["name", "trigger-type", "arguments", "response", "arguments"] $ do
        trigname <- getStringParam 0
        trigtype <- getIdParam 1
        step1 <- case trigtype of
            "recv" -> do exp <- getStringParam 2
                         return $ guardLineEvent >>> (\x -> guard (x =~ exp) >> return [x])
            _ -> fail "Invalid trigger type"
        resptype <- getIdParam 3
        step2 <- case resptype of
            "send" -> do dat <- getStringParam 4
                         return $ \x -> send dat >> return [LineTEvent x]
            _ -> fail "Invalid response type"
        lift $ modifyUserData $ \st -> st { mgDynTriggers = addDynTrigger (mgDynTriggers st) trigname (step1 >>> step2) }
        )
    , ("connect", Command ["host", "port"] $ do
        h <- getStringParam 0
        p <- getStringParam 1
        lift $ connect h p
        )
    ]

-- TRIGGERS -----------------------------------------------------------

dynTriggers = Permanent $ \x -> do
    st <- getState
    runDynTriggers (mgDynTriggers st) x

triggers = fightColorizer :>>: zaubererreportTrigger defaultZaubererStatus :>>: colorTriggers :>>: dynTriggers


boot :: Screen ()
boot =
    do
    bind [KEsc, KBS, KAscii '!', KAscii 'q'] $ mb quit
    bind [KEsc, KBS, KAscii '!', KAscii 'x', KEsc, KBS, KAscii 'q'] $ mb quit
    mb $ connect "openfish" "9999"
    --mb $ connect "localhost" "10000"
    screen

main :: IO ()
main = execScreen (MBConfig commands) (mkMBState (Just triggers) newMGState) boot
