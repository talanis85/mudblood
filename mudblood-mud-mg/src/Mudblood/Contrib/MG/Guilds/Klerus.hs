{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, FlexibleContexts, Rank2Types, DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.Guilds.Klerus
    ( R, component
    , stHS, stESS, stW, stGS, stMK, stESP
    , stGM, stSS
    ) where

import Data.Carte
import Data.Monoid
import Data.Maybe
import Data.Char

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens
import Control.Lens.TH

import Text.Printf

import Mudblood
import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.SkillDb as SkillDb
import Mudblood.Contrib.MG.Event

------------------------------------------------------------------------------

data Trool = On | Off | Between
data Element = Earth | Fire | Ice | Air | Acid | Water

data R a = R
    { _stHS     :: Trool
    , _stESS    :: Maybe Element
    , _stW      :: Trool
    , _stGS     :: Trool
    , _stMK     :: Trool
    , _stESP    :: Maybe Element
    , _stGM     :: Trool
    , _stSS     :: Trool
    }
  deriving (Functor)

mkSt = R
    { _stHS     = Off
    , _stESS    = Nothing
    , _stW      = Off
    , _stGS     = Off
    , _stMK     = Off
    , _stESP    = Nothing
    , _stGM     = Off
    , _stSS     = Off
    }

makeLenses ''R

------------------------------------------------------------------------------

component :: (SkillDb.R :@: r, MonadIO m, MonadFail m, MBEvent e) => MBComponent m e (Fix r) (Fix (R :*: r))
component =
      stateC mkSt
  >>> triggerC 50 spellStateTriggers
  >>> statusC status
  >>> SkillDb.skillC querySkills

------------------------------------------------------------------------------

status :: (Monad s, R :@: r) => MB (Fix r) s String
status = do
    kst <- use rec
    return $ printf "%s%s%s%s%s%s | schild: %s | sphaere: %s"
        (showOnOff "hs" $ kst ^. stHS)
        (showOnOff "w"  $ kst ^. stW)
        (showOnOff "gs" $ kst ^. stGS)
        (showOnOff "mk" $ kst ^. stMK)
        (showOnOff "gm" $ kst ^. stGM)
        (showOnOff "ss" $ kst ^. stSS)
        (showElement $ kst ^. stESS)
        (showElement $ kst ^. stESP)
  where
    showOnOff name On      = map toUpper name
    showOnOff name Between = map toLower name
    showOnOff name Off     = map (const ' ') name
    showElement Nothing      = "aus"
    showElement (Just Earth) = "erde"
    showElement (Just Fire)  = "feuer"
    showElement (Just Air)   = "luft"
    showElement (Just Ice)   = "eis"
    showElement (Just Water) = "wasser"
    showElement (Just Acid)  = "saeure"

------------------------------------------------------------------------------

a >>? b = \x -> a x >> b

spellStateTrigger setter states = mconcat $ map stateTrigger states
  where stateTrigger st = permanent $ do
            r <- parse' $ fetch >>= guardLine >>= \x -> case st x of
                Nothing -> mzero
                Just x' -> return x'
            lift $ rec . setter .= r

spellStateTriggers = mconcat
    [ spellStateTrigger stHS
        [ regex "^Lembold erhoert Dich\\. Ueber Deinem Haupt erscheint ein Heiligenschein\\.$" >>? return On
        , regex "^Dein Heiligenschein verglimmt\\.$" >>? return Off
        , regex "^Dein Heiligenschein flackert\\.$" >>? return Between
        ]
    , spellStateTrigger stW
        [ regex "^Du sprichst ein kurzes, inbruenstiges Gebet\\." >>? return On
        , regex "^Der Heilige Zorn Lembolds ist verraucht" >>? return Off
        ]
    , spellStateTrigger stGS
        [ regex "^Vergiftungen wirken nun nicht mehr so schnell bei Dir\\.$" >>? return On
        , regex "^Die Wirkung der Giftschwaechung ist nun ganz abgeklungen\\." >>? return Off
        , regex "^Die Wirkung der Giftschwaechung laesst nach\\." >>? return Between
        ]
    , spellStateTrigger stMK
        [ regex "^Kandri erfasst Dich mit ihrer Macht! Du beginnst zu gluehen!" >>? return On
        , regex "^Die wirbelnden Messer werden langsamer\\.$" >>? return Between
        , regex "^Der Kreis wirbelnder Messer verschwindet wieder\\.$" >>? return Off
        ]
    , spellStateTrigger stGM
        [ regex "^Als Dich die Aura umhuellt, spuerst Du" >>? return On
        , regex "^Die goettliche Aura verlaesst Dich wieder\\.$" >>? return Off
        ]
    , spellStateTrigger stESS
        [ regex "Eine Stichflamme schiesst vor Dir aus dem Boden und umgibt Dich" >>? return (Just Fire)
        , regex "^Die Erde zu Deinen Fuessen woelbt sich und bricht auf\\." >>? return (Just Earth)
        , regex "^Klirrende Kaelte umgibt Dich auf einmal schuetzend\\.$" >>? return (Just Ice)
        , regex "^Ein ploetzlicher Regenschauer prasselt hernieder, ohne Dich jedoch" >>? return (Just Water)
        , regex "^Ein starker Wind umtost Dich auf einmal und bildet so einen luftigen Schild\\.$" >>? return (Just Air)
        , regex "^Eine Wolke aus Saeuregasen bildet sich um Dich herum\\. Einige Blitze" >>? return (Just Acid)
        , regex "^Der Elementarschild zerfaellt\\.$" >>? return Nothing
        ]
    , spellStateTrigger stESP
        [ regex "^um Dich herum erscheint ein Blase aus kristalliner Erde\\. Dann wird Deine" >>? return (Just Earth)
        , regex "^um Dich herum erscheint ein Blase aus kristallinem Feuer\\. Dann wird Deine" >>? return (Just Fire)
        , regex "^um Dich herum erscheint ein Blase aus kristalliner Kaelte\\. Dann wird Deine" >>? return (Just Ice)
        , regex "^um Dich herum erscheint ein Blase aus kristallinem Wasser\\. Dann wird Deine" >>? return (Just Water)
        , regex "^um Dich herum erscheint ein Blase aus kristalliner Luft\\. Dann wird Deine" >>? return (Just Air)
        , regex "^Die Elementarsphaere loest sich auf\\.$" >>? return Nothing
        ]
    ]

------------------------------------------------------------------------------

querySkills = yieldSend "frag arkshat nach anrufungen" >> readSkills
    where
        readSkills = parse $ do
            fetchLineRegex "^Arkshat mustert Dich eindringlich. Dann sagt er:"
            fmap (map percentSkills) $ many $ fetchLineRegex2 "^Du beherrschst '([[:word:]]+)' +(.+)\\.$"
        percentSkills (k, v) = (k, fromMaybe 0 $ SkillDb.skillToPercent skillLevels v)

skills =
    [ "begrabe"
    , "blitz"
    , "donner"
    , "elementarschild"
    , "elementarsphaere"
    , "entfluche"
    , "entfrosche"
    , "entgifte"
    , "erloese"
    , "frieden"
    , "giftschwaechung"
    , "goettermacht"
    , "goetterzorn"
    , "heile"
    , "heiligenschein"
    , "heiltrank"
    , "identifiziere"
    , "kuriere"
    , "laeutere"
    , "lebenskraft"
    , "leuchten"
    , "messerkreis"
    , "praesenz"
    , "regeneriere"
    , "schaetz"
    , "segne"
    , "sonnenschutz"
    , "spaltung"
    , "weihe"
    ]

skillLevels =
    [ "gar nicht"
    , "erbaermlich schlecht"
    , "sehr schlecht"
    , "noch kaum"
    , "nur sehr wenig"
    , "gar nicht gut"
    , "nur wenig"
    , "noch nicht gut"
    , "schon nicht schlecht"
    , "nur unterdurchschnittlich"
    , "fast durchschnittlich gut"
    , "durchschnittlich gut"
    , "etwas besser als der Durchschnitt"
    , "ueberdurchschnittlich gut"
    , "besser als der Durchschnitt"
    , "recht gut"
    , "gut"
    , "schon sehr gut"
    , "ausserordentlich gut"
    , "hervorragend"
    , "phantastisch gut"
    , "fast perfekt"
    , "unuebertrefflich gut"
    ]
