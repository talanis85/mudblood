{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, FlexibleContexts, Rank2Types #-}

module Mudblood.Contrib.MG.Guilds.Karate
    ( R
    , attackSet, blockSet
    , status, triggers
    , setup
    , querySkills
    , updateAttacks, updateBlocks

    , Waza (..)
    , attacks, blocks
    , filterByLevel
    , ueben, autoueben
    ) where

import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Utils
import Data.Vinyl.Open
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Lens.TH

import Text.Printf

import Mudblood hiding (queryStatus)
import Mudblood.Contrib.MG.Event

import qualified Mudblood.Contrib.MG.Char as Char

------------------------------------------------------------------------------

data R = R
type instance TypeOf R = St

base = olens (Proxy :: Proxy R)

------------------------------------------------------------------------------

data Form = Normal | Wolf | Ghourdal | Horpas | Galbrag

data St = St
    { _attackSet :: Set.Set String
    , _blockSet :: Set.Set String
    }

mkSt :: St
mkSt = St
    { _attackSet = Set.empty
    , _blockSet = Set.empty
    }

makeLenses ''St

triggers = chain
  [ wazaListTrigger
  ]

setup = return ()

status = return ""

updateAttacks = do
  attacks <- use $ base . attackSet
  send $ "angriff mit " ++ (concat $ intersperse " " $ Set.toList attacks)
  echo $ toAS $ "angriff mit " ++ (concat $ intersperse " " $ Set.toList attacks)

updateBlocks = do
  blocks <- use $ base . blockSet
  send $ "abwehr mit " ++ (concat $ intersperse " " $ Set.toList blocks)

a >>? b = \x -> a x >> b

data Direction = In | Out | Unk
  deriving (Eq)

data Waza = Waza
  { wazaName :: String
  , wazaShort :: String
  , wazaDifficulty :: Int
  , wazaLearn :: Int
  , wazaNeed :: Int
  , wazaArea :: String
  , wazaKP :: Int
  , wazaDirection :: Direction
  }

defaultBlock = Waza "gedan-barai" "gb" 0 0 0 "g" 0 Out
defaultAttack = Waza "choku-zuki" "cz" 0 0 0 "fst" 0 Out

lookupWaza name = listToMaybe . filter (\x -> wazaName x == name)

numericGuildLevel x | x == 0 = 0
                    | x > 0  = 10 - x
                    | x < 0  = 10 + (abs x - 1)


fetchAttackList = do
  l1 <- fetchLineRegex1 "^Du greifst jetzt mit (.+)$"
  if l1 == "allem was Du kannst an. "
     then return []
     else do
          ls <- liftM (fmap fromAS) $ many fetchLine
          let wlist1 = l1 ++ " " ++ mconcat (intersperse " " ls)
              wlist2 = filter (/= '.') $ filter (/= ',') wlist1
              wlist3 = words wlist2
              wlist4 = filter (/= "an") $ filter (/= "und") wlist3
          return wlist4

fetchBlockList = do
  l1 <- fetchLineRegex1 "^Du verteidigst Dich jetzt mit (.+)$"
  if l1 == "allem was Du kannst. "
     then return []
     else do
          ls <- liftM (fmap fromAS) $ many fetchLine
          let wlist1 = l1 ++ " " ++ mconcat (intersperse " " ls)
              wlist2 = filter (/= '.') $ filter (/= ',') wlist1
              wlist3 = words wlist2
              wlist4 = filter (/= "und") wlist3
          return wlist4

wazaListTrigger = chain
  [ permanent $ parse' fetchAttackList >>= lift . (.=) (base . attackSet) . Set.fromList
  , permanent $ parse' fetchBlockList >>= lift . (.=) (base . blockSet) . Set.fromList
  ]

attacks = map mkWaza attacks'
  where
    mkWaza (a,b,c,d,e,f,g,h) = Waza a b c (numericGuildLevel d) (numericGuildLevel e) f g h
    attacks' =
      -- technik                abk     schw  ab  fuer  bereich   mp    richtung
      [ ("age-teisho-uchi",     "atu",  6,    7,  4,    "hks",    6 ,   Out)
      , ("age-zuki",            "az",   2,    0,  8,    "fst",    0 ,   Out)
      , ("awase-zuki",          "awz",  6,    7,  2,    "2fst",   6 ,   In)
      , ("choku-zuki",          "cz",   1,    0,  9,    "fst",    0 ,   Unk)
      , ("fumikiri-geri",       "fg",   4,    8,  5,    "tr",     0 ,   Out)
      , ("fumikomi-geri",       "fog",  6,    0,  6,    "tr",     6 ,   Unk)
      , ("gyaku-mawashi-geri",  "gmg",  8,    7,  4,    "tr",     15,   Out)
      , ("gyaku-mawashi-shitsui-geri", "gmsg", 7, 8, 4, "ktr",    14,   Out)
      , ("hasami-zuki",         "haz",  4,    8,  3,    "2fst",   4 ,   Out)
      , ("heiko-zuki",          "hz",   1,    9,  4,    "2fst",   1 ,   Out)
      , ("kagi-zuki",           "kaz",  3,    8,  6,    "fst",    3 ,   Out)
      , ("kentsui-hasami-uchi", "khu",  5,    8,  4,    "fsl",    8 ,   In)
      , ("kizami-zuki",         "kiz",  1,    0,  7,    "fst",    1 ,   Out)
      , ("mae-empi-uchi",       "meu",  3,    7,  5,    "ebs",    2 ,   Out)
      , ("mae-geri-keage",      "mgka", 7,    6,  4,    "tr",     13,   Out)
      , ("mae-geri-kekomi",     "mgk",  5,    0,  6,    "tr",     9 ,   Unk)
      , ("mae-shitsui-geri",    "msg",  5,    0,  6,    "ktr",    9 ,   Out)
      , ("mawashi-geri",        "mag",  8,    7,  4,    "tr",     16,   In)
      , ("mawashi-shitsui-geri","mwsg", 7,    8,  4,    "ktr",    12,   In)
      , ("mawashi-zuki",        "mz",   3,    9,  6,    "fst",    3 ,   In)
      , ("mikazuki-geri",       "mg",   5,    8,  4,    "tr",     10,   In)
      , ("otoshi-empi-uchi",    "oeu",  3,    6,  3,    "ebs",    3 ,   Out)
      , ("otoshi-kentsui-uchi", "oku",  4,    9,  6,    "fsl",    4 ,   In)
      , ("otoshi-shuto-uchi",   "osu",  6,    0,  6,    "hks",    7 ,   Unk)
      , ("otoshi-uraken-uchi",  "ouu",  2,    0,  7,    "fsl",    1 ,   In)
      , ("soto-heito-uchi",     "shu",  7,    6,  3,    "hks",    9 ,   Out)
      , ("soto-kentsui-uchi",   "sku",  3,    9,  6,    "fsl",    3 ,   Out)
      , ("soto-shuto-uchi",     "ssu",  2,    9,  5,    "hks",    4 ,   Out)
      , ("soto-uraken-uchi",    "suu",  1,    0,  7,    "fsl",    2 ,   Out)
      , ("tate-empi-uchi",      "teu",  4,    5,  3,    "ebs",    4 ,   Out)
      , ("tate-zuki",           "tz",   1,    0,  8,    "fst",    1 ,   Out)
      , ("uchi-heito-uchi",     "uhu",  7,    6,  3,    "hks",    9 ,   In)
      , ("uchi-kentsui-uchi",   "uku",  6,    9,  5,    "fsl",    7 ,   In)
      , ("uchi-shuto-uchi",     "usn",  4,    9,  5,    "hks",    5 ,   In)
      , ("uchi-teisho-uchi",    "utu",  5,    6,  3,    "hks",    10,   In)
      , ("ura-zuki",            "uz",   3,    8,  5,    "fst",    2 ,   Out)
      , ("ushiro-empi-uchi",    "ueu",  2,    8,  5,    "ebs",    1 ,   Out)
      , ("ushiro-geri-keage",   "ugka", 9,    4,  1,    "tr",     0 ,   Out)
      , ("ushiro-geri-kekomi",  "ugk",  8,    6,  2,    "tr",     17,   In)
      , ("ushiro-mawashi-geri", "umg",  10,   4,  2,    "tr",     13,   In)
      , ("yama-zuki",           "yz",   6,    7,  2,    "2fst",   6,    Unk)
      , ("yoko-empi-uchi",      "yeu",  2,    7,  4,    "ebs",    2,    Unk)
      , ("yoko-geri-keage",     "ygk",  9,    5,  1,    "tr",     14,   Out)
      , ("yoko-geri-kekomi",    "ygkk", 7,    6,  4,    "tr",     14,   In)
      , ("yoko-mawashi-empi-uchi", "ymeu", 6, 6,  3,    "ebs",    6,    In)
      , ("yoko-mawashi-geri",   "ymg",  8,    5,  3,    "tr",     15,   In)
      , ("yoko-tobi-geri-kekomi", "ytgk", 10, -1, -3,   "tr",     0,    Unk)
      ]

blocks = map mkWaza blocks'
  where
    mkWaza (a,b,c,d,e,f,g,h) = Waza a b c (numericGuildLevel d) (numericGuildLevel e) f g h
    blocks' =
      -- technik                abk     schw  ab  fuer  bereich   mp
      -- 10 Kyu
      [ ("uchi-ude-uke",        "uuu",  4,    0,  9,    "jc",     4 ,   In)
      , ("soto-ude-uke",        "suu",  2,    0,  9,    "c",      0 ,   Out)
      , ("gedan-barai",         "gb",   4,    0,  9,    "g",      5 ,   Out)
      , ("age-uke",             "au",   2,    0,  9,    "j",      0 ,   Out)
      -- 9 Kyu
      , ("soto-shuto-uke",      "ssu",  4,    9,  6,    "g",      5 ,   Out)
      , ("shuto-uke",           "shu",  3,    9,  7,    "jc",     2 ,   Out)
      , ("otoshi-ude-uke",      "ouu",  7,    9,  6,    "c",      14,   In)
      , ("naiwan-nagashi-uke",  "nnu",  4,    9,  7,    "j",      4 ,   Out)
      , ("maeude-deai-osae-uke","mdou", 4,    9,  5,    "c",      5 ,   In)
      , ("haiwan-nagashi-uke",  "hnu",  4,    9,  7,    "j",      5 ,   Out)
      , ("gaiwan-nagashi-uke",  "gnu",  6,    9,  8,    "j",      7 ,   Out)
      , ("gaiwan-gedan-uke",    "ggu",  6,    9,  5,    "g",      6 ,   Unk)
      -- 8 Kyu
      , ("te-osae-uke",         "tou",  5,    8,  4,    "c",      9 ,   In)
      , ("te-nagashi-uke",      "tnu",  5,    8,  5,    "jc",     18,   In)
      , ("shuto-juji-uke",      "sju",  4,    8,  5,    "j",      4 ,   Out)
      , ("ken-juji-uke",        "kju",  4,    8,  5,    "g",      5 ,   In)
      , ("heishu-uke",          "hu",   6,    8,  6,    "jc",     3 ,   Out)
      , ("age-teisho-uke",      "atu",  7,    8,  5,    "j",      12,   Out)
      -- ...
      -- , ("kakuto-uke",          "ku",   6,    0,  9,    "jc",     6 ,   Unk)
      -- , ("uchi-shuto-uke",      "usu",  5,    8,  5,    "g",      9 ,   Unk)
      ]

blocksForAttack att reg =
  let att' = fromMaybe defaultAttack $ lookupWaza att attacks
  in filter (\x -> reg `elem` wazaArea x) $ filter (\x -> wazaDirection att' /= wazaDirection x) blocks

filterByLevel gl = filter (\x -> wazaLearn x <= gl)

ueben = do
  yieldSend "uebe abwehr"
  (waza, area) <- parse' $ fetchLineRegex2 "^(.+) im (.+)-Bereich an\\.$"
  let regShort r = case r of
        "Jodan" -> 'j'
        "Chudan" -> 'c'
        "Gedan" -> 'g'
        _ -> 'g'
      bs = blocksForAttack (map toLower waza) (regShort area)
      b = fromMaybe defaultBlock $ listToMaybe bs
  yieldSend $ wazaName b

autoueben = do
  oneshot $ yieldSend "uebe abwehr"
  let uebe n = do
        oneshot $ do
          (waza, area) <- parse' $ fetchLineRegex2 "^(.+) im (.+)-Bereich an\\.$"
          wait 4
          let regShort r = case r of
                "Jodan" -> 'j'
                "Chudan" -> 'c'
                "Gedan" -> 'g'
                _ -> 'g'
              bs = case blocksForAttack (map toLower waza) (regShort area) of
                     [] -> [defaultBlock]
                     (x:xs) -> (x:xs)
              b = head $ drop n $ cycle bs
          yieldSend $ wazaName b
        uebe (n + 1)
  uebe 0

skillLevels =
  [ "total hundsmiserabel"
  , "hundsmiserabel"
  , "aeusserst miserabel"
  , "sehr miserabel"
  , "miserabel"
  , "sehr uebel"
  , "uebel"
  , "sehr schlecht"
  , "schlecht"
  , "mehr schlecht als recht"
  , "nicht mehr allzu schlecht"
  , "nicht mehr schlecht"
  , "eher maessig als schlecht"
  , "fast maessig"
  , "maessig"
  , "etwas besser als nur maessig"
  , "deutlich besser als nur maessig"
  , "halbwegs passabel"
  , "knapp passabel"
  , "passabel"
  , "gut passabel"
  , "bald mittelmaessig"
  , "fast, FAST mittelmaessig"
  , "mittelmaessig"
  , "gut mittelmaessig"
  , "schon bald recht gut"
  , "recht gut"
  , "besser als nur recht gut"
  , "fast schon gut"
  , "gut"
  , "wirklich gut"
  , "bald sehr gut"
  , "fast sehr gut"
  , "etwa sehr gut"
  , "wirklich sehr gut"
  , "fast ausgezeichnet"
  , "ausgezeichnet"
  , "sehr ausgezeichnet"
  , "ueberaus ausgezeichnet"
  , "hervorragend"
  , "wirklich hervorragend"
  , "so langsam phantastisch"
  , "beinahe phantastisch"
  , "phantastisch"
  , "aussergewoehnlich phantastisch"
  , "fast meisterlich"
  , "meisterlich"
  , "ueberaus meisterlich"
  , "aussergewoehnlich meisterlich"
  , "phantastisch meisterlich"
  , "so perfekt wie ein wahrer Meister"
  ]

querySkills = yieldSend "frag funakoshi nach status" >> readSkills
    where
        readSkills = parse $ do
          fetchLineRegex "Funakoshi sagt"
          fmap (map percentSkills) $ many fetchSkill
        percentSkills (k, v) = (k, fromMaybe 0 $ skillToPercent skillLevels v)
        fetchSkill = msum
          [ fetchLineRegex2 "^``Du kannst ([^ ]+) (.+),$"
          , fetchLineRegex2 "^([^ ]+) (.+),$"
          , fetchLineRegex2 "^([^ ]+) (.+) und$"
          , fetchLineRegex2 "^([^ ]+) (.+)\\.''$"
          ]

position :: (Eq a) => a -> [a] -> Maybe Int
position v l = position' 0 v l
    where position' i v [] = Nothing
          position' i v (x:xs) = if x == v then Just i else position' (i+1) v xs

skillToPercent :: [String] -> String -> Maybe Int
skillToPercent levels val = fmap calcpercent $ position val levels
    where calcpercent x = ((x+1) * 100) `div` length levels

{-
abwehrueben =
  [ ("mae-geri-kekomi",
    [ "gedan-barai"
    ])
  , ("yoko-mawashi-geri",
    [ "heishu-uke"
    ])
  , ("yoko-mawashi-empi-uchi",
    [ "gedan-barai"
    ])
-}

{-
querySkills :: (Monad m) => Trigger (Ev a) m [(String, Int)]
querySkills = succeed (SendEvent "frag arkshat nach anrufungen") >> readSkills
    where
        readSkills = do
            try $ guardLine >=> guard . ((~=) "^Arkshat mustert Dich eindringlich. Dann sagt er:")
            parseSkills
        parseSkills = do
            l <- try $ guardLine
            case "^Du beherrschst '([[:word:]]+)' +(.+)\\.$" ~~= l of
               Nothing -> return []
               Just [_, k, v] -> do
                   rest <- parseSkills
                   return $ (k, fromMaybe 0 $ skillToPercent skillLevels v) : rest

position :: (Eq a) => a -> [a] -> Maybe Int
position v l = position' 0 v l
    where position' i v [] = Nothing
          position' i v (x:xs) = if x == v then Just i else position' (i+1) v xs

skillToPercent :: [String] -> String -> Maybe Int
skillToPercent levels val = fmap calcpercent $ position val levels
    where calcpercent x = ((x+1) * 100) `div` length levels

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
-}
