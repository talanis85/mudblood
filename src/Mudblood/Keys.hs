module Mudblood.Keys
    ( Key (..)
    , parseKeys
    ) where

import Text.ParserCombinators.Parsec

data Key = KAscii Char
         | KEnter
         | KBS
         | KEsc
         | KF1 | KF2 | KF3 | KF4 | KF5 | KF6
         | KF7 | KF8 | KF9 | KF10 | KF11 | KF12
         | KUndefined
    deriving (Eq)

instance Show Key where
    show (KAscii c) = [c]
    show KEnter = "<RET>"
    show KBS = "<BS>"
    show KEsc = "<ESC>"
    show KF1 = "<F1>"
    show KF2 = "<F2>"
    show KF3 = "<F3>"
    show KF4 = "<F4>"
    show KF5 = "<F5>"
    show KF6 = "<F6>"
    show KF7 = "<F7>"
    show KF8 = "<F8>"
    show KF9 = "<F9>"
    show KF10 = "<F10>"
    show KF11 = "<F11>"
    show KF12 = "<F12>"
    show KUndefined = "<?>"

keyNames = 
    [ ("RET", KEnter)
    , ("BS", KBS)
    , ("ESC", KEsc)
    , ("F1", KF1)
    , ("F2", KF2)
    , ("F3", KF3)
    , ("F4", KF4)
    , ("F5", KF5)
    , ("F6", KF6)
    , ("F7", KF7)
    , ("F8", KF8)
    , ("F9", KF9)
    , ("F10", KF10)
    , ("F11", KF11)
    , ("F12", KF12)
    ]

parseKeys inp = case parse parseKeystring "" inp of
    Left e -> Nothing
    Right r -> Just r

parseKeystring = many1 $ try parseSpecialKey <|> try parseAsciiKey

parseSpecialKey = do
    char '<'
    k <- parseSpecialKeyName
    char '>'
    return k

parseSpecialKeyName = choice $ map mkKeyParser keyNames
    where
        mkKeyParser (a,b) = string a >> return b

parseAsciiKey = do
    c <- anyChar
    return $ KAscii c
