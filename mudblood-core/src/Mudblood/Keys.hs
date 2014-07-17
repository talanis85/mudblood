module Mudblood.Keys
    ( Key (..), KeyMod (..)
    , parseKeys
    ) where

import Text.ParserCombinators.Parsec

-- | Enumeration of all supported keys (TODO: incomplete)
data Key = KAscii Char
         | KEnter
         | KBS
         | KEsc
         | KTab
         | KFun Int
         | KPgUp | KPgDn
         | KUp | KDown | KLeft | KRight
         | KUndefined
    deriving (Eq)

-- | Enumeration of all supported key modifiers
data KeyMod = MShift | MCtrl | MMeta | MAlt
    deriving (Eq)

instance Show Key where
    show (KAscii c) = [c]
    show KEnter = "<RET>"
    show KBS = "<BS>"
    show KEsc = "<ESC>"
    show KTab = "<TAB>"
    show (KFun x) = "<F" ++ show x ++ ">"
    show KPgUp = "<PgUp>"
    show KPgDn = "<PgDn>"
    show KUp = "<Up>"
    show KDown = "<Down>"
    show KLeft = "<Left>"
    show KRight = "<Right>"
    show KUndefined = "<?>"

-----------------------------------------------------------------------------

keyNames = 
    [ ("RET", KEnter)
    , ("BS", KBS)
    , ("ESC", KEsc)
    , ("TAB", KTab)
    , ("F1", KFun 1)
    , ("F2", KFun 2)
    , ("F3", KFun 3)
    , ("F4", KFun 4)
    , ("F5", KFun 5)
    , ("F6", KFun 6)
    , ("F7", KFun 7)
    , ("F8", KFun 8)
    , ("F9", KFun 9)
    , ("F10", KFun 10)
    , ("F11", KFun 11)
    , ("F12", KFun 12)
    , ("PgUp", KPgUp)
    , ("PgDn", KPgDn)
    , ("Up", KUp)
    , ("Down", KDown)
    , ("Left", KLeft)
    , ("Right", KRight)
    ]

-- | Parse a key string (example: "<ESC>qa<F5>" means "Press Escape, then Q, then A, then F5").
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
