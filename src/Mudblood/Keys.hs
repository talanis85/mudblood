module Mudblood.Keys
    ( Key (..)
    ) where

data Key = KAscii Char
         | KEnter
         | KBS
         | KEsc
         | KUndefined
    deriving (Eq)

instance Show Key where
    show (KAscii c) = [c]
    show KEnter = "<RET>"
    show KBS = "<BS>"
    show KEsc = "<ESC>"
    show KUndefined = "<?>"
