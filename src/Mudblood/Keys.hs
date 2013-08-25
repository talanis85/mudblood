module Mudblood.Keys
    ( Key (..)
    ) where

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
    show KUndefined = "<?>"
