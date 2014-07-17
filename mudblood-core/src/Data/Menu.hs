module Data.Menu
    ( Menu (..)
    , emptyMenu, stepMenu, showMenu

    , module Data.Bifunctor
    ) where

import Data.Bifunctor

-----------------------------------------------------------------------------

-- | A simple menu structure
data Menu k v = MenuLeaf v | MenuNode [(k, (String, Menu k v))]

instance Bifunctor Menu where
    bimap f g (MenuLeaf v) = MenuLeaf $ g v
    bimap f g (MenuNode xs) = MenuNode $ map (bimapElem f g) xs
        where bimapElem f g (k, (desc, submenu)) = (f k, (desc, bimap f g submenu))

-- | The empty menu
emptyMenu = MenuNode []

-- | Step down one level
stepMenu :: (Eq k) => k -> Menu k v -> Maybe (String, Menu k v)
stepMenu key bindings = case bindings of
    MenuLeaf x -> Nothing
    MenuNode l -> lookup key l

-- | Represent a menu as pairs of (keybinding, description)
showMenu :: Menu k v -> [(k, String)]
showMenu m = case m of
    MenuLeaf x -> []
    MenuNode l -> map (\(key, (desc, _)) -> (key, desc)) l
