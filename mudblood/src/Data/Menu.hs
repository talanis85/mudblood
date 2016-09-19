module Data.Menu
    ( Menu
    , emptyMenu
    , menuOf
    , mkSubmenu
    , matchMenu
    , menuTitle
    , describeMenu
    , walkMenu
    , stepMenu, showMenu
    , menuItem

    , module Data.Bifunctor
    ) where

import Control.Monad
import Data.List
import Data.Bifunctor
import Data.Monoid

-----------------------------------------------------------------------------

newtype Menu k v = Menu { getMenu :: (String, Either v [(k, Menu k v)]) }

{-
-- | A simple menu structure
data Menu k v = MenuLeaf v | MenuNode [(k, (String, Menu k v))]
-}

instance Functor (Menu k) where
    fmap f m = Menu $ second (bimap f (map (second (fmap f)))) (getMenu m)

instance Bifunctor Menu where
    bimap f g (Menu (d, Left v))   = Menu (d, Left (g v))
    bimap f g (Menu (d, Right xs)) = Menu (d, Right (map (bimap f (bimap f g)) xs))

instance Monoid (Menu k v) where
    mempty = Menu ("", Right [])
    mappend a b = case (matchMenu a, matchMenu b) of
                    (Left x, Right []) -> Menu (menuTitle a, Left x)
                    (Left x, y)        -> Menu (menuTitle b, y)
                    (Right x, Right y) -> Menu (intercalate ", " (filter (/= "") [menuTitle a, menuTitle b]), Right (x <> y))

emptyMenu = Menu ("", Right [])

menuItem k d v = Menu ("", Right [(k, Menu (d, Left v))])

menuTitle :: Menu k v -> String
menuTitle = fst . getMenu

matchMenu :: Menu k v -> Either v [(k, Menu k v)]
matchMenu = snd . getMenu

mkSubmenu :: k -> Menu k v -> Menu k v
mkSubmenu k m = Menu ("", Right [(k, m)])

menuOf :: String -> [(k, Menu k v)] -> Menu k v
menuOf d xs = Menu (d, Right xs)

describeMenu :: String -> Menu k v -> Menu k v
describeMenu d m = Menu (first (const d) $ getMenu m)

{-
menuValue :: Menu k v -> Either v [(Key, Menu k v)]
menuValue = snd . getMenu
-}

-- | Step down one level
stepMenu :: (Eq k) => k -> Menu k v -> Maybe (Menu k v)
stepMenu key m = case matchMenu m of
    Left x  -> Nothing
    Right l -> lookup key l

walkMenu :: (Eq k) => [k] -> Menu k v -> Maybe (Menu k v)
walkMenu keys m = foldM (flip stepMenu) m keys

-- | Represent a menu as pairs of (keybinding, description)
showMenu :: Menu k v -> [(k, String)]
showMenu m = case matchMenu m of
    Left x   -> []
    Right xs -> map (second menuTitle) xs
