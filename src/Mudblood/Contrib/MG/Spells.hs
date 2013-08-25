module Mudblood.Contrib.MG.Spells
    ( spell
    ) where

import Mudblood
import Mudblood.Contrib.MG.State

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack
  = case begins haystack needle of
      Just remains -> replacement ++ remains
      Nothing      -> case haystack of
                        []     -> []
                        x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack []                = Just haystack
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _                 = Nothing

spell :: (MBMonad m) => String -> m ()
spell sp = do
    focus <- getUserData >>= return . mgFocus
    case focus of
        Nothing -> send sp
        Just f  -> send $ replace "%f" f sp
