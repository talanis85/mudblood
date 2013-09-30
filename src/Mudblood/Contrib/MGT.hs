{-# LANGUAGE TemplateHaskell #-}

module Mudblood.Contrib.MGT
    (
    ) where

import Language.Haskell.TH

lispGetSet :: Name -> String -> 
