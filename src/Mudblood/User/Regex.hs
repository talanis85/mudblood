{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Mudblood.User.Regex
    ( module Text.Regex.PCRE
    ) where

import Mudblood.Text
import Text.Regex.PCRE

instance Extract AttrString where
    empty = toAttrString ""
    before i s = toAttrString $ before i $ fromAttrString s
    after i s = toAttrString $ after i $ fromAttrString s
    extract w s = toAttrString $ extract w $ fromAttrString s

instance RegexContext Regex AttrString [[String]] where
    match r s = map (map fromAttrString) (match r s)
    matchM r s = do
        ret <- matchM r s
        return $ map (map fromAttrString) ret

instance RegexLike Regex AttrString where
    matchOnce r s = matchOnce r (fromAttrString s)
    matchAll r s = matchAll r (fromAttrString s)

