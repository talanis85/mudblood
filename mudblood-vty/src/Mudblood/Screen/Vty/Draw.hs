module Mudblood.Screen.Vty.Draw
    ( drawAttrString
    ) where

import Mudblood.Text
import qualified Graphics.Vty as V

mapColor :: Color -> V.MaybeDefault V.Color
mapColor c = case c of
    DefaultColor    -> V.Default
    Black           -> V.SetTo V.black
    White           -> V.SetTo V.white
    Cyan            -> V.SetTo V.cyan
    Magenta         -> V.SetTo V.magenta
    Blue            -> V.SetTo V.blue
    Yellow          -> V.SetTo V.yellow
    Green           -> V.SetTo V.green
    Red             -> V.SetTo V.red
    RGB r g b       -> V.SetTo $ V.rgbColor r g b

mapStyle :: Style -> V.MaybeDefault V.Style
mapStyle s = case s of
    StyleNormal     -> V.Default
    StyleBold       -> V.SetTo V.bold
    StyleUnderline  -> V.SetTo V.underline
    StyleReverse    -> V.SetTo V.reverseVideo

drawAttrString s = (V.horizCat $ map drawChunk (groupAS $ untabAS 8 s))
    where drawChunk (c, a) = V.string (V.Attr (mapStyle $ attrStyle a) (mapColor $ attrFg a) (mapColor $ attrBg a) V.Default) c
