module Mudblood.Colour
    ( parseColour
    , colourToGdk
    ) where

import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import qualified Graphics.UI.Gtk as G

type MBColour = Colour Double

parseColour :: String -> Maybe MBColour
parseColour col@('#':rest) = Just $ sRGB24read col
parseColour col = readColourName col

colourToGdk :: MBColour -> G.Color
colourToGdk col = let (RGB a b c) = toSRGB24 col
                  in G.Color (fromIntegral a * 256) (fromIntegral b * 256) (fromIntegral c * 256)
