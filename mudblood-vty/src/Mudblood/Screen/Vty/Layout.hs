module Mudblood.Screen.Vty.Layout
    ( Layout (..)
    , drawLayout
    ) where

import Mudblood.Keys
import Mudblood.Text
import Mudblood.Mapper

import Data.Monoid
import Data.List

import Data.QuasiEq

import qualified Graphics.Vty as V

import Mudblood.Screen.Vty.Draw

-- | Layouts are small pieces of information to be displayed by the screen.
data Layout =
    LayoutEmpty
  | LayoutText AttrString    -- ^ A singe line of text
  | LayoutTable [[String]]   -- ^ A table of textual cells
  | LayoutList [AttrString]
  | LayoutLabels [(String, AttrString)]
  | LayoutGauge Int Int Int
  | LayoutMap Int Int (QuasiEq Map)
  | LayoutSep
  | LayoutV Layout Layout
  | LayoutH Layout Layout
  | LayoutFocus Bool Layout
  deriving (Eq)

instance Semigroup Layout where
  (<>) LayoutEmpty x = x
  (<>) x LayoutEmpty = x
  (<>) a b = LayoutV a b

instance Monoid Layout where
  mempty = LayoutEmpty

drawLayout :: Int -> Layout -> V.Image
drawLayout width layout = case layout of
    LayoutEmpty -> V.emptyImage
    LayoutText str  -> mconcat $ map drawAttrString $ concat $ map (wrapAS width) $ linesAS str
    LayoutTable tab ->
        let tab' = rectify " " (maximum $ map length tab) tab
            cols = transpose tab'
            colsizes = map ((+1) . maximum . map length) cols
        in mconcat $ map drawLine $ map (zip colsizes) tab'
    LayoutList l -> mconcat $ map drawAttrString l
    LayoutLabels ls ->
        let fstmax = 1 + (maximum $ map (length . fst) ls)
        in mconcat $ map (drawLabel width fstmax) ls
    LayoutMap h cur m -> V.resize width h $ mconcat $ map (V.string V.defAttr) $ mapDrawAscii width h cur (unQuasiEq m)
    LayoutSep -> V.string V.defAttr $ take width $ repeat '-'
    LayoutV w1 w2 -> drawLayout width w1 V.<-> drawLayout width w2
      -- mconcat $ map ((V.<|> V.string V.defAttr " ") . drawLayout width) ws
    LayoutH w1 w2 -> let width' = width `div` 2
                     in drawLayout width' w1 V.<|> V.charFill V.defAttr '|' 1 150 V.<|> drawLayout (width - width' - 1) w2
    LayoutFocus focus w -> drawFocused focus width w
  where
    drawLine l = V.horizCat $ map (\(s,l) -> V.string V.defAttr $ padRight s ' ' $ l ++ " ") l
    drawLabel w lw (l,t) = (V.string V.defAttr $ padRight lw ' ' l) V.<|> V.vertCat (map drawAttrString $ concat $ map (wrapAS (w - lw)) $ linesAS t)
    rectify elem len mat = map (fill elem len) mat
    fill elem len l = l ++ (take (max 0 (len - length l)) $ repeat elem)
    drawFocused focus width layout =
      let attr   = if focus then V.defAttr `V.withStyle` V.reverseVideo else V.defAttr
          inner  = V.resizeWidth (width - 2) $ drawLayout (width - 2) layout
          bHoriz = V.charFill attr ' ' width 1
          bVert  = V.charFill attr ' ' 1 (V.imageHeight inner)
      in bHoriz V.<-> (bVert V.<|> inner V.<|> bVert) V.<-> bHoriz

padRight :: Int -> a -> [a] -> [a]
padRight n c l =
    if length l < n
        then l ++ (take (n - length l) $ repeat c)
        else take n l
