module Mudblood.Screen.Vty.Widgets
  ( wMain
  , wRecentLines
  , wLines
  , wSidebar
  , wStatus
  , wCompletion
  , wPrompt
  , wMenu
  ) where

-----------------------------------------------------------------------------

import           Control.Lens
import           Control.Monad
import           Control.Monad.State

import           Data.Buffer
import           Data.Menu
import           Data.Monoid
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.ListZipper as LZ
import qualified Data.PrefixZipper as Z

import qualified Graphics.Vty as V
import           Graphics.Vty.Widget

import           Mudblood.Text
import           Mudblood.Screen.Vty.Monad
import           Mudblood.Screen.Vty.Lines
import           Mudblood.Screen.Vty.Select
import           Mudblood.Screen.Vty.Draw
import           Mudblood.Screen.Vty.Layout

import           System.Locale

import           Text.Printf

-----------------------------------------------------------------------------

timestampAttr = V.defAttr `V.withBackColor` V.black
separatorAttr = V.defAttr `V.withBackColor` V.black
statusAttr = V.defAttr `V.withBackColor` V.black
modeAttr = V.defAttr `V.withStyle` V.reverseVideo

timestampW = 10
infoW = 10

-----------------------------------------------------------------------------

wMain :: FlowWidget VtyScreen
wMain = padding $ mkFlow $ \w h -> do
    mode <- use scrMode

    case mode of
        SelectMode buf title lines cb ->
            let sel = fromMaybe mempty $ parseSelector (bufferContent buf)
            in fromFlow (wSelectView lines sel) w h
        _ -> fromFlow wMainView w h

wRecentLines :: FixedHeightWidget VtyScreen
wRecentLines = mkFixedHeight $ \w -> do
    debugLevel <- use scrDebugLevel
    linebuffer <- use scrLinebuffer

    let preparedLines = filterDebug debugLevel $ prepareLines $ LZ.toList linebuffer

    if LZ.leftp linebuffer then
        fmap (\x -> (0, x)) $ fromFlow flowEmpty w 0
    else
        fmap (\x -> (10, x)) $ fromFlow (horizSep `vert` wLines preparedLines) w (10 :: Dimension)

wLines :: (Monad m, Functor m) => [DisplayLine] -> FlowWidget m
wLines lines = mkFlow $ \w h -> do
    let mainW = max 0 $ w - timestampW - infoW
        infoW = 10
        timestampW = 10

    let drawnLines = reverse $ take h $ map (drawLine infoW) $ wrapLines mainW $ lines
        img = if length drawnLines < h
                then mconcat $ (V.backgroundFill w (h - length drawnLines)) : drawnLines
                else mconcat drawnLines

    return $ withoutCursor img

wSelectView :: (Monad m, Functor m) => [DisplayLine] -> Selector -> FlowWidget m
wSelectView savedLines sel = mkFlow $ \w h -> do
    let numberLine (n, (a, (b, c))) = if isSelected (n-1) sel then (a, (b, setFg Cyan (toAS (printf "%3d: " n) <> c)))
                                                              else (a, (b, toAS (printf "%3d: " n) <> c))
        numberedLines = map numberLine $ zip [1..] savedLines
    fromFlow (wLines numberedLines) w h

wMainView :: FlowWidget VtyScreen
wMainView = mkFlow $ \w h -> do
    debugLevel <- use scrDebugLevel
    linebuffer <- use scrLinebuffer

    let preparedLines = filterDebug debugLevel $ prepareLines $ LZ.listRight linebuffer

    fromFlow (wLines preparedLines) w h

wSidebar :: FlowWidget VtyScreen
wSidebar = padding $ mkFlow $ \w h -> do
    sidebar <- use scrSidebar
    return $ withoutCursor $ drawLayout w sidebar

wStatus :: FixedHeightWidget VtyScreen
wStatus = mkFixedHeight $ \w -> do
    status <- use scrStatus
    mode   <- use scrMode
    let st = V.string statusAttr (padRight (w - 20) ' ' status)

    case mode of
        PromptMode buf title _ ->
            let pr = V.string statusAttr (title ++ ": " ++ (bufferContent buf))
                cursorX = bufferCursor buf + length title + 2
                cursorY = 0
            in return (1, withCursor cursorX cursorY pr)
        SelectMode buf title _ _ ->
            let pr = V.string statusAttr (title ++ "? " ++ (bufferContent buf))
                cursorX = bufferCursor buf + length title + 2
                cursorY = 0
            in return (1, withCursor cursorX cursorY pr)
        _ ->
            let pr = V.string modeAttr (padRight 20 ' ' $ " " ++ showMode mode)
            in return (1, withoutCursor $ pr V.<|> st)

wMenu :: FixedWidget VtyScreen
wMenu = mkFixed $ do
    menu <- use scrMenu
    case menu of
        Nothing -> return (0, 0, withoutCursor V.emptyImage)
        Just menu ->
            let (w', h', i) = drawList [] Nothing $ map (\(k,d) -> (show k) ++ ": " ++ d) (showMenu menu)
            in return (w', h', withoutCursor i)

wCompletion :: FixedWidget VtyScreen
wCompletion = mkFixed $ do
    buffer <- use scrNormalBuffer
    if bufferCompletionState buffer
        then let (a,b,c) = Z.toParts $ bufferCompletionList buffer
                 (w', h', i) = drawList a b c
             in return (w', h', withoutCursor i)
        else return (0, 0, withoutCursor V.emptyImage)

drawList a b c =
    let fullList = a ++ maybeToList b ++ c
        maximum' l = if l == [] then 0 else maximum l
        w = maximum' (map length fullList)
        h = length fullList
        attr = V.defAttr `V.withStyle` V.reverseVideo
        selectedAttr = V.defAttr `V.withStyle` V.reverseVideo `V.withForeColor` V.blue
        innerList = case b of
            Nothing ->
               (V.vertCat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) (a ++ c))
            Just b' ->
               (V.vertCat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) a)
               V.<->
               (V.string selectedAttr $ "  " ++ padRight w ' ' b' ++ "  ")
               V.<->
               (V.vertCat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) c)
        menu = (V.string attr $ take (w + 4) $ repeat ' ')
               V.<->
               innerList
               V.<->
               (V.string attr $ take (w + 4) $ repeat ' ')
        in (w + 4, h + 2, menu)

padRight :: Int -> a -> [a] -> [a]
padRight n c l =
    if length l < n
        then l ++ (take (n - length l) $ repeat c)
        else take n l

minImage :: V.Image -> V.Image
minImage img = if (V.imageWidth img) == 0 || (V.imageHeight img) == 0 then V.backgroundFill 1 1 else img

a <!> b
    | validImg a && validImg b = a V.<|> b
    | validImg a = a
    | validImg b = b
    | otherwise = V.emptyImage
  where
    validImg a = V.imageWidth a > 0 && V.imageHeight a > 0

drawLine :: Int -> (ZonedTime, (String, AttrString)) -> V.Image
drawLine w (ts, (i, l)) = minImage $        (V.string timestampAttr $ formatTime defaultTimeLocale "%H:%M:%S " ts)
                                      V.<|> (V.string timestampAttr $ padRight w ' ' i)
                                      V.<|> (V.string V.defAttr " ")
                                      V.<|> drawAttrString l

wPrompt :: FixedHeightWidget VtyScreen
wPrompt = mkFixedHeight $ \w -> do
    markedPrompt <- use scrMarkedPrompt
    curPrompt    <- use scrPrompt
    buffer       <- use scrNormalBuffer
    mode         <- use scrMode

    let mainW = max 0 $ w - timestampW - infoW
        promptW = mainW - length markedPrompt - length (escapeAll curPrompt)
        wrappedPrompt = map fromAS $ wrapAS promptW $ toAS $ bufferContent buffer
        promptH = length wrappedPrompt

        img = case mode of
                InteractiveMode buf h cb ->
                    V.string V.defAttr "] "
                    <!>
                    V.string (V.withForeColor V.defAttr V.yellow) (bufferContent buf)
                _ ->
                    V.string V.defAttr markedPrompt
                    <!>
                    V.string V.defAttr (escapeAll curPrompt)
                    <!>
                    mconcat (map (V.string (V.withForeColor V.defAttr V.yellow)) wrappedPrompt)

        (cx, cy) = multilineCursorPos wrappedPrompt (bufferCursor buffer)
        cursorX = timestampW + infoW + length markedPrompt
                  + length (escapeAll curPrompt)
                  + cx
        cursorY = cy
        prompt = V.translateX (timestampW + infoW) img

    return (promptH, withCursor cursorX cursorY $ V.resize w promptH prompt)

multilineCursorPos :: [String] -> Int -> (Int, Int)
multilineCursorPos ls n = multilineCursorPos' ls n 0 0
  where multilineCursorPos' [] _ x y = (x, y)
        multilineCursorPos' _  0 x y = (x, y)
        multilineCursorPos' (l:ls) n x y = multilineCursorPos'' l ls n x y
        multilineCursorPos'' [] [] n x y = multilineCursorPos' ls n x y
        multilineCursorPos'' [] ls n x y = multilineCursorPos' ls n 0 (y+1)
        multilineCursorPos'' l ls 0 x y = (x, y)
        multilineCursorPos'' (c:cs) ls n x y = multilineCursorPos'' cs ls (n-1) (x+1) y

{-
drawStatus :: Int -> VtyScreenState -> String -> V.Image
drawStatus w s status =
    case (scrMode s) of
        PromptMode buf title _ ->
            (V.resizeWidth 20 $ V.cropLeft 20 $ V.string statusAttr (title ++ ": " ++ (bufferContent buf)))
        _ ->
            (V.charFill statusAttr ' ' 20 1)
    <!>
    V.string statusAttr (padRight (w - 20) ' ' status)
-}
