{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Vty.Widget
    ( CImage, Widget, Dimension
    , FlowWidget, FixedWidget, FixedWidthWidget, FixedHeightWidget
    , mkFlow, mkFixed, mkFixedWidth, mkFixedHeight
    , fromFlow, fromFixed, fromFixedWidth, fromFixedHeight
    , fixw, fixh
    , CompH (horiz), CompV (vert)
    , draw
    , padding, padded
    , hoistWidget
    , withCursor, withoutCursor
    {-
    , horizRel, vertRel
    , horizFix1, horizFix2
    , vertFix1, vertFix2
    -}

    , topleft, bottomleft

    , flowEmpty, horizEmpty, vertEmpty
    , horizSep, vertSep
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Traversable (sequenceA)
import Data.Bifunctor
import Graphics.Vty

type Dimension = Int
-- type CompRatio = Int

type CImage = (Image, Cursor)

{-
type Widget f = Dimension -> Dimension -> f CImage
type FWidget f = f (Dimension, Dimension, CImage)
type FHWidget f = Dimension -> f (Dimension, CImage)
type FWWidget f = Dimension -> f (Dimension, CImage)
-}

{-
newtype Widget f   = Widget (Dimension -> Dimension -> f CImage)
newtype FWidget f  = FWidget (f (Dimension, Dimension, CImage))
newtype FWWidget f = FWWidget (Dimension -> f (Dimension, CImage))
newtype FHWidget f = FHWidget (Dimension -> f (Dimension, CImage))
-}

newtype Widget w h fw fh f = Widget (w -> h -> f (fw, fh, CImage))

type FlowWidget        = Widget Dimension Dimension () ()
type FixedWidget       = Widget () () Dimension Dimension
type FixedWidthWidget  = Widget () Dimension Dimension ()
type FixedHeightWidget = Widget Dimension () () Dimension

fromFlow :: (Functor f) => Widget w h () () f -> (w -> h -> f CImage)
fromFlow (Widget widget) = \w h -> fmap (\(_, _, x) -> x) (widget w h)

fromFixed :: Widget () () fw fh f -> f (fw, fh, CImage)
fromFixed (Widget widget) = widget () ()

fromFixedWidth :: (Functor f) => Widget () h fw () f -> h -> f (fw, CImage)
fromFixedWidth (Widget widget) = \h -> fmap (\(w, _, x) -> (w, x)) (widget () h)

fromFixedHeight :: (Functor f) => Widget w () () fh f -> w -> f (fh, CImage)
fromFixedHeight (Widget widget) = \w -> fmap (\(_, h, x) -> (h, x)) (widget w ())

mkFlow :: (Functor f) => (w -> h -> f CImage) -> Widget w h () () f
mkFlow widget = Widget $ \w h -> fmap (\x -> ((), (), x)) (widget w h)

mkFixed :: f (fw, fh, CImage) -> Widget () () fw fh f
mkFixed widget = Widget $ \() () -> widget

mkFixedWidth :: (Functor f) => (h -> f (fw, CImage)) -> Widget () h fw () f
mkFixedWidth widget = Widget $ \() h -> fmap (\(w, x) -> (w, (), x)) (widget h)

mkFixedHeight :: (Functor f) => (w -> f (fh, CImage)) -> Widget w () () fh f
mkFixedHeight widget = Widget $ \w () -> fmap (\(h, x) -> ((), h, x)) (widget w)

{-
data Widget f where
    Widget   :: (Dimension -> Dimension -> f CImage) -> Widget f
    FWidget  :: (f (Dimension, Dimension, CImage))   -> Widget f
    FHWidget :: (Dimension -> f (Dimension, CImage)) -> Widget f
    FWWidget :: (Dimension -> f (Dimension, CImage)) -> Widget f
-}

{-
class CompRel w1 w2 w3 where
    horizRel :: CompRatio -> w1 -> w2 -> w3
    vertRel  :: CompRatio -> w1 -> w2 -> w3

instance (Applicative f) => CompRel (Widget f) (Widget f) (Widget f) where
    horizRel r a b = Widget $ \w h ->
        let (w1, w2) = calcRatio r w
        in compHoriz_ a b w1 w2 h

    vertRel r a b = Widget $ \w h ->
        let (h1, h2) = calcRatio r h
        in compVert_ a b w h1 h2
-}

class CompH w1 w2 w3 where
    horiz :: (Monad f, Applicative f) => w1 f -> w2 f -> w3 f

class CompV w1 w2 w3 where
    vert  :: (Monad f, Applicative f) => w1 f -> w2 f -> w3 f

instance CompH FlowWidget FlowWidget FlowWidget where
    horiz a b = mkFlow $ \w h ->
        let (w1, w2) = calcRatio 50 w
        in liftA2 (compH w1) ((fromFlow a) w1 h) ((fromFlow b) w2 h)

instance CompV FlowWidget FlowWidget FlowWidget where
    vert a b = mkFlow $ \w h ->
        let (h1, h2) = calcRatio 50 h
        in liftA2 (compV h1) ((fromFlow a) w h1) ((fromFlow b) w h2)

instance CompH FlowWidget FixedWidthWidget FlowWidget where
    horiz a b = mkFlow $ \w h -> do
        (w', ci2) <- (fromFixedWidth b) h
        ci1 <- (fromFlow a) (w - w') h
        return $ compH (w - w') ci1 ci2

instance CompH FixedWidthWidget FlowWidget FlowWidget where
    horiz a b = mkFlow $ \w h -> do
        (w', ci1) <- (fromFixedWidth a) h
        ci2 <- (fromFlow b) (w - w') h
        return $ compH w' ci1 ci2

instance CompV FlowWidget FixedHeightWidget FlowWidget where
    vert a b = mkFlow $ \w h -> do
        (h', ci2) <- (fromFixedHeight b) w
        ci1 <- (fromFlow a) w (h - h')
        return $ compV (h - h') ci1 ci2

instance CompV FixedHeightWidget FlowWidget FlowWidget where
    vert a b = mkFlow $ \w h -> do
        (h', ci1) <- (fromFixedHeight a) w
        ci2 <- (fromFlow b) w (h - h')
        return $ compV h' ci1 ci2

instance CompH FixedWidthWidget FixedWidthWidget FixedWidthWidget where
    horiz a b = mkFixedWidth $ \h ->
        let comp (w1, ci1) (w2, ci2) = (w1 + w2, compH w1 ci1 ci2)
        in comp <$> (fromFixedWidth a) h <*> (fromFixedWidth b) h

instance CompV FixedHeightWidget FixedHeightWidget FixedHeightWidget where
    vert a b = mkFixedHeight $ \w ->
        let comp (h1, ci1) (h2, ci2) = (h1 + h2, compV h1 ci1 ci2)
        in comp <$> (fromFixedHeight a) w <*> (fromFixedHeight b) w

compV h (i1,c1) (i2,c2) = (i1 <-> i2, cursorPlus 0 h c1 c2)
compH w (i1,c1) (i2,c2) = (i1 <|> i2, cursorPlus w 0 c1 c2)

calcRatio r x = let a' = ceiling $ (min 100.0 (fromIntegral r) / 100.0) * (fromIntegral x)
                    b' = max 0 (x - a')
                in (a', b')

fixh :: (Functor f) => Dimension -> FlowWidget f -> FixedHeightWidget f
fixh h widget = mkFixedHeight $ \w -> fmap (\(c,i) -> (h,(c,i))) ((fromFlow widget) w h)

fixw :: (Functor f) => Dimension -> FlowWidget f -> FixedWidthWidget f
fixw w widget = mkFixedWidth $ \h -> fmap (\(c,i) -> (w,(c,i))) ((fromFlow widget) w h)

flowEmpty :: (Applicative f) => FlowWidget f
flowEmpty = mkFlow $ \w h -> pure $ withoutCursor $ resize w h emptyImage

vertEmpty :: (Applicative f) => FixedWidthWidget f
vertEmpty = mkFixedWidth $ \h -> pure $ (0, withoutCursor emptyImage)

horizEmpty :: (Applicative f) => FixedHeightWidget f
horizEmpty = mkFixedHeight $ \w -> pure (0, withoutCursor emptyImage)

-- These are duals?
-- type Widget' = Reader (Dimension, Dimension) CImage
-- type FWidget' = Env (Dimension, Dimension) CImage

cursorPlus :: Dimension -> Dimension -> Cursor -> Cursor -> Cursor
cursorPlus w h a NoCursor = a
cursorPlus w h _ (Cursor x y) = Cursor (x+w) (y+h)

draw :: (Applicative f) => [FlowWidget f] -> Dimension -> Dimension -> f Picture
draw widgets w h =
    let imgs = sequenceA $ map (\x -> (fromFlow x) w h) widgets
    in Picture <$> fmap (foldCursors . map snd) imgs <*> fmap (map fst) imgs <*> pure (Background ' ' defAttr)
  where
    foldCursors = foldr (cursorPlus 0 0) NoCursor

{-
compHoriz_, compVert_ :: (Applicative f) => Widget f -> Widget f -> Dimension -> Dimension -> Dimension -> f CImage

compHoriz_ a b w1 w2 h = liftA2 comp (a w1 h) (b w2 h)
  where comp (i1,c1) (i2,c2) = (i1 <|> i2, cursorPlus w1 0 c1 c2)
compVert_  a b w h1 h2 = liftA2 comp (a w h1) (b w h2)
  where comp (i1,c1) (i2,c2) = (i1 <-> i2, cursorPlus 0 h1 c1 c2)
-}


{-
padLeft   x = pad x 0 0 0
padTop    x = pad 0 x 0 0
padRight  x = pad 0 0 x 0
padBottom x = pad 0 0 0 x

fixSize :: Int -> Int -> Int -> Int -> Image -> Image
fixSize l t r b =
    let l' = if l < 0 then cropLeft   (abs l) else padLeft   l
        t' = if t < 0 then cropTop    (abs l) else padTop    l
        r' = if r < 0 then cropRight  (abs l) else padRight  l
        b' = if b < 0 then cropBottom (abs l) else padBottom l
    in l' . t' . r' . b'
-}

topleft :: (Functor f) => FixedWidget f -> FlowWidget f
topleft fw = mkFlow $ \w h -> fmap (\(w', h', (i, c)) -> (resize w h i, c)) (fromFixed fw)

bottomleft :: (Functor f) => FixedWidget f -> FlowWidget f
bottomleft fw = mkFlow $ \w h -> fmap (\(w', h', (i, c)) -> (resize w h (translate 0 (h - h') i), c)) (fromFixed fw)

padding :: (Functor f) => FlowWidget f -> FlowWidget f
padding widget = mkFlow $ \w h -> first (resize w h) <$> (fromFlow widget) w h

padded :: (Functor f) => Int -> Int -> Int -> Int -> FixedWidget f -> FixedWidget f
padded l t r b widget = mkFixed $ (\(w,h,(i,c)) -> (w+l+r, h+t+b, (pad l t r b i, c))) <$> (fromFixed widget)

{-
fw :: (Functor f) => Dimension -> FWWidget f -> FWidget f
fw h widget = fmap (\(w,(i,c)) -> (w,h,(i,c))) (widget h)

left :: (Functor f) => FWWidget f -> Widget f
left widget w h = \(w', h', (i, c)) ->
-}

hoistWidget :: (forall a. f a -> g a) -> Widget w h fw fh f -> Widget w h fw fh g
hoistWidget f (Widget wid) = Widget $ fmap (fmap f) wid

{-
horizRel, vertRel :: (Applicative f) => CompRatio -> Widget f -> Widget f -> Widget f

horizRel r a b w h =
    let (w1, w2) = calcRatio r w
    in compHoriz_ a b w1 w2 h

vertRel r a b w h =
    let (h1, h2) = calcRatio r h
    in compVert_ a b w h1 h2
-}

{-
instance (Applicative f) => CompRel (Widget f) (Widget f) (Widget f) where
    horizRel r a b w h =
        let (w1, w2) = calcRatio r w
        in compHoriz_ a b w1 w2 h

    vertRel r a b w h =
        let (h1, h2) = calcRatio r h
        in compVert_ a b w h1 h2

horizFix1, horizFix2, vertFix1, vertFix2 :: (Applicative f) => Dimension -> Widget f -> Widget f -> Widget f

horizFix1 fw a b w h = compHoriz_ a b fw (w - fw) h
horizFix2 fw a b w h = compHoriz_ a b (w - fw) fw h
vertFix1  fh a b w h = compVert_  a b w fh (h - fh)
vertFix2  fh a b w h = compVert_  a b w (h - fh) fh

horizSep w h = return $ withoutCursor $ charFill defAttr '-' w h
vertSep w h = return $ withoutCursor $ charFill defAttr '|' w h

horizSpace w h = return $ withoutCursor $ backgroundFill w h
vertSpace w h = return $ withoutCursor $ backgroundFill w h
-}

horizSep :: (Applicative f) => FixedHeightWidget f
horizSep = mkFixedHeight $ \w -> pure (1, withoutCursor $ charFill defAttr '-' w 1)

vertSep :: (Applicative f) => FixedWidthWidget f
vertSep = mkFixedWidth $ \h -> pure (1, withoutCursor $ charFill defAttr '|' 1 h)

horizSpace :: (Applicative f) => FlowWidget f
horizSpace = mkFlow $ \w h -> pure $ withoutCursor $ backgroundFill w h

vertSpace :: (Applicative f) => FlowWidget f
vertSpace = mkFlow $ \w h -> pure $ withoutCursor $ backgroundFill w h

withCursor x y a = (a, Cursor x y)
withoutCursor a  = (a, NoCursor)

--

{-
wMain :: Widget m
wSidebar :: Widget m
wStatus :: Widget m

wAll = compVertRel 80 imgMain imgSidebar
wMain = undefined -- draw lines with width / height
wSidebar = undefined -- draw widgets "
wStatus = undefined -- draw status
-}
