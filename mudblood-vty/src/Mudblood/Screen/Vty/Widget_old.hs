{-# LANGUAGE RankNTypes #-}
module Mudblood.Screen.Vty.Widget
    ( CImage, Widget, FWidget
    , Dimension, CompRatio
    , draw
    , padding, padded
    , hoistWidget
    , withCursor, withoutCursor
    , horizRel, vertRel
    , horizFix1, horizFix2
    , vertFix1, vertFix2

    , topleft, bottomleft

    , horizSep, vertSep
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Data.Traversable (sequenceA)
import Data.Bifunctor
import Graphics.Vty

type Dimension = Int
type CompRatio = Int

type CImage = (Image, Cursor)
type Widget f = Dimension -> Dimension -> f CImage
type FWidget f = f (Dimension, Dimension, CImage)
type FHWidget f = Dimension -> f (Dimension, CImage)
type FWWidget f = Dimension -> f (Dimension, CImage)

{-
data Widget f where
    Widget   :: (Dimension -> Dimension -> f CImage) -> Widget f
    FWidget  :: (f (Dimension, Dimension, CImage))   -> Widget f
    FHWidget :: (Dimension -> f (Dimension, CImage)) -> Widget f
    FWWidget :: (Dimension -> f (Dimension, CImage)) -> Widget f
-}

class CompRel w1 w2 w3 where
    horizRel :: CompRatio -> w1 -> w2 -> w3
    vertRel  :: CompRatio -> w1 -> w2 -> w3

-- These are duals?
-- type Widget' = Reader (Dimension, Dimension) CImage
-- type FWidget' = Env (Dimension, Dimension) CImage

cursorPlus :: Dimension -> Dimension -> Cursor -> Cursor -> Cursor
cursorPlus w h a NoCursor = a
cursorPlus w h _ (Cursor x y) = Cursor (x+w) (y+h)

draw :: (Applicative f) => [Widget f] -> Dimension -> Dimension -> f Picture
draw widgets w h =
    let imgs = sequenceA $ map (\x -> x w h) widgets
    in Picture <$> fmap (foldCursors . map snd) imgs <*> fmap (map fst) imgs <*> pure (Background ' ' defAttr)
  where
    foldCursors = foldr (cursorPlus 0 0) NoCursor

compHoriz_, compVert_ :: (Applicative f) => Widget f -> Widget f -> Dimension -> Dimension -> Dimension -> f CImage

compHoriz_ a b w1 w2 h = liftA2 comp (a w1 h) (b w2 h)
  where comp (i1,c1) (i2,c2) = (i1 <|> i2, cursorPlus w1 0 c1 c2)
compVert_  a b w h1 h2 = liftA2 comp (a w h1) (b w h2)
  where comp (i1,c1) (i2,c2) = (i1 <-> i2, cursorPlus 0 h1 c1 c2)

calcRatio r x =
    let a' = ceiling $ (min 100.0 (fromIntegral r) / 100.0) * (fromIntegral x)
        b' = max 0 (x - a')
    in (a', b')

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

topleft :: (Functor f) => FWidget f -> Widget f
topleft fw w h = fmap (\(w', h', (i, c)) -> (resize w h i, c)) fw

bottomleft :: (Functor f) => FWidget f -> Widget f
bottomleft fw w h = fmap (\(w', h', (i, c)) -> (resize w h (translate 0 (h - h') i), c)) fw

padding :: (Functor f) => Widget f -> Widget f
padding widget w h = first (resize w h) <$> widget w h

padded :: (Functor f) => Int -> Int -> Int -> Int -> FWidget f -> FWidget f
padded l t r b widget = (\(w,h,(i,c)) -> (w+l+r, h+t+b, (pad l t r b i, c))) <$> widget

fw :: (Functor f) => Dimension -> FWWidget f -> FWidget f
fw h widget = fmap (\(w,(i,c)) -> (w,h,(i,c))) (widget h)

left :: (Functor f) => FWWidget f -> Widget f
left widget w h = \(w', h', (i, c)) -> 

hoistWidget :: (forall a. f a -> g a) -> Widget f -> Widget g
hoistWidget f wid w h = f $ wid w h

{-
horizRel, vertRel :: (Applicative f) => CompRatio -> Widget f -> Widget f -> Widget f

horizRel r a b w h =
    let (w1, w2) = calcRatio r w
    in compHoriz_ a b w1 w2 h

vertRel r a b w h =
    let (h1, h2) = calcRatio r h
    in compVert_ a b w h1 h2
-}

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
