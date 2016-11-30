module Mudblood.Screen.Vty.Lines
    ( Line, DisplayLine
    , prepareLines
    , scrollLines
    , filterDebug
    , wrapLines
    ) where

import Prelude hiding (concat)
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bitraversable
import Data.Traversable.Instances

import Data.List.Flatten
import Mudblood.Text

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

{-
instance Foldable ((,) a) where
    foldMap f (_, y) = f y
    foldr f z (_, y) = f y z

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y
-}

class BiDistribute f where
    bidist :: (Bifunctor g) => f (g a b) -> g (f a) (f b)

instance BiDistribute ((,) a) where
    bidist (x, e) = bimap ((,) x) ((,) x) e

type Line = ((Int, ZonedTime), (Either String AttrString))
type DisplayLine = (ZonedTime, (String, AttrString))

-- bimap2 f = bimap f f

combineR :: (Monoid b, Monoid c) => (Maybe (a, b), Maybe (a, c)) -> Maybe (a, (b, c))
combineR (Just (_, a), Just (x, b)) = Just (x, (a,      b))
combineR (Nothing,     Just (x, b)) = Just (x, (mempty, b))
combineR (Just (x, a), Nothing)     = Just (x, (a, mempty))
combineR (Nothing,     Nothing)     = Nothing

linesLine :: Line -> [Line]
linesLine = sequenceA . fmap bisequenceA . fmap (bimap lines linesAS)

linesLines = concat . map linesLine
flattenLines = catMaybes . map combineR . flatten . fmap bidist

sequenceWrapped :: (Monoid a) => (a, [AttrString]) -> [(a, AttrString)]
sequenceWrapped (a, [])   = []
sequenceWrapped (a, x:xs) = (a, x) : map ((,) mempty) xs

prepareLines = flattenLines . linesLines
scrollLines = drop
filterDebug l = mapMaybe $ \((l', t), (m, m')) -> if l >= l' then Just (t, (m, m')) else Nothing
wrapLine w = sequenceA . fmap sequenceWrapped . fmap (fmap (reverse . wrapAS w))
wrapLines w = concat . map (wrapLine w)
