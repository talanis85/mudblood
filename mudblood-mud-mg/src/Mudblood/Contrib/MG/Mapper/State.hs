{-# LANGUAGE DeriveFunctor #-}
module Mudblood.Contrib.MG.Mapper.State
    (
    -- * Module boilerplate and data types
      R
    , mkSt
    , MapperMode (..)
    , WalkMode (..)
    -- * Lenses
    , mapStore, fileName, roomHash, currentRoom, walkStack
    , mode, walkMode
    , overlay, currentRoomData
    , baseMap, effectiveMap, effectiveEqMap
    , undoMap
    , hashIndex
    , applying
    -- * Overlays and Portals
    , mkOverlayFull
    -- * Re-exports
    , grab, uncache, Undo.undoify
    -- * Undos
    , undo, redo
    ) where

import Data.Carte
import qualified Data.Map as M
import Data.Cache
import qualified Data.Undo as Undo
import Data.QuasiEq
import qualified Data.ListZipper as Z

import Control.Monad.State
import Control.Lens

import Mudblood
import Mudblood.Contrib.MG.Mapper.Portals
import Mudblood.Contrib.MG.Mapper.Types

import System.Lock.SimpleLock

-----------------------------------------------------------------------------

data R a = R
    { _mapStore     :: MapStore
    , _fileName     :: Maybe (Either FilePath (FilePath, Lock))
    , _roomHash     :: String
    , _currentRoom  :: Int
    , _walkStack    :: Z.Zipper Int
    , _overlay'     :: [String]
    , _mode         :: MapperMode
    , _walkMode     :: WalkMode
    }
  deriving (Functor)

-- | Generate an 'OverlayMap' from an 'UndoableMap' using the specified overlay and portal set.
mkOverlayFull :: [String] -> [Int] -> UndoableMap -> OverlayMap
mkOverlayFull over ps m = cache (mapOverlay ["base"] . grab) $ cache (mapAddPortals ps) $ cache (indexLookup . mapGenRoomIndex "hash" hashIndexer . Undo.current) m
  where
    hashIndexer (UserValueString h) = [UserValueString h]
    hashIndexer (UserValueArray hs) = hs
    hashIndexer _ = []
    indexLookup m s = M.lookup (UserValueString s) m

-- | The default 'R'
mkSt = R
    { _mapStore       = quasiEq $ mkOverlayFull ["base"] [1..40] $ Undo.undoify 20 mapEmpty
    , _fileName       = Nothing
    , _roomHash       = ""
    , _currentRoom    = 0
    , _walkStack      = Z.empty
    , _overlay'       = ["base"]
    , _mode           = ModeFixed
    , _walkMode       = WalkSafe
    }

data MapperMode = ModeOff | ModeFixed | ModeAuto | ModeManual | ModeUpdate
    deriving (Eq)

instance Show MapperMode where
    show ModeOff = "off"
    show ModeFixed = "fixed"
    show ModeAuto = "auto"
    show ModeManual = "manual"
    show ModeUpdate = "update"

data WalkMode = WalkFast | WalkSafe | WalkAggro
    deriving (Eq, Show)

-----------------------------------------------------------------------------

makeLenses ''R

quasiEqer :: Lens' (QuasiEq a) a
quasiEqer = lens unQuasiEq (\x y -> updateQuasiEq (const y) x)

-- | The current overlay.
overlay :: Lens' (R a) [String]
overlay = lens _overlay' (\s v -> s { _overlay' = v, _mapStore = changeOverlay v (_mapStore s) })
  where changeOverlay over = updateQuasiEq (cache (mapOverlay over . grab) . uncache)

-- | The bare map, i.e. without portals, overlays etc.
--   Use this to modify the map.
baseMap :: Lens' MapStore Map
baseMap = quasiEqer . cacher . cacher . cacher . Undo.undoer
{- baseMap = lens (Undo.current . uncache . uncache . uncache . unQuasiEq . _mapStore)
                           (\s v -> s { _mapStore = updateQuasiEq (update (update (update (Undo.undoable (const v))))) (_mapStore s) }) -}

undoMap :: Lens' MapStore (Undo.Undo Map)
undoMap = quasiEqer . cacher . cacher . cacher

-- | The effective map with portals and overlays. Read only.
effectiveMap :: Getter MapStore Map
effectiveMap = quasiEqer . grabber
{- to $ \x -> let m = x ^. mapStore
                          in grab (unQuasiEq m) `quasiEqWith` m -}

-- | The effective map with portals and overlays, wrapped in a QuasiEq. Read only.
effectiveEqMap :: Getter MapStore (QuasiEq Map)
effectiveEqMap = to (\x -> grab (unQuasiEq x) `quasiEqWith` x)
{- effectiveEqMap = to $ \x -> let m = x ^. mapStore
                            in grab (unQuasiEq m) `quasiEqWith` m -}

hashIndex :: Getter MapStore (String -> Maybe Int)
hashIndex = quasiEqer . cacher . cacher . grabber

applying :: b -> Getter (b -> c) c
applying y = to (\x -> x y)

{-
roomWithHash :: String -> Getter MapStore (Maybe Int)
roomWithHash hash = hashIndex . applying hash
-}

-- | The 'UserData' of the current 'Node'.
currentRoomData :: Lens' (R a) UserData
currentRoomData = lens getter setter
  where
    getter x = let cur = x ^. currentRoom
               in x ^. mapStore . baseMap . mapRoomData cur
    setter x y = let cur = x ^. currentRoom
                 in x & mapStore . baseMap . mapRoomData cur .~ y

-----------------------------------------------------------------------------

{-
extractHashLookup = grab . uncache . uncache
extractEffective = grab
-}

-- uncacheBase = current . uncache . uncache . uncache

undo :: (MonadState (Fix r) m, R :@: r) => m ()
undo = rec . mapStore %= (updateQuasiEq $ update $ update $ update $ Undo.undo)

redo :: (MonadState (Fix r) m, R :@: r) => m ()
redo = rec . mapStore %= (updateQuasiEq $ update $ update $ update $ Undo.redo)
