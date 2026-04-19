{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Mudblood.Contrib.MG.NPCDB
    ( R, component
    , getNPCDB
    , Handle
    , NPC (..)
    , addArea, addNPC
    , npcForRoom, npcForArea
    , done, done', undo

    , addNPCDiff, getLastNPCDiff
    , plakette

    , component
    -- * Commands
    , npclistCmd
    , npcdoneCmd
    , npcaddCmd
    , npcaddhereCmd
    ) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.Monoid
import Data.Bifunctor

import qualified Database.SQLite.Simple as SQL
import Text.Printf

import Mudblood hiding (connect)
import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.Mapper as Mapper
import Mudblood.Component.Assets

--------------------------------------------------------------------------------------------------

type Handle = SQL.Connection

data R a = R
    { _stHandle :: Maybe Handle
    }
  deriving (Functor)

mkSt = R
    { _stHandle = Nothing
    }

makeLenses ''R

--------------------------------------------------------------------------------------------------

component :: (Assets :@: r, Char.R :@: r, Mapper.R :@: r, Screen m, MonadIO m) => MBComponent m e (Fix r) (Fix (R :*: r))
component = stateC mkSt
        >>> bootC loadNPCDB
        >>> commands

getNPCDB = use $ rec . stHandle

--------------------------------------------------------------------------------------------------

getNPCDB' = do
    npcdb <- use $ rec . stHandle
    case npcdb of
        Nothing -> throwError (stackTrace "npcdb" "No database loaded")
        Just npcdb -> return npcdb

showNPCList l =
    forM_ l $ \npc -> echo $ setFg Green $ toAS $
        printf "- %s - %s - %s" (npcName npc) (npcArea npc) (if npcDone npc then "DONE" :: String else "TODO" :: String)

npclistCmd = mkCommand "npclist" "Zeigt alle NPCs im angegebenen Gebiet." $
  f <$> arg stringParser "gebiet" "Gebiet"
    where
      f area = do
        npcdb <- getNPCDB'
        char  <- use $ rec . Char.name
        npcs  <- npcForArea npcdb char area
        showNPCList npcs

npcdoneCmd = mkCommand "npcdone" "Markiert den angegebenen NPC als erledigt." $
  f <$> arg stringParser "gebiet" "Gebiet"
    <*> arg stringParser "npc" "NPC"
    where
      f area name = do
        npcdb <- getNPCDB'
        char  <- use $ rec . Char.name
        done npcdb char name area
        echo $ toAS $ printf "Marked '%s' (%s) as DONE" name area

npcaddCmd = mkCommand "npcadd" "Erstellt einen neuen NPC." $
  f <$> arg stringParser "gebiet" "Gebiet"
    <*> arg stringParser "npc" "NPC"
    where
      f area name = do
        npcdb <- getNPCDB'
        char  <- use $ rec . Char.name
        addNPC npcdb name area Nothing
        echo $ toAS $ printf "Added '%s' (%s)" name area

npcaddhereCmd = mkCommand "npcaddhere" "Erstellt einen neuen NPC im aktuellen Raum." $
  f <$> arg stringParser "gebiet" "Gebiet"
    <*> arg stringParser "npc" "NPC"
    where
      f area name = do
        npcdb <- getNPCDB'
        char  <- use $ rec . Char.name
        room  <- use $ rec . Mapper.currentRoom
        addNPC npcdb name area (Just room)
        echo $ toAS $ printf "Added '%s' (%s) for room #%d" name area room

commands = mconcat
  [ commandC npclistCmd
  , commandC npcdoneCmd
  , commandC npcaddCmd
  , commandC npcaddhereCmd
  ]

--------------------------------------------------------------------------------------------------

hoistJust = maybe (return ()) throwError

catchSQLError :: (MonadIO m, MonadError StackTrace m) => String -> IO a -> m a
catchSQLError msg m = do
  r <- liftIO (try m)
  case r of
    Left (e :: SomeException) -> throwError (stackTrace "npcdb" "SQL Error")
    Right x -> return x

loadNPCDB :: (Assets :@: r, R :@: r, MonadIO m) => MBX e (Fix r) m ()
loadNPCDB = do
    path <- getGameAssetPath "npcdb"
    h <- liftIO $ SQL.open path
    liftIO $ SQL.execute_ h $ "PRAGMA foreign_keys = ON;"
    -- liftIO $ SQL.execStatement_ h schema
    rec . stHandle .= Just h

{-
connect path = do
    c <- SQL.openConnection path
    SQL.execStatement_ c $ "PRAGMA foreign_keys = ON;"
    return c
-}

addArea :: (MonadIO m, MonadError StackTrace m) => Handle -> String -> m ()
addArea h name = do
    catchSQLError "Adding area" $ SQL.execute h "INSERT INTO area (name) VALUES (?);" (SQL.Only name)

addNPC :: (MonadIO m, MonadError StackTrace m) => Handle -> String -> String -> Maybe Int -> m ()
addNPC h name area room = do
    r <- catchSQLError "Checking if npc already exists" $ SQL.query h
        "SELECT * FROM npc WHERE name=? AND area=?;" (name, area)
    case (room, r :: [NPC]) of
        (Nothing, []) -> do
            catchSQLError "Insert npc" $ SQL.execute h
                "INSERT INTO npc (name, area) VALUES (?,?);" (name, area)
        (Nothing, (_:_)) -> do
            throwError $ stackTrace "npcdb" "NPC already known in that region"
        (Just room', []) -> do
            catchSQLError "Insert npc" $ SQL.execute h
                "INSERT INTO npc (name, area) VALUES (?,?);" (name, area)
            catchSQLError "Associating npc with room" $ SQL.execute h
                "INSERT INTO npc_room (name, area, roomId) VALUES (?,?,?);" (name, area, room')
        (Just room', (_:_)) -> do
            catchSQLError "Associating npc with room" $ SQL.execute h
                "INSERT INTO npc_room (name, area, roomId) VALUES (?,?,?);" (name, area, room')

done h char name area = do
    catchSQLError "Setting npc as done" $ SQL.execute h
        "INSERT INTO npc_done (name, area, player, date) VALUES (?,?,?,datetime('now'));" (name, area, char)

done' h char name area = do
    catchSQLError "Setting npc as done" $ SQL.execute h
        "INSERT INTO npc_done (name, area, player) VALUES (?,?,?);" (name, area, char)

undo h char name area = do
    catchSQLError "Setting npc as todo" $ SQL.execute h
        "DELETE FROM npc_done WHERE name=? AND area=? AND player=?;" (name, area, char)

{-
extractResult r = do
    name <- lookup "name" r
    area <- lookup "area" r
    rooms <- lookup "rooms" r >>= sqliteToString
    done <- lookup "done" r >>= sqliteToInt
    return $ NPC
      { npcName = name
      , npcArea = area
      , npcRooms = rooms
      , npcDone = if done == 0 then False else True
      }
-}

data NPC = NPC
  { npcName :: String
  , npcArea :: String
  , npcRooms :: String
  , npcDone :: Bool
  }
  deriving (Show)

instance SQL.FromRow NPC where
  fromRow = NPC <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

npcForRoom :: (MonadError StackTrace m, MonadIO m) => SQL.Connection -> String -> Int -> m [NPC]
npcForRoom h char room = do
    r <- catchSQLError "Querying npcs for room" $ SQL.query h
        "SELECT npc.name as name, \
        \       npc.area as area, \
        \       group_concat(npc_room.roomId) as rooms, \
        \       count((select player from npc_done where npc_done.name == npc.name and npc_done.area == npc.area and npc_done.player == ?)) as done \
        \FROM npc, \
        \     npc_room \
        \WHERE npc_room.roomId=? AND \
        \      npc.name = npc_room.name AND npc.area = npc_room.area \
        \GROUP BY npc.name, npc.area;"
        (char, room)
    return r

npcForArea :: (MonadError StackTrace m, MonadIO m) => SQL.Connection -> String -> String -> m [NPC]
npcForArea h char area = do
    r <- catchSQLError "Querying npcs for area" $ SQL.query h
        "SELECT npc.name as name, \
        \       npc.area as area, \
        \       group_concat(npc_room.roomId) as rooms, \
        \       count((select player from npc_done where npc_done.name == npc.name and npc_done.area == npc.area and npc_done.player == ?)) as done \
        \FROM npc, npc_room \
        \WHERE npc.area = ? \
        \GROUP BY npc.name, npc.area;"
        (char, area)
    return r

{-
sqliteToString :: SQL.Value -> Maybe String
sqliteToString v = case v of
    SQL.Text s -> Just s
    _ -> Nothing

sqliteToInt :: SQL.Value -> Maybe Int
sqliteToInt v = case v of
    SQL.Int s -> Just (fromIntegral s)
    _ -> Nothing
-}

addNPCDiff :: (MonadError StackTrace m, MonadIO m) => Handle -> String -> String -> Int -> Int -> m ()
addNPCDiff h char name added missing = do
    catchSQLError "Adding NPC diff" $ SQL.execute h
        "INSERT INTO npc_diff (player, other, date, added, missing) VALUES (?,?,datetime('now'),?,?);"
        (char, name, added, missing)
    return ()

getLastNPCDiff :: Handle -> String -> String -> IO (Int, Int)
getLastNPCDiff h char name = do
    r <- SQL.query h
        "SELECT * FROM npc_diff WHERE player=? AND other=? ORDER BY date DESC;"
        (char, name)
    case r of
        [] -> return (0, 0)
        (r:_) -> return r

fetchPlakette :: (MBEvent a, Monad m, MonadFail m) => Parser (Ev a) m (String, Int, Int)
fetchPlakette = do
    (count, name) <- fetchLineRegex2 "^Du hast ([[:digit:]]+) Monster getoetet, die ([[:word:]]+) noch nicht getoetet hat"
    count' <- fetchLineRegex1 $ "^" ++ name ++ " hat ([[:digit:]]+) Monster getoetet, die Du noch nicht getoetet hast"

    return (name, read count, read count')

diffPlakette :: Handle -> String -> (String, Int, Int) -> IO (Int, Int)
diffPlakette h char (name, added, missing) = do
    (added', missing') <- getLastNPCDiff h char name
    return (added - added', missing - missing')

-- plakette :: (Game s m, MonadIO m) => Handle -> String -> Iteration (Ev a) m ()
plakette h char = do
    (name, added, missing) <- parse $ fetchPlakette
    (added', missing') <- liftIO $ diffPlakette h char (name, added, missing)
    lift $ addNPCDiff h char name added missing
    yieldLines $ map toAS
      [ "Du hast " ++ show added ++ " Monster getoetet, die " ++ name ++ " noch nicht getoetet hat."
      , if added' > 0
           then "Seit dem letzten Check hast Du " ++ show added' ++ " neue Monster gefunden."
           else if added' == 0
                   then "Seit dem letzten Check hat sich hier nichts geaendert."
                   else "Seit dem letzten Check hat " ++ name ++ " " ++ show (abs added') ++ " Monster aufgeholt."
      , name ++ " hat " ++ show missing ++ " Monster getoetet, die Du noch nicht getoetet hast."
      , if missing' > 0
           then "Seit dem letzten Check hat " ++ name ++ " " ++ show missing' ++ " neue Monster gefunden."
           else if missing' == 0
                   then "Seit dem letzten Check hat sich hier nichts geaendert."
                   else "Seit dem letzten Check hast Du " ++ show (abs missing') ++ " Monster aufgeholt."
      ]
