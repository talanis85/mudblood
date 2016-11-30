{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

module Mudblood.Contrib.MG.NPCDB
    ( Handle
    , connect
    , addArea, addNPC
    , npcForRoom, npcForArea
    , done, done', undo
    , NPC (..)

    , addNPCDiff, getLastNPCDiff
    , plakette
    ) where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.Convertible
import Data.Either
import Data.Typeable

import qualified Database.HDBC.Sqlite3 as SQLite
import Database.HDBC
import Database.HDBC
import Text.Printf

import Mudblood hiding (connect)

type Handle = SQLite.Connection

catchSQLError :: (MonadError StackTrace m, MonadIO m) => String -> IO r -> m r
catchSQLError msg m = liftIO (catchSql (liftM Right m) (return . Left . show)) >>= either (throwError . stackTrace "npcdb") return

connect path = do
    c <- SQLite.connectSqlite3 path
    SQLite.setBusyTimeout c 1000
    runRaw c $ "PRAGMA foreign_keys = ON;"
    return c

addArea :: (MonadIO m, MonadError StackTrace m) => Handle -> String -> m ()
addArea h name = do
  catchSQLError "Adding area" $ withTransaction h $ \h -> run h "INSERT INTO area (name) VALUES (?);" [toSql name]
  return ()

addNPC :: (MonadIO m, MonadError StackTrace m) => Handle -> String -> String -> Maybe Int -> m ()
addNPC h name area room = do
    r <- catchSQLError "Checking if npc already exists" $
            quickQuery' h "SELECT name FROM npc WHERE name=? AND area=?;"
                          [toSql name, toSql area]
    case (room, r) of
      (Nothing, []) -> do
        catchSQLError "Insert npc" $ withTransaction h $ \h -> do
          run h "INSERT INTO npc (name, area) VALUES (?, ?)"
                [toSql name, toSql area]
      (Nothing, _) -> do
        throwError $ stackTrace "npcdb" "NPC already known in that region"
      (Just room', []) -> do
        catchSQLError "Insert npc and associate with room" $ withTransaction h $ \h -> do
          run h "INSERT INTO npc (name, area) VALUES (?, ?);" [toSql name, toSql area]
          run h "INSERT INTO npc_room (name, area, roomId) VALUES (?, ?, ?)" [toSql name, toSql area, toSql room']
      (Just room', (_:_)) -> do
        catchSQLError "Associating npc with room" $ withTransaction h $ \h -> do
          run h "INSERT INTO npc_room (name, area, roomId) VALUES (?, ?, ?);"
                [toSql name, toSql area, toSql room']
    return ()

done h char name area = do
    catchSQLError "Setting npc as done" $ withTransaction h $ \h -> do
      run h "INSERT INTO npc_done (name, area, player, date) VALUES (?, ?, ?, datetime('now'));"
            [toSql name, toSql area, toSql char]
    return ()

done' h char name area = do
    catchSQLError "Setting npc as done" $ withTransaction h $ \h -> do
      run h "INSERT INTO npc_done (name, area, player) VALUES (?, ?, ?);"
            [toSql name, toSql area, toSql char]
    return ()

undo h char name area = do
    catchSQLError "Setting npc as todo" $ withTransaction h $ \h -> do
      run h "DELETE FROM npc_done WHERE name=? AND area=? AND player=?;"
            [toSql name, toSql area, toSql char]
    return ()

data NPC = NPC
  { npcName :: String
  , npcArea :: String
  , npcRooms :: String
  , npcDone :: Bool
  }
  deriving (Typeable, Show)

extractResult :: [SqlValue] -> ConvertResult NPC
extractResult r = do
  if length r < 3
     then convError "Invalid NPC record" r
     else do
       let (name':area':rooms':done':_) = r
       name  <- safeConvert name'
       area  <- safeConvert area'
       rooms <- safeConvert rooms'
       done  <- safeConvert done' :: ConvertResult Int
       return $ NPC name area rooms (if done == 0 then False else True)

npcForRoom h char room = do
    r <- catchSQLError "Querying npcs for room" $
      quickQuery' h
        "SELECT npc.name as name, \
        \       npc.area as area, \
        \       group_concat(npc_room.roomId) as roomId, \
        \       count((select player from npc_done where npc_done.name == npc.name and npc_done.area == npc.area and npc_done.player == ?)) as done \
        \FROM npc, \
        \     npc_room \
        \WHERE npc_room.roomId=? AND \
        \      npc.name = npc_room.name AND npc.area = npc_room.area \
        \GROUP BY npc.name, npc.area;"
        [toSql char, toSql room]
    return $ rights $ map extractResult r

npcForArea h char area = do
    r <- catchSQLError "Querying npcs for area" $
      quickQuery' h
        "SELECT npc.name as name, \
        \       npc.area as area, \
        \       group_concat(npc_room.roomId) as roomId, \
        \       count((select player from npc_done where npc_done.name == npc.name and npc_done.area == npc.area and npc_done.player == ?)) as done \
        \FROM npc,npc_room \
        \WHERE npc.area = ? \
        \GROUP BY npc.name, npc.area;"
        [toSql char, toSql area]
    return $ rights $ map extractResult r

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
    catchSQLError "Adding NPC diff" $ withTransaction h $ \h -> do
      run h "INSERT INTO npc_diff (player, other, date, added, missing) VALUES (?, ?, datetime('now'), ?, ?);"
            [toSql char, toSql name, toSql added, toSql missing]
      commit h
    return ()

getLastNPCDiff :: (MonadIO m, MonadError StackTrace m) => Handle -> String -> String -> m (Int, Int)
getLastNPCDiff h char name = do
    r <- catchSQLError "Getting last diff" $ withTransaction h $ \h -> do
           quickQuery' h "SELECT added,missing FROM npc_diff WHERE player=? AND other=? ORDER BY date DESC;"
                         [toSql char, toSql name]
    case r of
      ([added, missing]:_) -> do
        let converted = do
              added'   <- safeConvert added
              missing' <- safeConvert missing
              return (added', missing')
        case converted of
          Left _ -> throwError $ stackTrace "npcdb" "Invalid row"
          Right (added, missing) -> return (added, missing)
      _ -> return (0, 0)

fetchPlakette :: (Monad m, MBEvent a) => Parser (Ev a) m (String, Int, Int)
fetchPlakette = do
    (count, name) <- fetchLineRegex2 "^Du hast ([[:digit:]]+) Monster getoetet, die ([[:word:]]+) noch nicht getoetet hat"
    count' <- fetchLineRegex1 $ "^" ++ name ++ " hat ([[:digit:]]+) Monster getoetet, die Du noch nicht getoetet hast"

    return (name, read count, read count')

diffPlakette :: (MonadError StackTrace m, MonadIO m) => Handle -> String -> (String, Int, Int) -> m (Int, Int)
diffPlakette h char (name, added, missing) = do
    (added', missing') <- getLastNPCDiff h char name
    return (added - added', missing - missing')

-- plakette :: (Game s m, MonadIO m) => Handle -> String -> Iteration (Ev a) m ()
plakette h char = do
    (name, added, missing) <- parseU $ fetchPlakette
    (added', missing') <- lift $ diffPlakette h char (name, added, missing)
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
