module Mudblood.Contrib.SkillDb
    ( SkillDbHandle
    , openSkillDb
    , readSkill, writeSkill
    , updateSkills, displaySkills
    ) where

import Mudblood
import qualified Database.SQLite as SQL

import Control.Monad
import Text.Printf

schema = "CREATE TABLE skills ( \
        \   name STRING, \
        \   value INTEGER, \
        \   date INTEGER, \
        \   PRIMARY KEY (name, date) \
        \ );"

type SkillDbHandle = SQL.SQLiteHandle

openSkillDb path = do
    h <- SQL.openConnection path
    SQL.execStatement_ h schema
    return h

readSkill :: SQL.SQLiteHandle -> String -> IO Int
readSkill sql sk = do
    res <- SQL.execStatement sql $ "SELECT value FROM skills WHERE name='" ++ sk ++ "' ORDER BY date DESC"
    return $ case res of
        Left err -> 0
        Right [] -> 0
        Right (r:_) -> case r of
            (((_, SQL.Int v):_):_) -> fromIntegral v
            _ -> 0

writeSkill :: SQL.SQLiteHandle -> String -> Int -> IO ()
writeSkill sql sk val = do
    SQL.execStatement_ sql $
        "INSERT INTO skills (name, value, date) VALUES ('" ++ sk ++ "', " ++ show val ++ ", strftime('%s', 'now'))"
    return ()

updateSkills :: (MBMonad m u) => SkillDbHandle -> [(String, Int)] -> m [(String, Int, Int)]
updateSkills h = mapM $ \(sk, v) -> do
    oldv <- io $ readSkill h sk
    let diff = v - oldv
    when (diff /= 0) $ io $ writeSkill h sk v
    return (sk, v, diff)

displaySkills :: (MBMonad m u) => [(String, Int, Int)] -> m ()
displaySkills = mapM_ $ \(sk, v, d) -> do
    echo $ printf "-- %s : %d%% (+%d)" sk v d
