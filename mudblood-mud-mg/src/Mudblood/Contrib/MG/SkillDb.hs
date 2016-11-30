{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.SkillDb
    ( R, component
    , skillC

    , skillToPercent
    ) where

import Mudblood
import Mudblood.Component.Assets
import qualified Database.SQLite as SQL

import Control.Lens
import Control.Monad
import Text.Printf

--------------------------------------------------------------------------------------------------

type SkillDbHandle = SQL.SQLiteHandle

data R a = R
    { _stHandle :: Maybe SkillDbHandle
    }
  deriving (Functor)

mkSt = R
    { _stHandle = Nothing
    }

makeLenses ''R

--------------------------------------------------------------------------------------------------

schema = "CREATE TABLE skills ( \
        \   name STRING, \
        \   value INTEGER, \
        \   date INTEGER, \
        \   PRIMARY KEY (name, date) \
        \ );"

--------------------------------------------------------------------------------------------------

component :: (Assets :@: r, MonadIO m) => MBComponent m e (Fix r) (Fix (R :*: r))
component = stateC mkSt
        >>> bootC loadSkillDb

skillC querySkills = commandC "skills" "" "Fragt Deine Gildenfaehigkeiten ab." $ lift $ dispatch $ oneshot $ skillTrigger querySkills

--------------------------------------------------------------------------------------------------

loadSkillDb :: (Assets :@: r, R :@: r, MonadIO m) => MBX e (Fix r) m ()
loadSkillDb = do
    path <- getCharAssetPath "skills"
    h <- liftIO $ SQL.openConnection path
    liftIO $ SQL.execStatement_ h schema
    rec . stHandle .= Just h

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

updateSkills :: SkillDbHandle -> [(String, Int)] -> IO [(String, Int, Int)]
updateSkills h = mapM $ \(sk, v) -> do
    oldv <- readSkill h sk
    let diff = v - oldv
    when (diff /= 0) $ writeSkill h sk v
    return (sk, v, diff)

displaySkills :: (Monad m, LineEvent :<: e) => [(String, Int, Int)] -> Iteration (Ev e) m ()
displaySkills = mapM_ $ \(sk, v, d) -> do
    yieldLine $ toAS $ printf "-- %30s : %d%% (+%d)" sk v d

-- skillTrigger :: (MonadState (ORec r) m, Has R r, MBEvent a, MonadIO m) => Iteration (Ev a) m [(String, Int)] -> Iteration (Ev a) m ()
skillTrigger querySkills = do
  skilldb <- lift $ use $ rec . stHandle
  case skilldb of
      Nothing -> yieldError (stackTrace "skilldb" "No database loaded")
      Just skilldb -> do
          s <- querySkills
          s' <- lift $ liftIO $ updateSkills skilldb s
          displaySkills s'

position :: (Eq a) => a -> [a] -> Maybe Int
position v l = position' 0 v l
    where position' i v [] = Nothing
          position' i v (x:xs) = if x == v then Just i else position' (i+1) v xs

skillToPercent :: [String] -> String -> Maybe Int
skillToPercent levels val = fmap calcpercent $ position val levels
    where calcpercent x = ((x+1) * 100) `div` length levels
