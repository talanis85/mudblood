module Mudblood.Contrib.MG.Mapper.RoomActions
    ( 
    -- * before-exit
      roomActionsBeforeExit
    -- * blockers
    , roomCheckBlockers
    -- * Exit settings
    , parseExitSettings, unparseExitSettings
    ) where

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Text.ParserCombinators.Parsec

import Mudblood
import Mudblood.Contrib.Regex

-- | Read a string array from a room's userdata
getExitStringArray :: Map       -- ^ The map
                   -> Int       -- ^ Room id
                   -> String    -- ^ Exit name
                   -> String    -- ^ Userdata key
                   -> [String]
getExitStringArray m room ex key = concat $
    case lookupUserValue key (mapGetExitData room ex (mapGraph m)) of
        UserValueArray actions -> (flip map) actions $ \a -> case a of
            UserValueString str -> [str]
            _ -> []
        _ -> []

-- | Write a string array into a room's userdata
putExitStringArray :: Int       -- ^ Room id
                   -> String    -- ^ Exit name
                   -> String    -- ^ Userdata key
                   -> [String]  -- ^ The string array
                   -> Map       -- ^ Original map
                   -> Map
putExitStringArray room ex key value =
    mapModifyGraph $ mapModifyExitData room ex $ M.insert key (UserValueArray (map UserValueString value))

-- | Return "before-exit" actions for a given exit
roomActionsBeforeExit :: Map -> Int -> String -> [TriggerEvent]
roomActionsBeforeExit m room ex = map SendTEvent $ getExitStringArray m room ex "before-exit"


mapMaybeM f l = mapM f l >>= return . catMaybes

knuddelRegexes = map matchAS'
    [ "^Wen willst Du denn knudeeln"
    , "^Du kannst soviel ich weiss"
    , "^Knuddle wen"
    , "^Wen willst Du knuddeln"
    ]

-- | Check for blockers in a given direction. Return all offending blocker names.
roomCheckBlockers :: Map -> Int -> String -> MBTrigger u [String]
roomCheckBlockers m room ex = do
    let blockers = getExitStringArray m room ex "blockers"
    ret <- (flip mapMaybeM) blockers $ \b -> do
        r <- (yieldSend $ "knuddel " ++ b) >>= guardLine
        if or $ map ($ r) knuddelRegexes
            then return $ Nothing
            else if matchAS' "^Du knuddelst" r
                    then return $ Just b
                    else failT
    return $ ret

-----------------------------------------------------------------------------

-- | Parses the following language of exit settings:
--
--   Settings ::= (Blocker | Before-exit | Weight)*
--   Blocker ::= 'blocker' String
--   Weight ::= 'weight' Integer
--   Before-exit ::= 'before-exit' String
parseExitSettings :: String -> Either String UserData
parseExitSettings str = case runParser (many (try parse_weight <|> try parse_blocker <|> try parse_before_exit) >> eof >> getState >>= return) M.empty "" str of
    Left err -> Left $ show err
    Right ud -> Right ud

-- | Generates the language of exit settings
unparseExitSettings :: UserData -> String
unparseExitSettings m =
    let blockers = getStringArray "blockers" m
        before_exit = getStringArray "before-exit" m
        weight = fromMaybe 1 $ userValueToInt $ lookupUserValue "weight" m
    in unlines $ ["weight " ++ (show weight)]
              ++ (map ("blocker " ++) blockers)
              ++ (map ("before-exit " ++) before_exit)
  where
    getStringArray key m = mconcat $
        case lookupUserValue key m of
            UserValueArray actions -> (flip map) actions $ \a -> case a of
                UserValueString str -> [str]
                _ -> []
            _ -> []

parse_blocker = do
    string "blocker"
    many1 space
    val <- many1 (noneOf "\n")
    updateState $ M.alter (\x -> Just $ userValueFromStringArray $ val : (fromMaybe [] $ x >>= userValueToStringArray)) "blockers"
    (void newline <|> eof)
    return ()

parse_before_exit = do
    string "before-exit"
    many1 space
    val <- many1 (noneOf "\n")
    updateState $ M.alter (\x -> Just $ userValueFromStringArray $ val : (fromMaybe [] $ x >>= userValueToStringArray)) "before-exit"
    (void newline <|> eof)
    return ()

parse_weight = do
    string "weight"
    many1 space
    val <- many1 digit
    updateState $ M.insert "weight" (userValueFromInt $ read val)
    (void newline <|> eof)
    return ()
