{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mudblood.Contrib.MG.Mapper
    ( module Mudblood.Contrib.MG.Mapper.State
    , module Mudblood.Contrib.MG.Mapper.Portals
    , module Mudblood.Contrib.MG.Mapper.RoomActions
    , module Mudblood.Contrib.MG.Mapper.UserData

    , component, menu

    -- * Map queries
    , findRoom, findPath, findPathFromCurrent
    -- , roomArea
    -- , checkCurrentHash
    -- * Map actions
    , walkTo, walkUndo, walkRedo
    -- * Stepper functions
    , fastStepper, safeStepper
    , modeStepper
    -- * Triggers
    , inRoom
    -- , guardCurrentHash
    --, triggerGmcpRoom
    , roomTrigger
    -- * Widgets
    -- , mapperWidgets
    -- * Commands
    , walkmodeCmd
    , tagCmd
    , roominfoCmd
    , newroomCmd
    , addexitCmd
    , rmexitCmd
    , rmroomCmd
    , clearhashCmd
    , splitCmd
    , weightCmd
    , addblockerCmd
    , rmblockerCmd
    , saferoomCmd
    , unsaferoomCmd
    ) where

import Data.Carte
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Data.List (intercalate)
import qualified Data.ListZipper as Z

import qualified Control.Exception as Exception
import Control.Monad.Trans.Maybe
import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Control.Lens

import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.State hiding (forM_)
import Control.Applicative ((<|>))

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Queue

import Data.QuasiEq
import System.Lock.SimpleLock
import Data.GMCP
import Mudblood

import Mudblood.Component.Assets

import Mudblood.Contrib.MG.Mapper.State
import Mudblood.Contrib.MG.Mapper.MGMap
import Mudblood.Contrib.MG.Mapper.Portals
import Mudblood.Contrib.MG.Mapper.RoomActions
import Mudblood.Contrib.MG.Mapper.UserData
import Mudblood.Contrib.MG.Event

import qualified Mudblood.Contrib.MG.Combat as Combat
import qualified Mudblood.Contrib.MG.Char as Char

import Text.Printf

------------------------------------------------------------------------------

standardExits =
    [ ("n", "s")
    , ("no", "sw")
    , ("o", "w")
    , ("so", "nw")
    , ("s", "n")
    , ("sw", "no")
    , ("w", "o")
    , ("nw", "so")
    , ("u", "ob")
    , ("ob", "u")
    ]

------------------------------------------------------------------------------

echoMapper str = do
  echo $ toAS $ "[MAPPER] " ++ str

------------------------------------------------------------------------------

-- inRoom :: (Monad m, MonadState) => String -> Parser (Ev a) m r
inRoom h = do
  cur    <- lift $ use $ rec . currentRoomData . roomValue . mgRoomHash
  curh   <- lift $ use $ rec . roomHash
  guard $ case curh of
            Nothing -> False
            Just hash -> hash `elem` cur

------------------------------------------------------------------------------

component :: (Assets :@: r, Screen m, MonadIO m, MonadFail m, MGEvent e) => [Int] -> MBComponent m e (Fix r) (Fix (R :*: r))
component portals = stateC (mkSt portals)
        >>> triggerC 100 roomTrigger
        >>> bootC loadMap
        >>> shutdownC releaseMap
        >>> paraRoomC
        >>> statusC (("overlay: " ++) <$> show <$> use (rec . overlay))
        >>> statusC (("mapper: " ++) <$> show <$> use (rec . mode))
        >>> commands

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe e = maybe (throwError e) return

walkmodeCmd = mkCommand "walkmode" "Setzt den Speedwalkmodus" $
  f <$> arg (enumParser ["safe", "fast", "aggro"]) "modus" "'safe', 'fast' oder 'aggro'"
    where
      f mode = case mode of
        "safe"  -> rec . walkMode .= WalkSafe
        "fast"  -> rec . walkMode .= WalkFast
        "aggro" -> rec . walkMode .= WalkAggro
        _       -> throwError (stackTrace "mapper" "Unknown walk mode")

tagCmd = mkCommand "tag" "Weist dem aktuellen Raum einen Kurznamen zu" $
  f <$> arg stringParser "tag" "Kurzname"
    where
      f newtag = rec . currentRoomData . roomTag .= Just newtag

roominfoCmd = mkCommand "roominfo" "Zeigt eine Uebersicht des aktuellen Raums an" $
  pure f
    where
      f = do
        cur <- use $ rec . currentRoom
        m   <- use $ rec . baseMap
        let exits = mapGetExits cur m
            hash = m ^. mapRoomData cur . roomValue . mgRoomHash
        echo $ toAS $ "Id:   " ++ show cur
        echo $ toAS $ "Hash: " ++ show hash
        echo $ toAS $ "Exits:"
        let formatExit (n, d) =
              let headline = printf " * %-20s %-5d %-5s %s"
                                    (d ^. exitKey) n (d ^. exitLayer) (if d ^. exitSplit then "SPLIT" else "")
                  blockers = case d ^. exitValue . mgExitBlockers of
                               [] -> ""
                               xs -> "\n   blockers: " ++ intercalate ", " xs
                  beforeexits = case d ^. exitValue . mgExitBeforeExit of
                               [] -> ""
                               xs -> "\n   before-exit: " ++ intercalate ", " xs
              in headline ++ blockers ++ beforeexits
        mapM_ (echo . toAS . formatExit) exits

newroomCmd = mkCommand "newroom" "Erstellt einen neuen, isolierten Raum" $
  pure f
    where
      f = do
        m <- use $ rec . baseMap
        (m', r) <- liftMaybe (stackTrace "mapper" "Could not create room") $ mapAddRoom initMGRoomData m
        rec . baseMap .= m'
        rec . currentRoom .= r

addexitCmd = mkCommand "addexit" "Erstellt einen neuen Ausgang im aktuellen Raum" $
  f <$> arg intParser "para" "Parallelweltnummer (0 fuer normal)"
    <*> arg stringParser "ausgang" "Name des Ausgangs"
    <*> arg stringParser "zielraum" "Zielraum. Bei '#' wird ein neuer Zielraum erstellt"
    where
      f para exit room = do
        cur   <- use $ rec . currentRoom
        m     <- use $ rec . baseMap
        layer <- parseParaLayer para

        case room of
          "#" -> do
            (m', r) <- liftMaybe (stackTrace "mapper" "Could not create room") $ mapAddRoom initMGRoomData m
            rec . baseMap .= mapAddExit cur exit r layer initMGExitData m'
          room -> do
            roomId <- findRoom room >>= liftMaybe (stackTrace "mapper" "Could not find target room")
            rec . baseMap %= mapAddExit cur exit roomId layer initMGExitData

rmexitCmd = mkCommand "rmexit" "Loescht den angegebenen Ausgang" $
  f <$> arg intParser "para" "Parallelweltnummer (0 fuer normal)"
    <*> arg stringParser "ausgang" "Name des Ausgangs"
    where
      f para exit = do
        cur   <- use $ rec . currentRoom
        layer <- parseParaLayer para
        rec . baseMap %= mapDeleteExit cur exit layer

rmroomCmd = mkCommand "rmroom" "Loescht den angegebenen Raum" $
  f <$> arg stringParser "raum" "tag, #raumnummer oder $hash"
    where
      f room = do
        roomId <- findRoom room >>= liftMaybe (stackTrace "mapper" "Could not find room")
        rec . baseMap %= mapDeleteRoom roomId

clearhashCmd = mkCommand "clearhash" "Loescht den Hash des aktuellen Raums" $
  pure f
    where
      f = rec . currentRoomData . roomValue . mgRoomHash .= []

splitCmd = mkCommand "split" "Aktiviert oder deaktiviert das split-Flag fuer den angegebenen Ausgang" $
  f <$> arg stringParser "ausgang" "Zu teilender Ausgang"
    where
      f exit = rec . exitDataHere exit Nothing . exitSplit %= not

weightCmd = mkCommand "weight" "Setzt das Kantengewicht des angegebenen Ausgangs" $
  f <$> arg stringParser "ausgang" "Gewuenschter Ausgang"
    <*> arg intParser "gewicht" "Gewuenschtes Gewicht"
    where
      f exit weight = do
        when (weight < 1) $ throwError $ stackTrace "mapper" "Das Gewicht muss groesser als 0 sein."
        rec . exitDataHere exit Nothing . exitValue . mgExitWeight .= weight

addblockerCmd = mkCommand "addblocker" "Fuegt einen Blocker zu einem Ausgang hinzu" $
  f <$> arg stringParser "ausgang" "Ausgang"
    <*> arg stringParser "npc" "Name des Blockers"
    where 
      f exit name = rec . exitDataHere exit Nothing . exitValue . mgExitBlockers %= (++ [name])

rmblockerCmd = mkCommand "rmblocker" "Entfernt einen Blocker von einem Ausgang" $
  f <$> arg stringParser "ausgang" "Ausgang"
    <*> arg stringParser "npc" "Name des Blockers"
    where
      f exit name = rec . exitDataHere exit Nothing . exitValue . mgExitBlockers %= (filter (/= name))

addbeforeexitCmd = mkCommand "addbeforeexit" "Fuegt einen Befehl ein, der vor dem Benutzen des Ausgangs ausgefuehrt werden soll." $
  f <$> arg stringParser "ausgang" "Ausgang"
    <*> arg stringParser "befehl" "Befehl"
    where
      f exit command = rec . exitDataHere exit Nothing . exitValue . mgExitBeforeExit %= (++ [command])

clearbeforeexitCmd = mkCommand "clearbeforeexit" "Loescht die before-exit-Befehle des Ausgangs" $
  f <$> arg stringParser "ausgang" "Ausgang"
    where
      f exit = rec . exitDataHere exit Nothing . exitValue . mgExitBeforeExit .= []

saferoomCmd = mkCommand "saferoom" "Markiert den aktuellen Raum als sicher" $
  pure f
    where
      f = do
        cur <- use (rec . currentRoom)
        rec . baseMap . mapRoomData cur . roomValue . mgRoomSafe .= True

unsaferoomCmd = mkCommand "unsaferoom" "Markiert den aktuellen Raum als nicht sicher" $
  pure f
    where
      f = do
        cur <- use (rec . currentRoom)
        rec . baseMap . mapRoomData cur . roomValue . mgRoomSafe .= False

splitroomCmd = mkCommand "splitroom" "Teile den aktuellen Raum nach Raum-IDs auf" $
  pure f
    where
      f = do
        m <- use $ rec . baseMap
        cur <- use $ rec . currentRoom
        curIds <- use $ rec . baseMap . mapRoomData cur . roomValue . mgRoomHash
        case curIds of
          [x] -> return ()
          (x:xs) -> do
            rec . baseMap . mapRoomData cur . roomValue . mgRoomHash .= [x]
            mapM_ (splitRoom cur) xs
      splitRoom cur hash = do
        m <- use $ rec . baseMap
        case mapAddRoom (mgRoomHash .~ [hash] $ initMGRoomData) m of
          Just (m', newroom) -> do
            rec . baseMap .= m'
            mapM_ (splitEntrance newroom) $ mapGetEntrances cur m'
          Nothing -> throwError $ stackTrace "mapper" "Cannot create room"
      splitEntrance newroom (room, ed) =
        rec . baseMap %= mapAddExit room (ed ^. exitKey) newroom (ed ^. exitLayer) initMGExitData

pararoomCmd = mkCommand "pararoom" "Verlege aktuellen Raum in eine Parallelwelt" $
  f <$> arg intParser "para" "Parallelweltnummer"
    where
      f para = do
        m <- use $ rec . baseMap
        cur <- use $ rec . currentRoom
        mapM_ (setPara cur para) $ mapGetEntrances cur m
      setPara room para (entrance, ed) = do
        layerName <- getLayerName para
        rec . baseMap . mapExitDataByNodes entrance room . exitLayer .= layerName
      getLayerName 0 = return "base"
      getLayerName para
        | para < 0 || para > 7 = throwError $ stackTrace "mapper" "Argument muss zwischen 0 und 7 liegen"
        | otherwise = return $ "p" ++ show para

commands = mconcat $
    [ commandC walkmodeCmd
    , commandC tagCmd
    , commandC roominfoCmd
    , commandC newroomCmd
    , commandC addexitCmd
    , commandC rmexitCmd
    , commandC rmroomCmd
    , commandC clearhashCmd
    , commandC splitCmd
    , commandC weightCmd
    , commandC addblockerCmd
    , commandC rmblockerCmd
    , commandC addbeforeexitCmd
    , commandC clearbeforeexitCmd
    , commandC saferoomCmd
    , commandC unsaferoomCmd
    , commandC splitroomCmd
    , commandC pararoomCmd
    ]

parseParaLayer para
    | para < 0 || para > 7 = throwError (stackTrace "mapper" "Invalid para number")
    | para == 0            = return "base"
    | otherwise            = return $ "p" ++ show para

menu :: (Screen m, MonadIO m, MonadFail m, R :@: r, MGEvent e) => MBComponent m e (Fix r) (Fix r)
menu = describe "Mapper" menu'
  where
    menu' = mconcat
        [ submenu (KAscii 'm') modeMenu
        , bindArg (KAscii 'w') "Walk" $ \r -> do
            r' <- findRoom r
            r'' <- liftMaybe (stackTrace "mapper" "Destination not found") r'
            dispatch $ walkTo modeStepper r''
        , bind (KAscii 'b') "Walk undo" $ dispatch $ walkUndo modeStepper
        , submenu (KAscii 's') $ describe "Save" $ bind (KAscii 's') "Really save" saveMap
        ]

modeMenu = describe "Mode" $ mconcat
    [ bind (KAscii 'f') "Fixed"  $ rec . mode .= ModeFixed
    , bind (KAscii 'o') "Off"    $ rec . mode .= ModeOff
    , bind (KAscii 'a') "Auto"   $ rec . mode .= ModeAuto
    , bind (KAscii 'm') "Manual" $ rec . mode .= ModeManual
    , bind (KAscii 'u') "Update" $ rec . mode .= ModeUpdate
    ]

loadMap :: (Screen m, MonadIO m, Assets :@: r, R :@: r) => MBX e (Fix r) m ()
loadMap = do
    mapPath <- getGameAssetPath "map"
    mapfile <- liftIO $ Exception.try $ readFile mapPath
    m <- case mapfile of
        Left (e :: Exception.IOException) -> do
            echoLog $ "Creating map: " ++ mapPath
            return $ mapEmpty initMGRoomData
        Right mapfile -> do
            echoLog $ "Loading map: " ++ mapPath
            case mapFromString mapfile of
                Nothing -> do
                    echoError $ stackTrace "MAPPER" $ "Invalid map file: " ++ mapPath
                    return $ mapEmpty initMGRoomData
                Just m -> return m
    rec . baseMap .= m
    lock <- liftIO $ acquire mapPath
    case lock of
      Nothing -> do
        echoLog "Map is read-only."
        rec . fileName .= Just (Left mapPath)
      Just lock -> do
        rec . fileName .= Just (Right (mapPath, lock))

saveMap :: (Screen m, MonadIO m, R :@: r) => MBX e (Fix r) m ()
saveMap = do
    m  <- use $ rec . baseMap
    fn <- use $ rec . fileName
    case fn of
        Nothing         -> throwError $ stackTrace "mapper" "No file name given"
        Just (Left fn)  -> throwError $ stackTrace "mapper" "Map is in read-only mode"
        Just (Right (fn, lock)) -> do
            liftIO $ writeFile fn $ mapToString m
            echo $ toAS $ "Written map to " ++ fn

releaseMap = do
    fn <- use $ rec . fileName
    case fn of
        Just (Right (fn, lock)) -> do
            liftIO $ release lock
        _ -> do
            return ()

------------------------------------------------------------------------------

{-
nbft :: Graph gr => Node -> gr a b -> [[(Node, a)]]
nbft v g = case lab v g of
             Nothing -> []
             Just l  -> nbf (queuePut [(v, l)] mkQueue) g

nbf :: Graph gr => Queue [(Node, a)] -> gr a b -> [[(Node, a)]]
nbf q g | queueEmpty q || isEmpty g = []
        | otherwise =
            let (p, q') = queueGet q
            in case match (head p) g of

nbf :: Graph gr => Queue [Node] -> gr a b -> [[(Node, a)]]
nbf q g | queueEmpty q || isEmpty g = []
        | otherwise =
             case match v g of
               (Just c, g')  -> p : nbf (queuePutList (map (: p) (suc' c)) q') g'
               (Nothing, g') -> nbf q' g'
               where (p@(v:_),q') = queueGet q
-}

{-
roomArea :: Int -> Int -> MGMap -> Maybe String
roomArea depth r m =
  let paths = bft r m
      lookupArea x = userValueToString $ lookupUserValue "area" $ m ^. mapRoomData x
      firstArea paths = getFirst $ mconcat $ map (First . lookupArea . head) $ take depth paths
  in firstArea paths
-}

------------------------------------------------------------------------------

prewalk = yieldSend "ultrakurz"
postwalk = yieldSend "lang" >> yieldSend "schau"

walk stepper path = prewalk >> walker stepper path >> postwalk

modeStepper n = do
  mode <- lift $ use $ rec . walkMode
  case mode of
    WalkFast -> fastStepper n
    WalkSafe -> safeStepper n
    WalkAggro -> aggroStepper n

fastStepper n = return WalkerContinue

safeStepper n = do
    isSafe <- fmap getAny $ lift $ uses (rec . effectiveMap . mapRoomData n . roomValue . mgRoomSafe) Any
    if isSafe
       then parse (gmcp n) -- return WalkerContinue
       else parse blocker `chainIteration` parse (gmcp n)
  where
    blocker = do
      name <- fetchBlocker
      return WalkerStop
    gmcp n = do
      fetchSignal >>= guard . (== "room-enter")
      cur <- lift $ use $ rec . currentRoom

      if cur /= n
         then do lift $ echoMapper "Room number mismatch. Cancelling walk."
                 return WalkerStop
         else do return WalkerContinue

aggroStepper n = do
    isSafe <- fmap getAny $ lift $ uses (rec . effectiveMap . mapRoomData n . roomValue . mgRoomSafe) Any
    if isSafe
       then return WalkerContinue
       else blockerTot `chainIteration` blocker `chainIteration` parse (gmcp n)
  where
    blocker = do
      name <- parse' fetchBlocker
      yieldFeedback $ mkEv $ CommandEvent ("autofight", [name])
      return WalkerPause
    blockerTot = do
        parse' Combat.fetchDeath
        return WalkerRetry
    gmcp n = do
      fetchSignal >>= guard . (== "room-enter")
      cur <- lift $ use $ rec . currentRoom

      if cur /= n
        then do
          lift $ echoMapper "Room number mismatch. Cancelling walk."
          return WalkerStop
        else
          return WalkerContinue

{-
walk path = prewalk >> walker walkerTrigger path >> postwalk
  where
    walkerTrigger n = join $ parseU $ gmcp n <|> blocker
    blocker = fetchBlocker --> return WalkerStop
    {-
    gmcp n = fetchGMCPModule "MG.room.info" ==> \gmcp -> do
      m   <- lift $ use $ base . mapStore . effectiveMap
      cur <- lift $ use $ base . currentRoom

      let curhash = lookupUserValue "hash" $ m ^. mapRoomData cur
          newhash = fromMaybe "" $ getStringField "id" gmcp

      lift $ echo $ toAS $ "[pre]  r / ex / curhash / newhash : " ++ show cur ++ " / " ++ show n ++ " / " ++ show curhash ++ " / " ++ show newhash
      yield $ mkEv $ GMCPEvent gmcp
      lift $ echo $ toAS $ "[post] r / ex / curhash / newhash : " ++ show cur ++ " / " ++ show n ++ " / " ++ show curhash ++ " / " ++ show newhash

      if cur /= n
        then do
          lift $ echoMapper "Room number mismatch. Cancelling walk."
          return WalkerStop
        else do
          if (newhash /= "" && not (isEmptyHash curhash) && not (containsHash newhash curhash))
            then if not (newhash `elem` ["95d7dff424c9a9585c4e62dff396ec36"])
              then do
                lift $ echoMapper "Room ID mismatch. Cancelling walk."
                return WalkerStop
              else return WalkerContinue
            else return WalkerContinue
    -}
    gmcp n = (fetchSignal >>= guard . (== "room-enter")) --> do
      cur <- lift $ use $ base . currentRoom

      if cur /= n
        then do
          lift $ echoMapper "Room number mismatch. Cancelling walk."
          return WalkerStop
        else
          return WalkerContinue
-}

withPathToRoom r f = do
  cur <- lift $ use $ rec . currentRoom
  path <- lift $ findPath cur r
  case path of
    Nothing   -> lift $ echoMapper "Path not found"
    Just path -> f path

{-
walk' dest = do
    cur <- lift $ use $ base . currentRoom
    path <- lift $ findPath cur dest
    case path of
        Nothing   -> lift $ echoMapper "Path not found"
        Just path -> do
            lift $ base . contRoom .= dest

            yieldSend "ultrakurz"
            mgWalker path
            yieldSend "lang"
            yieldSend "schau"
-}

{-
walkFast dest = withPathToRoom dest $ \path -> prewalk >> fastWalker path >> postwalk
  where
    fastWalker [] = return ()
    fastWalker ((x,n):xs) = do
      yieldSend x
      fastWalker xs
-}

walkTo stepper dest = withPathToRoom dest $ \path -> do
  cur <- lift $ use $ rec . currentRoom
  lift $ rec . walkStack %= Z.left . Z.cons cur . Z.discard
  walk stepper path

walkUndo stepper = do
  cur <- lift $ use $ rec . currentRoom
  ws <- lift $ use $ rec . walkStack
  case Z.rightFocus ws of
    Nothing -> lift $ echoMapper "Already at oldest room"
    Just x -> withPathToRoom x $ \path -> do
      lift $ rec . walkStack %= Z.insertLeft cur . Z.deleteRight
      walk stepper path

walkRedo stepper = do
  cur <- lift $ use $ rec . currentRoom
  ws <- lift $ use $ rec . walkStack
  case Z.leftFocus ws of
    Nothing -> lift $ echoMapper "Already at newest room"
    Just x -> withPathToRoom x $ \path -> do
      lift $ rec . walkStack %= Z.insertRight cur . Z.deleteLeft
      walk stepper path

------------------------------------------------------------------------------

findPath :: (MonadState (Fix r) m, R :@: r) => Int -> Int -> m (Maybe [(String, Int)])
findPath src dest = do
    m <- use $ rec . effectiveMap
    let weightfun edge = edge ^. exitValue . mgExitWeight
    case mapShortestPath weightfun src dest m of
        [] -> return Nothing
        p  -> return $ Just p

findPathFromCurrent :: (MonadState (Fix r) m, R :@: r) => Int -> m (Maybe [(String, Int)])
findPathFromCurrent r = do
    cur <- use $ rec . currentRoom
    findPath cur r

findRoom :: (MonadState (Fix r) m, MonadFail m, R :@: r) => String -> m (Maybe Int)
findRoom name = do
    case name of
        ('$':hash) -> do
          m <- use $ rec . effectiveMap
          return $ mapFindRoomBy (\x -> hash `elem` x ^. roomValue . mgRoomHash) m
        ('#':n) -> case reads n of
            ((n, _):_) -> return $ Just n
            _ -> fail "Invalid room id"
        tag     -> do
          m <- use $ rec . effectiveMap
          return $ mapFindRoomBy (\x -> Just tag == x ^. roomTag) m

------------------------------------------------------------------------------

lift2 x = lift $ lift x

data RoomTriggerState = RoomTriggerState
  { rtsLastline :: String
  , rtsDidMove :: Bool
  }

initRoomTriggerState = RoomTriggerState
  { rtsLastline = ""
  , rtsDidMove = False
  }

roomTrigger :: (Screen s, R :@: r, MGEvent a) => Trigger (Ev a) (MB (Fix r) s) ()
roomTrigger = stateful initRoomTriggerState $ permanent $
  await >>= \x -> void $ runMaybeT $ msum [ onSend x, onGMCP x, lift (yield x) ]
  where
    onSend ev = do
      s <- guardSend ev
      lift $ do
        -- Speicher das fuer das naechste GMCP event.
        lift $ modify' $ \x -> x { rtsLastline = s, rtsDidMove = False }

        mmode <- lift2 $ use $ rec . mode
        effm  <- lift2 $ use $ rec . effectiveMap
        cur   <- lift2 $ use $ rec . currentRoom

        let next = mapFindAdjacentRooms cur s effm      -- Raum, zu dem Ausgang s fuehrt.
            opp  = lookup s standardExits               -- Evtl. die Gegenrichtung (falls Standardausgang)

        case (mmode, next, opp) of
          -- Mapper ist aus. Tu nichts.
          (ModeOff, _, _) -> yieldSend s
          -- Mappermodus "manual", eingegebene Zeile ist ein Standardausgang, angegebene
          -- Richtung existiert noch nicht. Baue neuen Ausgang und bewege in den neuen Raum.
          (ModeManual, [], Just opp) -> do
            lift2 $ addRoomAndMoveWith $ \r ->
              mapAddExit r opp cur "base" initMGExitData . mapAddExit cur s r "base" initMGExitData
            yieldSend s
          -- Ausgang existiert schon. before-exit-Aktionen ausfuehren und auf Blocker checken.
          (_, (next:othernext), _) -> do
            let beforeExit = roomActionsBeforeExit effm cur s

            when (not (null othernext)) $ do
              lift2 $ echoMapper $ "Ausgang '" ++ show s ++ "' nicht eindeutig. Deaktiviere Automapper."
              lift2 $ rec . mode .= ModeFixed

            wegFrei <- roomCheckBlockers effm cur s
            case wegFrei of
              [] -> do
                -- Weg ist frei
                lift2 $ rec . currentRoom .= next
                mapM_ yield beforeExit
                lift $ modify' $ \s -> s { rtsDidMove = True }
                yieldSend s
              blockers -> do
                -- Blocker im Weg
                lift2 $ echoMapper $ "BLOCKER: " ++ (show blockers)
                mapM_ feedbackBlocker blockers
          -- Ansonsten, tu nix
          _ -> yieldSend s

    onGMCP ev = do
      gmcp <- guardGMCPModule "MG.room.info" ev
      lift $ do
        pushback $ mkEv $ GMCPEvent gmcp

        rts <- lift $ get
        let lastline = rtsLastline rts
        let didMove = rtsDidMove rts

        mmode   <- lift2 $ use $ rec . mode
        effm    <- lift2 $ use $ rec . effectiveMap
        cur     <- lift2 $ use $ rec . currentRoom

        let curhashes = effm ^. mapRoomData cur . roomValue . mgRoomHash    -- Bekannte Hashes des aktuellen Raums
            newhash = getStringField "id" gmcp                              -- Empfangener Hash
            newhash' = fromMaybe "" newhash
            newroom = mapFindRoomBy (\x -> newhash' `elem` x ^. roomValue . mgRoomHash) effm

        lift2 $ rec . roomHash .= newhash

        -- Im "update" modus holen wir uns immer die aktuellen Kurzbeschreibungen
        -- und Regionen.
        when (mmode == ModeUpdate) $ do
            lift2 $ rec . currentRoomData . roomValue . mgRoomShort .= getStringField "short" gmcp
            lift2 $ rec . currentRoomData . roomValue . mgRoomDomain .= getStringField "domain" gmcp

        if not (newhash' `elem` curhashes)
          then case (mmode, newroom) of
            -- Mapper ist aus. Tu nichts.
            (ModeOff, _) -> return ()
            -- Mappermode "auto", Hash ist nicht bekannt.
            (ModeAuto, Nothing) -> do
              if didMove
                -- Ausgang existiert bereits, aber die neue Raum-ID ist nicht bekannt. Vermutlich
                -- Parallelwelt. Fuege dem Raum die neue ID hinzu.
                -- FIXME: Wenn dem client bekannt ist, dass wir in einer Parallelwelt sind, neuen Ausgang
                -- mit layer "pN" erzeugen, sonst nur eine Meldung geben und sonst wie ModeFixed
                then lift2 $ rec . baseMap . mapRoomData cur . roomValue . mgRoomHash %= (++ [newhash'])
                -- Ausgang existiert noch nicht. Erstelle neuen Raum und gehe da hin.
                else lift2 $ addRoomAndMoveWith $ \r ->
                    (mapRoomData r . roomValue . mgRoomHash .~ maybeToList newhash)
                  . (mapRoomData r . roomValue . mgRoomShort .~ getStringField "short" gmcp)
                  . (mapRoomData r . roomValue . mgRoomDomain .~ getStringField "domain" gmcp)
                  . (mapAddExit cur lastline r "base" initMGExitData)
              signal "room-enter"
            -- Mappermode "auto", Hash ist bekannt. Baue ggf. den entsprechenden Ausgang und
            -- bewege in den entsprechenden Raum.
            (ModeAuto, Just r) -> do
              case mapFindAdjacentRoom cur lastline effm of
                Nothing -> lift2 $ rec . baseMap %= mapAddExit cur lastline r "base" initMGExitData
                Just _  -> return ()
              lift2 $ rec . currentRoom .= r
              signal "room-enter"
            -- Mappermode "update", Hash ist nicht bekannt. Weise dem Raum einen neuen Hash zu.
            -- Ein Raum kann mehrere Hashes haben.
            (ModeUpdate, Nothing) -> do
              case newhash of
                Nothing -> return ()
                Just newhash' -> lift2 $ rec . currentRoomData . roomValue . mgRoomHash %= ((:) newhash')
            -- Mappermode "update", Hash ist bekannt. Fehlermeldung.
            (ModeUpdate, Just r) -> do
              lift2 $ echoMapper $ "Hash " ++ newhash' ++ " ist bereits belegt."
            -- Statischer modus, Hash ist unbekannt.
            (_, Nothing) -> do
              lift2 $ echoMapper $ "Raum " ++ newhash' ++ " nicht gefunden"
            -- Statischer modus, Hash ist bekannt. Geh dahin.
            (_, Just r) -> do
              lift2 $ rec . currentRoom .= r
              signal "room-enter"
          else do
            signal "room-enter"

        lift $ modify' $ \x -> x { rtsDidMove = False }

    {-
    addHash h x = case x of
      UserValueString v -> UserValueArray [UserValueString h, UserValueString v]
      UserValueArray v -> UserValueArray (UserValueString h : v)
      _ -> UserValueString h
    maybeInsertString s g = case getStringField s g of
      Nothing -> id
      Just x -> M.insert s (UserValueString x)
    -}
    addRoomAndMoveWith f = do
      basem <- use $ rec . baseMap
      case mapAddRoom initMGRoomData basem of
        Nothing -> echoMapper "Konnte Raum nicht erstellen."
        Just (newm, newroom) -> do
          rec . currentRoom .= newroom
          rec . baseMap .= f newroom newm

-----------------------------------------------------------------------------

-- Dummy for now. Later we could add triggers to notify the user that he has found a special room
specialRoomC :: (Monad m, R :@: r) => String -> MBComponent m e (Fix r) (Fix r) -> MBComponent m e (Fix r) (Fix r)
specialRoomC h c = c

paraRoomC = specialRoomC "a4741e724fb9c425327d9df84dd314e6" $ triggerC 50 $ permanent $ do
    pnum <- parse $ do
        x <- fetch
        inRoom "a4741e724fb9c425327d9df84dd314e6"
        r <- guardSend x >>= regex1 "^para ([[:digit:]])$"
        guard $ r `elem` ["0", "1", "2", "3", "4", "5", "6", "7"]
        return r
    lift $ rec . overlay .= if pnum == "0" then ["base"] else ["base", "p" ++ pnum]
    yieldSend $ "betrete portal\n" ++ pnum
