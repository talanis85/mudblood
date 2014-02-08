module Mudblood.Contrib.MG.Mapper.Hash
    ( updateHashIndex
    , containsHash
    , checkCurrentHash, guardCurrentHash
    ) where

import Mudblood

updateHashIndex :: (MBMonad m u) => m ()
updateHashIndex = let indexer (UserValueString h) = [UserValueString h]
                      indexer (UserValueArray hs) = hs
                      indexer _ = []
                  in modifyMap (mapGenRoomIndex "hash" indexer)

-- | Checks if a uservalue is or contains a given hash
containsHash :: String -> UserValue -> Bool
containsHash hash (UserValueString hash') = hash == hash'
containsHash hash (UserValueArray hashes) = (UserValueString hash) `elem` hashes
containsHash hash _ = False

-- | Checks if the current room contains a given hash
checkCurrentHash :: String -> MBTrigger u Bool
checkCurrentHash hash = do
    m <- getMap
    return $ containsHash hash $ lookupUserValue "hash" $ mapGetRoomData (mapCurrentId m) (mapGraph m)

-- | Fails if the current room does not contain a given hash. Otherwise return its input.
guardCurrentHash :: String -> a -> MBTrigger u a
guardCurrentHash hash = keep1 (const (checkCurrentHash hash) >=> guard)
