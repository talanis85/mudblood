{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Mudblood.Trigger.Regex
    ( (~=), (~~=)
    , regex, regex1, regex2, regex3
    , fetchLineRegex, fetchLineRegex1, fetchLineRegex2, fetchLineRegex3
    , fetchSendRegex, fetchSendRegex1, fetchSendRegex2
    ) where

import Mudblood.Trigger
import Mudblood.Text

import Control.Monad
import Data.Maybe

import qualified Data.Array as Array
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.String as RegexString

import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M

-----------------------------------------------------------------------------

instance Extract AttrString where
    before n s = mapAS (take n) s
    after n s = mapAS (drop n) s
    empty = toAS ""

instance RegexLike RegexString.Regex AttrString where
    matchAll r s = matchAll r (fromAS s)
    matchOnce r s = matchOnce r (fromAS s)
    matchCount r s = matchCount r (fromAS s)
    matchTest r s = matchTest r (fromAS s)
    -- matchAllText r s = matchAllText r (fromAS s)
    -- matchOnceText r s = matchOnceText r (fromAS s)

instance RegexContext RegexString.Regex AttrString [[String]] where
    match r s = let arrs = matchAll r s
                    matches (off, len) = extract (off, len) s
                in map (map fromAS . map matches . Array.elems) arrs
    matchM r s = fail "Not implemented"

-----------------------------------------------------------------------------

regexCache :: IORef (M.Map String RegexString.Regex)
{-# NOINLINE regexCache #-}
regexCache = unsafePerformIO (newIORef M.empty)

makeRegex' r = unsafePerformIO $ do
    m <- readIORef regexCache
    case M.lookup r m of
        Nothing -> do
            let re = makeRegex r :: RegexString.Regex
            modifyIORef regexCache $ M.insert r re
            return re
        Just re -> do
            return re

-----------------------------------------------------------------------------

compileRegex = RegexString.compile defaultCompOpt defaultExecOpt

(~=) :: ( RegexContext RegexString.Regex source1 target )
     => String -> source1 -> target

(~=) r = match (makeRegex' r :: RegexString.Regex)

-----------------------------------------------------------------------------

(~~=) :: ( RegexContext RegexString.Regex source1 [target]
         , MonadPlus m )
      => String -> source1 -> m target

(~~=) r = let re = makeRegex' r :: RegexString.Regex
          in \x -> case match re x of
                [] -> mzero
                (x:_) -> return x

-----------------------------------------------------------------------------

regex re x = do
    if re ~= x then return x else mzero

regex1 re x = do
    (_:(y:_)) <- re ~~= x
    return y

regex2 re x = do
    (_:(y:(z:_))) <- re ~~= x
    return (y, z)

regex3 re x = do
    (_:(y:(z:(u:_)))) <- re ~~= x
    return (y, z, u)

-----------------------------------------------------------------------------

fetchLineRegex re = fetchLine >>= regex re
fetchLineRegex1 re = fetchLine >>= regex1 re
fetchLineRegex2 re = fetchLine >>= regex2 re
fetchLineRegex3 re = fetchLine >>= regex3 re

fetchSendRegex re = fetchSend >>= regex re
fetchSendRegex1 re = fetchSend >>= regex1 re
fetchSendRegex2 re = fetchSend >>= regex2 re
