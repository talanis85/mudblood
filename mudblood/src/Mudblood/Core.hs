module Mudblood.Core
    ( telnetToGMCP
    , gmcpHello
    ) where

import qualified Codec.Binary.UTF8.String as UTF8

import           Mudblood.Monad
import           Mudblood.Text
import           Mudblood.Telnet ( TelnetNeg (..), TelnetCommand (..)
                                 , Communication (..), TelnetOption (..)
                                 )
import           Mudblood.Error
import           Mudblood.Encoding

import           Data.Maybe
import           Data.GMCP

--------------------------------------------------------------------------------------------------

telnetToGMCP :: TelnetNeg -> Maybe GMCP
telnetToGMCP t = case t of
  TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat -> parseGMCP $ UTF8.decode dat
  _ -> Nothing

--------------------------------------------------------------------------------------------------

-- | Send a standard GMCP hello.
gmcpHello :: [String]           -- ^ A list of supported GMCP modules
          -> [Communication]
gmcpHello supports =
    [ Communication $ TelnetNeg (Just CMD_DO) (Just OPT_GMCP) []
    , Communication $ gmcpCoreHello "mudblood" "0.1"
    , Communication $ gmcpCoreSupportsSet supports
    ]
