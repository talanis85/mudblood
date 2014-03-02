module Mudblood.Contrib.Lisp.Builtins
    ( coreBuiltins
    ) where

import Mudblood

import Mudblood.Contrib.Lisp.Core
import Mudblood.Contrib.Lisp.MB

coreBuiltins :: Context (MB u) Value
coreBuiltins = mkContext $
    [ ("quit", Function [] $ liftL quit >> return nil)
    , ("echo", Function ["text"] $ do
        x <- getSymbol "text"
        case x of
            Value (AttrStringValue x) -> liftL $ echoA x
            Value (StringValue x)     -> liftL $ echo x
            _                         -> typeError "string"
        return nil
      )
    , ("connect", Function ["host", "port"] $ do
        h <- getSymbol "host" >>= typeString
        p <- getSymbol "port" >>= typeInt
        liftL $ connect h (show p)
        return nil
      )
    , ("send", Function ["text"] $ do
        d <- getSymbol "text" >>= typeString
        liftL $ send d
        return nil
      )
    , ("bind", Function ["keystring", "action"] $ do
        keystring <- getSymbol "keystring" >>= typeString
        fun <- getSymbol "action" >>= typeString
        case parseKeys keystring of
            Nothing -> throwError "Invalid keystring"
            Just k  -> liftL $ ui $ UIBind k $ command fun
        return nil
      )
    ]
