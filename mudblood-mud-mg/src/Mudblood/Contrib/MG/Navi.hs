module Mudblood.Contrib.MG.Navi
  ( component
  ) where

import Mudblood

component =
      triggerC 5 introduction

msg ls = do
    echo $ setFg Green $ toAS "Navi teilt Dir mit:"
    mapM_ (echo . setFg Green . toAS . ("  " ++)) ls

introduction = oneshot $ do
    parse' fetchEOR
    wait 3
    lift $ msg ["Huhu, ich bin Navi und ich kann Dir helfen, mit deinem Client zurechtzukommen."]
