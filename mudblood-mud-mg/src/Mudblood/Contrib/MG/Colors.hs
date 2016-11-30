module Mudblood.Contrib.MG.Colors
  ( colorizer
  ) where

import Mudblood
import Mudblood.Contrib.MG.Settings

colorizeSetting setting re = permanent $ do
  l <- parseU $ fetchLineRegex re
  liftI $ yieldLineAs setting $ fromAS l

colorizer = chain $ map (\(a,b) -> colorizeSetting a b)
  [ ("general.inform.leave", "hat gerade das MorgenGrauen verlassen\\.$")
  , ("general.inform.join", "ist gerade ins MorgenGrauen gekommen\\.$")

  , ("general.error.wiebitte", "^Wie bitte\\?$")
  , ("general.error.nodetail", "^Sowas siehst Du da nicht!$")

  , ("general.clock", "^Du hoerst die Uhr schlagen: Es ist jetzt")

  , ("actions.knuddeln", "knuddelt Dich\\.$")
  , ("actions.knuddeln.remote", "knuddelt Dich aus der Ferne\\.$")
  , ("actions.wuscheln", "verwuschelt Dein (Haar|Fell)\\.$")
  , ("actions.wuscheln.remote", "verwuschelt Dein (Haar|Fell) aus der Ferne\\.$")
  , ("actions.winken", "winkt Dir zu\\.$")
  , ("actions.winken.remote", "winkt Dir aus der Ferne zu\\.$")

  , ("items.nex", "^Dein Guertel waermt sich kurz auf und heilt einige deiner Wunden\\.$")
  , ("items.adaschuhe", "^Deine Schuhe gluehen kurz auf und heilen Dich ein wenig\\.$")

  , ("combat.die", "faellt tot zu Boden\\.$")
  , ("combat.prey", "jagt Dich nicht mehr\\.$")
  , ("combat.hunter", "Du jagst (.+) nicht mehr\\.$")
  ]
