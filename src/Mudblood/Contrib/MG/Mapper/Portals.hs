module Mudblood.Contrib.MG.Mapper.Portals
    ( mapAddPortals
    ) where

import Data.Maybe

import Mudblood

portals =
    [ (1, ("tamibar", "bf586f14b202c43ea8727aefe7d5ae8a"))
    , (2, ("drachenzinnen", "a8760b707dbcf11d85da1c841abad7c9"))
    , (3, ("pv", "7ccced1f8b62fabb7b1494a6d9fd164d"))
    , (4, ("hochebene", "d7fe62577bcf655c97e285096d22c333"))
    , (5, ("polar", "f0d2288ec1a24d86318c8fbf9a221a08"))
--    , (6, ("tundra", "")))
    , (8, ("waldweg", "fbf2d086dd2983fd1f2e02d71941eff6"))
    , (9, ("valgessa", "0bc13858fa88bc03a57714362ea574ad"))
    , (10, ("wueste", "69aeaf5183002e46c31abf400d01c5d1"))
    , (11, ("aurora", "622018029d883479a5b07fb31f830230"))
    , (12, ("svolvaer", "9a45fd1465923334f1ee7a5d6617013f"))
    , (13, ("bergdorf", "aa17651e56c3f4f15a0be1bb30b6dac8"))
    , (14, ("nibelheim", "5429e40b37348ab7f679020a052435c0"))
    , (16, ("dschungel", "b712083865c62f24846239c6f14cb974"))
    , (18, ("fernwest", "24888644005a6a795c998358fa6948ee"))
    , (19, ("vland", "521620dec482f66752aa1ef0ca3cb806"))
    , (20, ("rodelbahn", "a48224c2b328606166f35c0716b03b46"))
    , (21, ("hobbitdorf", "2e844c5316daef31d19ed74861025396"))
    , (22, ("akhar", "a7920efb5367e33a39c689bb0814e9ac"))
    , (23, ("schacht", "1019c848caffd364c9a8f1074ef5a7c4"))
    , (24, ("endederwelt", "3d444f32639c6569fcc53598a56891d8"))
    , (25, ("friedhof", "9e1fad054f198baac665ebd228e7e1e3"))
    , (26, ("shaky", "f538d7b66dc3a114147cb12462a4dafa"))
    , (27, ("kaempfer", "7a58372be3796c191815e56202706937"))
    , (28, ("tortuga", "c28da35dcbcc94bb197f790efe7237b7"))
    , (29, ("katzmandu", "e0f281cbcfc32e9dd049abfac5418ce4"))
    , (30, ("rieseninsel", "b443892bf4661f2ede266eaee84ea776"))
    , (31, ("gebirge", "6577c8f842a9cd0661e27f9a0171367b"))
    , (32, ("portalraum", "297770a7a2af5e85fb461230aa6fdfe4"))
    , (33, ("umjak", "daf72c5edc7464972d4af4bdd83429b3"))
    , (34, ("tanjian", "cb057f19224b8d8d571b359487640800"))
    , (36, ("innuit", "8572f582b7c9fd45ce8e5d7ecbc8df71"))
    , (37, ("werwolfinsel", "c3917d2ca063f4304938051f1e51db86"))
    , (38, ("krylaios", "dbcef72d665565d2e277532d0d57f20b"))
    , (39, ("magieinsel", "eae4cf8f8bbe343c5bfc4f627290bdf6"))
    , (40, ("abgrund", "9ffe18fd06b3413a982ae16191d27b98"))
    ]

mapAddPortals :: [Int] -> Map -> Map
mapAddPortals pset g = foldr addPortal g $ mapMaybe (\x -> fmap (\y -> (x, y)) $ lookup x portals) pset
    where
        addPortal (p, (tag, hash)) g = foldr (addPortal' (p, (tag, hash))) g portals
        addPortal' (p, (tag, hash)) (p', (tag', hash')) g =
            case (findByHash hash g, findByHash hash' g) of
                (Just n, Just n') -> mapModifyGraph (mapAddExit n ("t " ++ show p') n' "base") g
                _ -> g
        findByHash h = mapFindRoomByIndex "hash" (UserValueString h)
