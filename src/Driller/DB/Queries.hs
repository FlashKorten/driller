{-# LANGUAGE OverloadedStrings #-}
module Driller.DB.Queries
    ( JoinMap
    , scenarioListQuery
    , groupQuery
    , initJoinMap
    , initGroupMap
    , initQueryMap
    ) where

import Driller.Data
    ( QueryMap
    , QueryType(..)
    , QueryTarget(COUNT, ENTRY, GROUP)
    , QueryCategory(..)
    , ComponentMap
    , Config(..)
    , JoinMapComponents
    , JoinMap
    , GroupMap
    , GroupMapComponents
    , Parameter
    , categoryToQuery
    , ParameterValue(..)
    )
import Database.PostgreSQL.Simple ( Query )
import Data.Monoid ( mappend, mconcat )
import Data.List ( foldl' )
import Data.Text ( Text )
import qualified Data.DList as DL ( toList, fromList, append, concat )
import Data.HashMap.Strict ( fromList, (!) )

groupQuery :: Config -> Text -> [Parameter] -> Query
groupQuery config group pList =
    mconcat $ DL.toList $ DL.concat [ DL.fromList [selectPart]
                                    , DL.fromList joinParts
                                    , DL.fromList [wherePart]
                                    , DL.fromList whereParts
                                    , DL.fromList [orderPart]
                                    ]
                   where (selectPart, wherePart, orderPart) = groupMap ! group
                         (joinParts, whereParts) = unzip $ map (getParameterQuery joinMap) $ drop 1 pList
                         joinMap = getJoinMap config
                         groupMap = getGroupMap config

scenarioListQuery :: Config -> [Parameter] -> Query
scenarioListQuery config pList = foldl' mappend prefix parts
             where prefix = "SELECT distinct(s.id) FROM dr_scenario AS s"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList
                   joinMap = getJoinMap config

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, Number value) = (j, w)
                                              where (j, w1, w2) = (joinMap !) key
                                                    w = if value >= 0 then w1 else w2
getParameterQuery _ (a, b) = error $ "This will not have happened: " ++ show a ++ " - " ++ show b ++ "!!!"

categoriesMappedToGame :: [QueryCategory]
categoriesMappedToGame = [GENRE, ENGINE, THEME, MECHANIC, PUBLISHER, SERIES]

categoriesMappedToScenario :: [QueryCategory]
categoriesMappedToScenario = [AUTHOR, SIDE, PARTY, LEADER]

mappedCategories :: [QueryCategory]
mappedCategories = categoriesMappedToGame ++ categoriesMappedToScenario

numberCategories :: [QueryCategory]
numberCategories = [LATITUDE, LONGITUDE, FROM_YEAR, UPTO_YEAR, RANGE, TIMESCALE, FROM_RANGE, UPTO_RANGE, FROM_TIMESCALE, UPTO_TIMESCALE]

initQueryMap :: QueryMap
initQueryMap = fromList
  $ [((category, ENTRY, OMNI), omniEntryFromMap         $ categoryToQuery category) | category <- mappedCategories]
 ++ [((category, ENTRY, OMNI), omniEntryFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, ENTRY, MONO), monoEntryFromMap         $ categoryToQuery category) | category <- mappedCategories]
 ++ [((category, ENTRY, MONO), monoEntryForExactValues  $ categoryToQuery category) | category <- [LATITUDE, LONGITUDE]]
 ++ [((category, ENTRY, MONO), monoEntryForMinValues    $ categoryToQuery category) | category <- [FROM_YEAR, FROM_RANGE, FROM_TIMESCALE]]
 ++ [((category, ENTRY, MONO), monoEntryForMaxValues    $ categoryToQuery category) | category <- [UPTO_YEAR, UPTO_RANGE, UPTO_TIMESCALE]]
 ++ [((category, ENTRY, POLYA), polyAEntryFromGameMap     $ categoryToQuery category) | category <- categoriesMappedToGame]
 ++ [((category, ENTRY, POLYA), polyAEntryFromScenarioMap $ categoryToQuery category) | category <- categoriesMappedToScenario]
 ++ [((category, ENTRY, POLY), polyEntryFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, ENTRY, POLYB), polyBEntryFromGameMap     $ categoryToQuery category) | category <- categoriesMappedToGame]
 ++ [((category, ENTRY, POLYB), polyBEntryFromScenarioMap $ categoryToQuery category) | category <- categoriesMappedToScenario]
 ++ [((category, ENTRY, POLY), polyEntryFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, GROUP, OMNI), omniGroupFromMap         $ categoryToQuery category) | category <- mappedCategories]
 ++ [((category, GROUP, OMNI), omniGroupFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, GROUP, POLY), polyGroupFromGameMap     $ categoryToQuery category) | category <- categoriesMappedToGame]
 ++ [((category, GROUP, POLY), polyGroupFromScenarioMap $ categoryToQuery category) | category <- categoriesMappedToScenario]
 ++ [((category, GROUP, POLY), polyGroupFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, GROUP, MONO), monoGroupFromMap         $ categoryToQuery category) | category <- mappedCategories]
 ++ [((category, GROUP, MONO), monoGroupFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((category, COUNT, POLY), polyCountFromGameMap     $ categoryToQuery category) | category <- categoriesMappedToGame]
 ++ [((category, COUNT, POLY), polyCountFromScenarioMap $ categoryToQuery category) | category <- categoriesMappedToScenario]
 ++ [((category, COUNT, POLY), polyCountFromValues      $ categoryToQuery category) | category <- numberCategories]
 ++ [((GAME, ENTRY, POLYA),    polyAEntryForGame)]
 ++ [((GAME, ENTRY, POLYB),    polyBEntryForGame)]
 ++ [((GAME, GROUP, OMNI),     omniGroupForGame)]
 ++ [((GAME, GROUP, POLY),     polyGroupForGame)]
 ++ [((GAME, GROUP, MONO),     monoGroupForGame)]
 ++ [((GAME, COUNT, POLY),     polyCountForGame)]
 ++ [((GAME, ENTRY, OMNI),     omniEntryForGame)]
 ++ [((GAME, ENTRY, MONO),     monoEntryForGame)]
 ++ [((SCENARIO, ENTRY, OMNI), omniEntryForScenario)]
 ++ [((SCENARIO, ENTRY, MONO), monoEntryForScenario)]
 ++ [((SCENARIO, ENTRY, POLY), polyEntryForScenario)]

omniEntryForGame :: Query
omniEntryForGame = mappend
  selectForMonoOrOmniGameEntry " ORDER BY title, subtitle"

omniEntryForScenario :: Query
omniEntryForScenario = mappend
  selectForScenarioEntry " ORDER BY sd.title, sd.subtitle"

omniEntryFromMap :: Query -> Query
omniEntryFromMap q = mconcat
  [ "SELECT id, ", q
  , " FROM dr_", q
  , " ORDER BY ", q
  ]

omniEntryFromValues :: Query -> Query
omniEntryFromValues q = mconcat
  [ "SELECT ", q
  , " FROM dr_scenario"
  , " GROUP BY ", q
  , " ORDER BY ", q
  ]

omniGroupForGame :: Query
omniGroupForGame = mconcat
  [ "SELECT grp, count(id)"
  , " FROM dr_game"
  , " GROUP BY grp"
  , " ORDER BY grp"
  ]

omniGroupFromMap :: Query -> Query
omniGroupFromMap q = mconcat
  [ "SELECT grp, count(id)"
  , " FROM dr_", q
  , " GROUP BY grp"
  , " ORDER BY grp"
  ]

omniGroupFromValues :: Query -> Query
omniGroupFromValues q = mconcat
  [ "SELECT ", q, "_group, count(distinct(", q, "))"
  , " FROM dr_scenario"
  , " GROUP BY ", q, "_group"
  , " ORDER BY ", q, "_group"
  ]

polyEntryForScenario :: Query
polyEntryForScenario = mappend
  selectForScenarioEntry " WHERE s.id IN ?"

polyAEntryForGame :: Query
polyAEntryForGame = mconcat
  [ "SELECT g.id, g.title, g.subtitle"
  , " FROM dr_game AS g"
  , " JOIN dr_scenario AS s ON s.id_game = g.id"
  , " WHERE s.id IN ?"
  , " GROUP BY g.id, g.title, g.subtitle"
  , " HAVING count(g.id) < ?"
  , " ORDER BY g.title, g.subtitle"
  ]

polyBEntryForGame :: Query
polyBEntryForGame = mconcat
  [ "SELECT g.id, g.title, g.subtitle"
  , " FROM dr_game AS g"
  , " JOIN dr_scenario AS s ON s.id_game = g.id"
  , " WHERE s.id IN ?"
  , " GROUP BY g.id, g.title, g.subtitle"
  , " HAVING count(g.id) = ?"
  , " ORDER BY g.title, g.subtitle"
  ]

polyAEntryFromGameMap :: Query -> Query
polyAEntryFromGameMap q = mconcat
  [ "SELECT a.id, a.", q
  , " FROM dr_", q, " AS a"
  , " JOIN dr_map_", q, " AS ma ON a.id = ma.id_", q
  , " JOIN dr_scenario AS s ON s.id_game = ma.id_game"
  , " WHERE s.id IN ?"
  , " GROUP BY a.id, a.", q
  , " HAVING count(a.id) < ?"
  , " ORDER BY a.", q
  ]

polyBEntryFromGameMap :: Query -> Query
polyBEntryFromGameMap q = mconcat
  [ "SELECT a.id, a.", q
  , " FROM dr_", q, " AS a"
  , " JOIN dr_map_", q, " AS ma ON a.id = ma.id_", q
  , " JOIN dr_scenario AS s ON s.id_game = ma.id_game"
  , " WHERE s.id IN ?"
  , " GROUP BY a.id, a.", q
  , " HAVING count(a.id) = ?"
  , " ORDER BY a.", q
  ]

polyAEntryFromScenarioMap :: Query -> Query
polyAEntryFromScenarioMap q = mconcat
  [ "SELECT a.id, a.", q
  , " FROM dr_", q, " AS a"
  , " JOIN dr_map_", q, " AS ma ON a.id = ma.id_", q
  , " WHERE ma.id_scenario IN ?"
  , " GROUP BY a.id, a.", q
  , " HAVING count(a.id) < ?"
  , " ORDER BY a.", q
  ]

polyBEntryFromScenarioMap :: Query -> Query
polyBEntryFromScenarioMap q = mconcat
  [ "SELECT a.id, a.", q
  , " FROM dr_", q, " AS a"
  , " JOIN dr_map_", q, " AS ma ON a.id = ma.id_", q
  , " WHERE ma.id_scenario IN ?"
  , " GROUP BY a.id, a.", q
  , " HAVING count(a.id) = ?"
  , " ORDER BY a.", q
  ]

polyEntryFromValues :: Query -> Query
polyEntryFromValues q = mconcat
  [ "SELECT ", q
  , " FROM dr_scenario"
  , " WHERE id IN ?"
  , " GROUP BY ", q
  , " ORDER BY ", q
  ]

polyGroupForGame :: Query
polyGroupForGame = mconcat
  [ "SELECT a.grp, count(distinct(a.id))"
  , " FROM dr_game AS a"
  , " JOIN dr_scenario AS s ON a.id = s.id_game"
  , " WHERE s.id IN ?"
  , " GROUP BY grp"
  , " HAVING count(a.id) < ?"
  , " ORDER BY grp"
  ]

polyGroupFromGameMap :: Query -> Query
polyGroupFromGameMap = polyGroupFromMap "s.id_game = ma.id_game"

polyGroupFromScenarioMap :: Query -> Query
polyGroupFromScenarioMap = polyGroupFromMap "s.id = ma.id_scenario"

polyGroupFromMap :: Query -> Query -> Query
polyGroupFromMap mapping q = mconcat
  [ "SELECT a.grp, count(distinct(a.id))"
  , " FROM dr_", q, " AS a"
  , " JOIN dr_map_", q, " AS ma on a.id = ma.id_", q
  , " JOIN dr_scenario AS s ON ", mapping
  , " WHERE s.id IN ?"
  , " GROUP BY grp"
  , " HAVING count(a.id) < ?"
  , " ORDER BY grp"
  ]

polyGroupFromValues :: Query -> Query
polyGroupFromValues q = mconcat
  [ "SELECT ", q, "_group, count(distinct(", q, "))"
  , " FROM dr_scenario"
  , " WHERE id IN ?"
  , " GROUP BY ", q, "_group"
  , " ORDER BY ", q, "_group"
  ]

polyCountForGame :: Query
polyCountForGame = mconcat
  [ "SELECT count(distinct(id_game))"
  , " FROM dr_scenario"
  , " WHERE id IN ?"
  ]

polyCountFromGameMap :: Query -> Query
polyCountFromGameMap = polyCountFromMap "s.id_game = a.id_game"

polyCountFromScenarioMap :: Query -> Query
polyCountFromScenarioMap = polyCountFromMap "s.id = a.id_scenario"

polyCountFromMap :: Query -> Query -> Query
polyCountFromMap mapping q = mconcat
  [ "SELECT count(distinct(id_", q, "))"
  , " FROM dr_map_", q, " AS a"
  , " JOIN dr_scenario AS s ON ", mapping
  , " WHERE s.id IN ?"
  ]

polyCountFromValues :: Query -> Query
polyCountFromValues q = mconcat
  [ "SELECT count(distinct(", q, "))"
  , " FROM dr_scenario"
  , " WHERE id IN ?"
  ]

monoEntryForGame :: Query
monoEntryForGame = mappend
  selectForMonoOrOmniGameEntry " WHERE id = ?"

monoEntryForScenario :: Query
monoEntryForScenario = mappend
  selectForScenarioEntry " WHERE s.id = ?"

monoEntryFromMap :: Query -> Query
monoEntryFromMap q = mconcat
  [ "SELECT id, ", q
  , " FROM dr_", q
  , " WHERE id = ?"
  ]

monoEntryForMinValues :: Query -> Query
monoEntryForMinValues q = mconcat
  [ "SELECT min(", q, ")"
  , " FROM dr_scenario"
  , " WHERE ", q, " >= ?"
  ]

monoEntryForMaxValues :: Query -> Query
monoEntryForMaxValues q = mconcat
  [ "SELECT max(", q, ")"
  , " FROM dr_scenario"
  , " WHERE ", q, " <= ?"
  ]

monoEntryForExactValues :: Query -> Query
monoEntryForExactValues q = mconcat
  [ "SELECT ", q
  , " FROM dr_scenario"
  , " WHERE ", q, " = ?"
  ]

monoGroupForGame :: Query
monoGroupForGame = mconcat
  [ "SELECT id, title, subtitle"
  , " FROM dr_game"
  , " WHERE grp = ?"
  , " ORDER BY title"
  ]

monoGroupFromMap :: Query -> Query
monoGroupFromMap q = mconcat
  [ "SELECT id, ", q
  , " FROM dr_", q
  , " WHERE grp = ?"
  , " ORDER BY ", q
  ]

monoGroupFromValues :: Query -> Query
monoGroupFromValues q = mconcat
  [ "SELECT ", q
  , " FROM dr_scenario"
  , " WHERE ", q, "_group = ?"
  , " GROUP BY ", q
  , " ORDER BY ", q
  ]

selectForScenarioEntry :: Query
selectForScenarioEntry = mconcat
  [ "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto"
  , " FROM dr_scenario AS s"
  , " JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario"
  ]

selectForMonoOrOmniGameEntry :: Query
selectForMonoOrOmniGameEntry = mconcat
  [ "SELECT id, title, subtitle"
  , " FROM dr_game"
  ]

initGroupMap :: GroupMap
initGroupMap = fromList $ prepareGroupList parameterList selectList whereGroupList orderGroupList

prepareGroupList :: [Text] -> ComponentMap -> ComponentMap -> ComponentMap -> [(Text, GroupMapComponents)]
prepareGroupList (p:ps) sMap wMap oMap = (p, (sMap ! p, wMap ! p, oMap ! p)) : prepareGroupList ps sMap wMap oMap
prepareGroupList _ _ _ _               = []

initJoinMap :: JoinMap
initJoinMap = fromList $ prepareJoinList parameterList joinList whereIncludeList whereExcludeList

prepareJoinList :: [Text] -> ComponentMap -> ComponentMap -> ComponentMap -> [(Text, JoinMapComponents)]
prepareJoinList (p:ps) j w1 w2 = (p, (j ! p, w1 ! p, w2 ! p)) : prepareJoinList ps j w1 w2
prepareJoinList _ _ _ _        = []

parameterList :: [Text]
parameterList =
  [ "author"
  , "publisher"
  , "theme"
  , "genre"
  , "mechanic"
  , "side"
  , "party"
  , "series"
  , "leader"
  , "engine"
  , "game"
  , "latitude"
  , "longitude"
  , "fromYear"
  , "upToYear"
  , "fromRange"
  , "upToRange"
  , "fromTimescale"
  , "upToTimescale"
  ]

joinForMap :: Query -> Query -> Query -> Query
joinForMap mapSource mapTarget q = mconcat
  [" JOIN dr_map_", q
  , " AS ", q
  , " ON s.", mapSource, " = ", q, ".", mapTarget
  ]

joinForMapToGame :: Query -> Query
joinForMapToGame = joinForMap "id_game" "id_game"

joinForMapToScenario :: Query -> Query
joinForMapToScenario = joinForMap "id" "id_scenario"

joinList :: ComponentMap
joinList = fromList
  [ ("author",    joinForMapToScenario "author")
  , ("side",      joinForMapToScenario "side")
  , ("party",     joinForMapToScenario "party")
  , ("leader",    joinForMapToScenario "leader")
  , ("series",    joinForMapToGame "series")
  , ("publisher", joinForMapToGame "publisher")
  , ("theme",     joinForMapToGame "theme")
  , ("genre",     joinForMapToGame "genre")
  , ("mechanic",  joinForMapToGame "mechanic")
  , ("engine",    joinForMapToGame "engine")
  , ("game",      "")
  , ("latitude",  "")
  , ("longitude", "")
  , ("fromYear",  "")
  , ("upToYear",  "")
  , ("fromRange", "")
  , ("upToRange", "")
  , ("fromTimescale", "")
  , ("upToTimescale", "")
  ]

whereIncludeList :: ComponentMap
whereIncludeList = fromList
  [ ("author",    whereIncludeForMap "author")
  , ("publisher", whereIncludeForMap "publisher")
  , ("theme",     whereIncludeForMap "theme")
  , ("genre",     whereIncludeForMap "genre")
  , ("mechanic",  whereIncludeForMap "mechanic")
  , ("side",      whereIncludeForMap "side")
  , ("party",     whereIncludeForMap "party")
  , ("series",    whereIncludeForMap "series")
  , ("leader",    whereIncludeForMap "leader")
  , ("engine",    whereIncludeForMap "engine")
  , ("game",      " AND s.id_game = ?")
  , ("latitude",  whereForLatitude)
  , ("longitude", whereForLongitude)
  , ("fromYear",  whereForFromYear)
  , ("upToYear",  whereForUpToYear)
  , ("fromRange", whereForFromRange)
  , ("upToRange", whereForUpToRange)
  , ("fromTimescale", whereForFromTimescale)
  , ("upToTimescale", whereForUpToTimescale)
  ]

whereIncludeForMap :: Query -> Query
whereIncludeForMap q = mconcat
  [" AND ", q, ".id_", q, " = ?"]

whereExcludeList :: ComponentMap
whereExcludeList = fromList
  [ ("author",    whereExcludeForMapToScenario "author")
  , ("side",      whereExcludeForMapToScenario "side")
  , ("party",     whereExcludeForMapToScenario "party")
  , ("leader",    whereExcludeForMapToScenario "leader")
  , ("publisher", whereExcludeForMapToGame "publisher")
  , ("theme",     whereExcludeForMapToGame "theme")
  , ("genre",     whereExcludeForMapToGame "genre")
  , ("mechanic",  whereExcludeForMapToGame "mechanic")
  , ("series",    whereExcludeForMapToGame "series")
  , ("engine",    whereExcludeForMapToGame "engine")
  , ("game",      " AND s.id_game != (-1 * ?)")
  , ("latitude",  whereForLatitude)
  , ("longitude", whereForLongitude)
  , ("fromYear",  whereForFromYear)
  , ("upToYear",  whereForUpToYear)
  , ("fromRange", whereForFromRange)
  , ("upToRange", whereForUpToRange)
  , ("fromTimescale", whereForFromTimescale)
  , ("upToTimescale", whereForUpToTimescale)
  ]

whereExcludeForMapToGame :: Query -> Query
whereExcludeForMapToGame = whereExcludeForMap "id_game" "id_game"

whereExcludeForMapToScenario :: Query -> Query
whereExcludeForMapToScenario = whereExcludeForMap "id_scenario" "id"

whereExcludeForMap :: Query -> Query -> Query -> Query
whereExcludeForMap what mappedTo q = mconcat
  [ " AND NOT EXISTS (SELECT ", what
  , " FROM dr_map_" ,q
  , " WHERE ", what, " = s.", mappedTo
  , " AND id_", q, " = (-1 * ?))"
  ]

whereForLatitude, whereForLongitude,
  whereForFromYear, whereForUpToYear,
  whereForFromRange, whereForUpToRange,
  whereForFromTimescale, whereForUpToTimescale :: Query
whereForLatitude      = " AND s.latitude_trunc = ?"
whereForLongitude     = " AND s.longitude_trunc = ?"
whereForFromYear      = " AND NOT s.year_upto < ?"
whereForUpToYear      = " AND NOT s.year_from > ?"
whereForFromRange     = " AND s.range >= ?"
whereForUpToRange     = " AND s.range <= ?"
whereForFromTimescale = " AND s.timescale >= ?"
whereForUpToTimescale = " AND s.timescale <= ?"

selectList :: ComponentMap
selectList = fromList
  [ ("author",        selectGroupEntriesForMapToGame "author")
  , ("side",          selectGroupEntriesForMapToGame "side")
  , ("party",         selectGroupEntriesForMapToGame "party")
  , ("leader",        selectGroupEntriesForMapToGame "leader")
  , ("series",        selectGroupEntriesForMapToScenario "series")
  , ("publisher",     selectGroupEntriesForMapToScenario "publisher")
  , ("theme",         selectGroupEntriesForMapToScenario "theme")
  , ("genre",         selectGroupEntriesForMapToScenario "genre")
  , ("mechanic",      selectGroupEntriesForMapToScenario "mechanic")
  , ("engine",        selectGroupEntriesForMapToScenario "engine")
  , ("latitude",      selectGroupEntriesForNumbers "latitude_trunc")
  , ("longitude",     selectGroupEntriesForNumbers "longitude_trunc")
  , ("fromYear",      selectGroupEntriesForNumbers "year_from")
  , ("upToYear",      selectGroupEntriesForNumbers "year_upto")
  , ("fromRange",     selectGroupEntriesForNumbers "range")
  , ("upToRange",     selectGroupEntriesForNumbers "range")
  , ("fromTimescale", selectGroupEntriesForNumbers "timescale")
  , ("upToTimescale", selectGroupEntriesForNumbers "timescale")
  , ("game",          selectGroupEntriesForGameMap)
  ]

selectGroupEntriesForMapToGame, selectGroupEntriesForMapToScenario  :: Query -> Query
selectGroupEntriesForMapToGame     = selectGroupEntriesForMap "id"      "id_scenario"
selectGroupEntriesForMapToScenario = selectGroupEntriesForMap "id_game" "id_game"

selectGroupEntriesForMap :: Query -> Query -> Query -> Query
selectGroupEntriesForMap mappedTo mapId q = mconcat
  [ "SELECT ", q, ".id, ", q, ".", q
  , " FROM dr_scenario s JOIN dr_map_", q, " AS m_", q
  , " ON s.", mappedTo, " = m_" , q, ".",mapId
  , " JOIN dr_", q, " AS ", q
  , " ON m_", q, ".id_", q, " = ", q, ".id"]

selectGroupEntriesForNumbers :: Query -> Query
selectGroupEntriesForNumbers q = mconcat
  [ "SELECT distinct(s.", q, ")"
  , " FROM dr_scenario s"
  ]

selectGroupEntriesForGameMap :: Query
selectGroupEntriesForGameMap = mconcat
  [ "SELECT game.id, game.title, game.subtitle"
  , " FROM dr_scenario s"
  , " JOIN dr_game AS game ON s.id_game = game.id "
  ]

whereGroupEntriesForMap :: Query -> Query
whereGroupEntriesForMap q = mconcat
  [" WHERE ", q, ".grp = ?"]

whereGroupEntriesForNumbers :: Query -> Query
whereGroupEntriesForNumbers q = mconcat
  [" WHERE s.", q, "_group = ?"]

whereGroupList :: ComponentMap
whereGroupList = fromList
  [ ("author",        whereGroupEntriesForMap     "author")
  , ("publisher",     whereGroupEntriesForMap     "publisher")
  , ("theme",         whereGroupEntriesForMap     "theme")
  , ("genre",         whereGroupEntriesForMap     "genre")
  , ("mechanic",      whereGroupEntriesForMap     "mechanic")
  , ("side",          whereGroupEntriesForMap     "side")
  , ("party",         whereGroupEntriesForMap     "party")
  , ("series",        whereGroupEntriesForMap     "series")
  , ("leader",        whereGroupEntriesForMap     "leader")
  , ("game",          whereGroupEntriesForMap     "game")
  , ("engine",        whereGroupEntriesForMap     "engine")
  , ("latitude",      whereGroupEntriesForNumbers "latitude_trunc")
  , ("longitude",     whereGroupEntriesForNumbers "longitude_trunc")
  , ("fromYear",      whereGroupEntriesForNumbers "year_from")
  , ("upToYear",      whereGroupEntriesForNumbers "year_upto")
  , ("fromRange",     whereGroupEntriesForNumbers "range")
  , ("upToRange",     whereGroupEntriesForNumbers "range")
  , ("fromTimescale", whereGroupEntriesForNumbers "timescale")
  , ("upToTimescale", whereGroupEntriesForNumbers "timescale")
  ]

orderGroupList :: ComponentMap
orderGroupList = fromList
  [ ("author",        orderGroupEntriesForMap     "author")
  , ("publisher",     orderGroupEntriesForMap     "publisher")
  , ("theme",         orderGroupEntriesForMap     "theme")
  , ("genre",         orderGroupEntriesForMap     "genre")
  , ("mechanic",      orderGroupEntriesForMap     "mechanic")
  , ("side",          orderGroupEntriesForMap     "side")
  , ("party",         orderGroupEntriesForMap     "party")
  , ("series",        orderGroupEntriesForMap     "series")
  , ("leader",        orderGroupEntriesForMap     "leader")
  , ("engine",        orderGroupEntriesForMap     "engine")
  , ("latitude",      orderGroupEntriesForNumbers "latitude_trunc")
  , ("longitude",     orderGroupEntriesForNumbers "longitude_trunc")
  , ("fromYear",      orderGroupEntriesForNumbers "year_from")
  , ("upToYear",      orderGroupEntriesForNumbers "year_upto")
  , ("fromRange",     orderGroupEntriesForNumbers "range")
  , ("upToRange",     orderGroupEntriesForNumbers "range")
  , ("fromTimescale", orderGroupEntriesForNumbers "timescale")
  , ("upToTimescale", orderGroupEntriesForNumbers "timescale")
  , ("game",          orderGroupEntriesForGameQ)
  ]

orderGroupEntriesForMap :: Query -> Query
orderGroupEntriesForMap q = mconcat
  [ " GROUP BY ", q, ".id, ", q, ".", q
  , " ORDER BY ", q, ".", q
  ]

orderGroupEntriesForNumbers :: Query -> Query
orderGroupEntriesForNumbers q = mconcat
  [" ORDER BY s.", q]

orderGroupEntriesForGameQ :: Query
orderGroupEntriesForGameQ = mconcat
  [ " GROUP BY game.title, game.subtitle, game.id"
  , " ORDER BY game.title, game.subtitle"
  ]
