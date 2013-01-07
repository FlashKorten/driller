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
    , QueryType(MONO, OMNI, POLY)
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
             where prefix = "SELECT s.id FROM dr_scenario AS s"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList
                   joinMap = getJoinMap config

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, Number value) = (j, w)
                                              where (j, w1, w2) = (joinMap !) key
                                                    w = if value >= 0 then w1 else w2
getParameterQuery _ (a, b) = error $ "This will not have happened!" ++ show a ++ " - " ++ show b

mapToGameCategories :: [QueryCategory]
mapToGameCategories = [GENRE, ENGINE, THEME, MECHANIC, PUBLISHER, SERIES]

mapToScenariosCategories :: [QueryCategory]
mapToScenariosCategories = [AUTHOR, SIDE, PARTY, LEADER]

mapCategories :: [QueryCategory]
mapCategories = mapToGameCategories ++ mapToScenariosCategories

simpleValueCategories :: [QueryCategory]
simpleValueCategories = [LATITUDE, LONGITUDE, FROM_YEAR, UPTO_YEAR, RANGE, TIMESCALE, FROM_RANGE, UPTO_RANGE, FROM_TIMESCALE, UPTO_TIMESCALE]

initQueryMap :: QueryMap
initQueryMap = fromList $ [((category, ENTRY, OMNI), omniEntryFromMap         $ categoryToQuery category) | category <- mapCategories]
                       ++ [((category, ENTRY, OMNI), omniEntryFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((category, ENTRY, MONO), monoEntryFromMap         $ categoryToQuery category) | category <- mapCategories]
                       ++ [((category, ENTRY, MONO), monoEntryForExactValues  $ categoryToQuery category) | category <- [LATITUDE, LONGITUDE]]
                       ++ [((category, ENTRY, MONO), monoEntryForMinValues    $ categoryToQuery category) | category <- [FROM_YEAR, FROM_RANGE, FROM_TIMESCALE]]
                       ++ [((category, ENTRY, MONO), monoEntryForMaxValues    $ categoryToQuery category) | category <- [UPTO_YEAR, UPTO_RANGE, UPTO_TIMESCALE]]
                       ++ [((category, ENTRY, POLY), polyEntryFromGameMap     $ categoryToQuery category) | category <- mapToGameCategories]
                       ++ [((category, ENTRY, POLY), polyEntryFromScenarioMap $ categoryToQuery category) | category <- mapToScenariosCategories]
                       ++ [((category, ENTRY, POLY), polyEntryFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((category, GROUP, OMNI), omniGroupFromMap         $ categoryToQuery category) | category <- mapCategories]
                       ++ [((category, GROUP, OMNI), omniGroupFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((category, GROUP, POLY), polyGroupFromGameMap     $ categoryToQuery category) | category <- mapToGameCategories]
                       ++ [((category, GROUP, POLY), polyGroupFromScenarioMap $ categoryToQuery category) | category <- mapToScenariosCategories]
                       ++ [((category, GROUP, POLY), polyGroupFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((category, GROUP, MONO), monoGroupFromMap         $ categoryToQuery category) | category <- mapCategories]
                       ++ [((category, GROUP, MONO), monoGroupFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((category, COUNT, POLY), polyCountFromGameMap     $ categoryToQuery category) | category <- mapToGameCategories]
                       ++ [((category, COUNT, POLY), polyCountFromScenarioMap $ categoryToQuery category) | category <- mapToScenariosCategories]
                       ++ [((category, COUNT, POLY), polyCountFromValues      $ categoryToQuery category) | category <- simpleValueCategories]
                       ++ [((SCENARIO, ENTRY, OMNI), "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario ORDER BY sd.title, sd.subtitle")]
                       ++ [((SCENARIO, ENTRY, MONO), "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario WHERE s.id = ?")]
                       ++ [((SCENARIO, ENTRY, POLY), "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario WHERE s.id IN ?")]
                       ++ [((GAME, ENTRY, OMNI),     "SELECT id, title, subtitle FROM dr_game ORDER BY title, subtitle")]
                       ++ [((GAME, ENTRY, MONO),     "SELECT id, title, subtitle FROM dr_game WHERE id = ?")]
                       ++ [((GAME, ENTRY, POLY),     "SELECT g.id, g.title, g.subtitle FROM dr_game AS g JOIN dr_scenario AS s ON s.id_game = g.id WHERE s.id IN ? GROUP BY g.id, g.title, g.subtitle ORDER BY g.title, g.subtitle")]
                       ++ [((GAME, GROUP, OMNI),     "SELECT grp, count(id) FROM dr_game GROUP BY grp ORDER BY grp")]
                       ++ [((GAME, GROUP, POLY),     "SELECT a.grp, count(distinct(a.id)) FROM dr_game AS a JOIN dr_scenario AS s ON a.id = s.id_game WHERE s.id IN ? GROUP BY grp ORDER BY grp")]
                       ++ [((GAME, GROUP, MONO),     "SELECT id, title, subtitle FROM dr_game WHERE grp = ? ORDER BY title")]
                       ++ [((GAME, COUNT, POLY),     "SELECT count(distinct(id_game)) FROM dr_scenario WHERE id IN ?")]

omniEntryFromMap, omniEntryFromValues, monoEntryFromMap,
 polyEntryFromGameMap, polyEntryFromScenarioMap, polyEntryFromValues,
 omniGroupFromMap, omniGroupFromValues,
 polyGroupFromGameMap, polyGroupFromScenarioMap, polyGroupFromValues,
 monoGroupFromMap, monoGroupFromValues,
 polyCountFromGameMap, polyCountFromScenarioMap, polyCountFromValues,
 monoEntryForExactValues, monoEntryForMinValues, monoEntryForMaxValues,
 omniEntryFromMap :: Query -> Query
omniEntryFromMap         q = mconcat ["SELECT id, ", q, " FROM dr_", q, " ORDER BY ", q]
omniEntryFromValues      q = mconcat ["SELECT ", q, " FROM dr_scenario GROUP BY ", q, " ORDER BY ", q]
monoEntryFromMap         q = mconcat ["SELECT id, ", q, " FROM dr_", q, " WHERE id = ?"]
polyEntryFromGameMap     q = mconcat ["SELECT d.id, d.", q, " FROM dr_", q, " AS d JOIN dr_map_", q, " AS m ON m.id_", q, " = d.id JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.", q, " ORDER BY d.", q]
polyEntryFromScenarioMap q = mconcat ["SELECT d.id, d.", q, " FROM dr_", q, " AS d JOIN dr_map_", q, " AS m ON m.id_", q, " = d.id WHERE m.id_scenario IN ? GROUP BY d.id, d.", q, " ORDER BY d.", q]
polyEntryFromValues      q = mconcat ["SELECT ", q, " FROM dr_scenario WHERE id IN ? GROUP BY ", q, " ORDER BY ", q]
omniGroupFromMap         q = mconcat ["SELECT grp, count(id) FROM dr_", q, " GROUP BY grp ORDER BY grp"]
omniGroupFromValues      q = mconcat ["SELECT ", q, "_group, count(distinct(", q, ")) FROM dr_scenario GROUP BY ", q, "_group ORDER BY ", q, "_group"]
polyGroupFromGameMap     q = mconcat ["SELECT a.grp, count(distinct(a.id)) FROM dr_", q, " AS a JOIN dr_map_", q, " AS ma on a.id = ma.id_", q, " JOIN dr_scenario AS s ON s.id_game = ma.id_game WHERE s.id IN ? GROUP BY grp ORDER BY grp"]
polyGroupFromScenarioMap q = mconcat ["SELECT a.grp, count(distinct(a.id)) FROM dr_", q, " AS a JOIN dr_map_", q, " AS ma on a.id = ma.id_", q, " JOIN dr_scenario AS s ON s.id = ma.id_scenario WHERE s.id IN ? GROUP BY grp ORDER BY grp"]
polyGroupFromValues      q = mconcat ["SELECT ", q, "_group, count(distinct(", q, ")) FROM dr_scenario WHERE id IN ? GROUP BY ", q, "_group ORDER BY ", q, "_group"]
monoGroupFromMap         q = mconcat ["SELECT id, ", q, " FROM dr_", q, " WHERE grp = ? ORDER BY ", q]
monoGroupFromValues      q = mconcat ["SELECT ", q, " FROM dr_scenario WHERE ", q, "_group = ? GROUP BY ", q, " ORDER BY ", q]
polyCountFromGameMap     q = mconcat ["SELECT count(distinct(id_", q, ")) FROM dr_map_", q, " AS a JOIN dr_scenario AS s ON s.id_game = a.id_game WHERE s.id IN ?"]
polyCountFromScenarioMap q = mconcat ["SELECT count(distinct(id_", q, ")) FROM dr_map_", q, " AS a JOIN dr_scenario AS s ON s.id = a.id_scenario WHERE s.id IN ?"]
polyCountFromValues      q = mconcat ["SELECT count(distinct(", q, ")) FROM dr_scenario WHERE id IN ?"]
monoEntryForMinValues    q = mconcat ["SELECT min(", q, ") FROM dr_scenario WHERE ", q, " >= ?"]
monoEntryForMaxValues    q = mconcat ["SELECT max(", q, ") FROM dr_scenario WHERE ", q, " <= ?"]
monoEntryForExactValues  q = mconcat ["SELECT ", q, " FROM dr_scenario WHERE ", q, " = ?"]

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
parameterList = [ "author"
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
                ]

joinForMap :: Query -> Query -> Query -> Query
joinForMap mapSource mapTarget q = mconcat [" JOIN dr_map_", q, " AS ", q, " ON s.", mapSource, " = ", q, ".", mapTarget]

joinForMapToGame :: Query -> Query
joinForMapToGame = joinForMap "id_game" "id_game"

joinForMapToScenario :: Query -> Query
joinForMapToScenario = joinForMap "id" "id_scenario"

joinList :: ComponentMap
joinList = fromList[ ("author",    joinForMapToScenario "author")
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
                   ]

whereIncludeForMap :: Query -> Query
whereIncludeForMap q = mconcat [" AND ", q, ".id_", q, " = ?"]

whereIncludeList :: ComponentMap
whereIncludeList = fromList[ ("author",    whereIncludeForMap "author")
                           , ("publisher", whereIncludeForMap "publisher")
                           , ("theme",     whereIncludeForMap "theme")
                           , ("genre",     whereIncludeForMap "genre")
                           , ("mechanic",  whereIncludeForMap "mechanic")
                           , ("side",      whereIncludeForMap "side")
                           , ("party",     whereIncludeForMap "party")
                           , ("series",    whereIncludeForMap "series")
                           , ("leader",    whereIncludeForMap "leader")
                           , ("engine",    whereIncludeForMap "engine")
                           , ("game",      " AND s.id_game              = ?")
                           , ("latitude",  " AND s.latitude_trunc       = ?")
                           , ("longitude", " AND s.longitude_trunc      = ?")
                           , ("fromYear",  " AND NOT s.year_upto        < ?")
                           , ("upToYear",  " AND NOT s.year_from        > ?")
                           , ("fromRange", " AND s.range               >= ?")
                           , ("upToRange", " AND s.range               <= ?")
                           ]

whereExcludeForMapToGame :: Query -> Query
whereExcludeForMapToGame = whereExcludeForMap "id_game" "id_game"

whereExcludeForMapToScenario :: Query -> Query
whereExcludeForMapToScenario = whereExcludeForMap "id_scenario" "id"

whereExcludeForMap :: Query -> Query -> Query -> Query
whereExcludeForMap what mappedTo q = mconcat [ " AND NOT EXISTS (SELECT ", what
                                             , " FROM dr_map_" ,q
                                             , " WHERE ", what, " = s.", mappedTo
                                             , " AND id_", q, " = (-1 * ?))"]

whereExcludeList :: ComponentMap
whereExcludeList = fromList [ ("author",    whereExcludeForMapToScenario "author")
                            , ("side",      whereExcludeForMapToScenario "side")
                            , ("party",     whereExcludeForMapToScenario "party")
                            , ("leader",    whereExcludeForMapToScenario "leader")
                            , ("publisher", whereExcludeForMapToGame "publisher")
                            , ("theme",     whereExcludeForMapToGame "theme")
                            , ("genre",     whereExcludeForMapToGame "genre")
                            , ("mechanic",  whereExcludeForMapToGame "mechanic")
                            , ("series",    whereExcludeForMapToGame "series")
                            , ("engine",    whereExcludeForMapToGame "engine")
                            , ("game",      " AND s.id_game             != (-1 * ?)")
                            , ("latitude",  " AND s.latitude_trunc       = ?")
                            , ("longitude", " AND s.longitude_trunc      = ?")
                            , ("fromYear",  " AND NOT s.year_upto        < ?")
                            , ("upToYear",  " AND NOT s.year_from        > ?")
                            , ("fromRange", " AND s.range               >= ?")
                            , ("upToRange", " AND s.range               <= ?")
                            ]

groupEntriesForMapPrefix :: Query -> Query -> Query -> Query
groupEntriesForMapPrefix mappedTo mapId q = mconcat [ "SELECT ", q, ".id, ", q, ".", q
                                                    , " FROM dr_scenario s JOIN dr_map_", q, " AS m_", q
                                                    , " ON s.", mappedTo, " = m_" , q, ".",mapId, " JOIN dr_", q, " AS ", q
                                                    , " ON m_", q, ".id_", q, " = ", q, ".id"
                                                    ]

groupEntriesForSimpleValuesPrefix :: Query -> Query
groupEntriesForSimpleValuesPrefix q = mconcat ["SELECT distinct(s.", q, ") FROM dr_scenario s"]

selectList :: ComponentMap
selectList = fromList[ ("author",    groupEntriesForMapPrefix "id" "id_scenario" "author")
                     , ("side",      groupEntriesForMapPrefix "id" "id_scenario" "side")
                     , ("party",     groupEntriesForMapPrefix "id" "id_scenario" "party")
                     , ("leader",    groupEntriesForMapPrefix "id" "id_scenario" "leader")
                     , ("series",    groupEntriesForMapPrefix "id_game" "id_game"  "series")
                     , ("publisher", groupEntriesForMapPrefix "id_game" "id_game"  "publisher")
                     , ("theme",     groupEntriesForMapPrefix "id_game" "id_game"  "theme")
                     , ("genre",     groupEntriesForMapPrefix "id_game" "id_game"  "genre")
                     , ("mechanic",  groupEntriesForMapPrefix "id_game" "id_game"  "mechanic")
                     , ("engine",    groupEntriesForMapPrefix "id_game" "id_game"  "engine")
                     , ("latitude",  groupEntriesForSimpleValuesPrefix "latitude_trunc")
                     , ("longitude", groupEntriesForSimpleValuesPrefix "longitude_trunc")
                     , ("fromYear",  groupEntriesForSimpleValuesPrefix "year_from")
                     , ("upToYear",  groupEntriesForSimpleValuesPrefix "year_upto")
                     , ("fromRange", groupEntriesForSimpleValuesPrefix "range")
                     , ("upToRange", groupEntriesForSimpleValuesPrefix "range")
                     , ("game",      "SELECT game.id, game.title, game.subtitle FROM dr_scenario s JOIN dr_game AS game ON s.id_game = game.id ")
                     ]

whereGroupEntriesForMapSuffix :: Query -> Query
whereGroupEntriesForMapSuffix q = mconcat [" WHERE ", q, ".grp = ?"]

whereGroupEntriesForSimpleValuesSuffix :: Query -> Query
whereGroupEntriesForSimpleValuesSuffix q = mconcat [" WHERE s.", q, "_group = ?"]

whereGroupList :: ComponentMap
whereGroupList = fromList[ ("author",    whereGroupEntriesForMapSuffix "author")
                         , ("publisher", whereGroupEntriesForMapSuffix "publisher")
                         , ("theme",     whereGroupEntriesForMapSuffix "theme")
                         , ("genre",     whereGroupEntriesForMapSuffix "genre")
                         , ("mechanic",  whereGroupEntriesForMapSuffix "mechanic")
                         , ("side",      whereGroupEntriesForMapSuffix "side")
                         , ("party",     whereGroupEntriesForMapSuffix "party")
                         , ("series",    whereGroupEntriesForMapSuffix "series")
                         , ("leader",    whereGroupEntriesForMapSuffix "leader")
                         , ("engine",    whereGroupEntriesForMapSuffix "engine")
                         , ("latitude",  whereGroupEntriesForSimpleValuesSuffix "latitude_trunc")
                         , ("longitude", whereGroupEntriesForSimpleValuesSuffix "longitude_trunc")
                         , ("fromYear",  whereGroupEntriesForSimpleValuesSuffix "year_from")
                         , ("upToYear",  whereGroupEntriesForSimpleValuesSuffix "year_upto")
                         , ("fromRange", whereGroupEntriesForSimpleValuesSuffix "range")
                         , ("upToRange", whereGroupEntriesForSimpleValuesSuffix "range")
                         , ("game",      " WHERE game.grp = ?")
                         ]

orderGroupEntriesForMapSuffix :: Query -> Query
orderGroupEntriesForMapSuffix q = mconcat [" GROUP BY ", q, ".id, ", q, ".", q, " ORDER BY ", q, ".", q]

orderGroupEntriesForSimpleValuesSuffix :: Query -> Query
orderGroupEntriesForSimpleValuesSuffix q = mconcat [" ORDER BY s.", q]

orderGroupList :: ComponentMap
orderGroupList = fromList[ ("author",    orderGroupEntriesForMapSuffix "author")
                         , ("publisher", orderGroupEntriesForMapSuffix "publisher")
                         , ("theme",     orderGroupEntriesForMapSuffix "theme")
                         , ("genre",     orderGroupEntriesForMapSuffix "genre")
                         , ("mechanic",  orderGroupEntriesForMapSuffix "mechanic")
                         , ("side",      orderGroupEntriesForMapSuffix "side")
                         , ("party",     orderGroupEntriesForMapSuffix "party")
                         , ("series",    orderGroupEntriesForMapSuffix "series")
                         , ("leader",    orderGroupEntriesForMapSuffix "leader")
                         , ("engine",    orderGroupEntriesForMapSuffix "engine")
                         , ("latitude",  orderGroupEntriesForSimpleValuesSuffix "latitude_trunc")
                         , ("longitude", orderGroupEntriesForSimpleValuesSuffix "longitude_trunc")
                         , ("fromYear",  orderGroupEntriesForSimpleValuesSuffix "year_from")
                         , ("upToYear",  orderGroupEntriesForSimpleValuesSuffix "year_upto")
                         , ("fromRange", orderGroupEntriesForSimpleValuesSuffix "range")
                         , ("upToRange", orderGroupEntriesForSimpleValuesSuffix "range")
                         , ("game",      " GROUP BY game.title, game.subtitle, game.id ORDER BY game.title, game.subtitle")
                         ]

