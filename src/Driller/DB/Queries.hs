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
    )
import Database.PostgreSQL.Simple ( Query )
import Data.Monoid ( mappend, mconcat )
import Data.List ( foldl' )
import Data.Text ( Text )
import qualified Data.DList as DL ( toList, fromList, append, concat )
import Data.HashMap.Strict ( fromList, (!) )

groupQuery :: Config -> Text -> [Parameter] -> Query
groupQuery config group pList = mconcat $ DL.toList $ DL.concat [DL.fromList [select], DL.fromList joins, DL.fromList (" WHERE 1=1":wheres), DL.fromList [order]]
                   where (select, order) = groupMap ! group
                         (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList
                         joinMap = getJoinMap config
                         groupMap = getGroupMap config

scenarioListQuery :: Config -> [Parameter] -> Query
scenarioListQuery config pList = foldl' mappend prefix parts
             where prefix = "SELECT s.id FROM dr_scenario AS s"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList
                   joinMap = getJoinMap config

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, value) = (j, w)
                                        where (j, w1, w2) = (joinMap !) key
                                              w = if value >= 0 then w1 else w2

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
initGroupMap = fromList $ prepareGroupList parameterList selectList orderList

prepareGroupList :: [Text] -> ComponentMap -> ComponentMap -> [(Text, GroupMapComponents)]
prepareGroupList (p:ps) s o = (p, (s ! p, o ! p)) : prepareGroupList ps s o
prepareGroupList _ _ _      = []

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

joinList :: ComponentMap
joinList = fromList[ ("author",    " JOIN dr_map_author    AS author    ON s.id      = author.id_scenario")
                   , ("publisher", " JOIN dr_map_publisher AS publisher ON s.id_game = publisher.id_game")
                   , ("theme",     " JOIN dr_map_theme     AS theme     ON s.id_game = theme.id_game")
                   , ("genre",     " JOIN dr_map_genre     AS genre     ON s.id_game = genre.id_game")
                   , ("mechanic",  " JOIN dr_map_mechanic  AS mechanic  ON s.id_game = mechanic.id_game")
                   , ("side",      " JOIN dr_map_side      AS side      ON s.id      = side.id_scenario")
                   , ("party",     " JOIN dr_map_party     AS party     ON s.id      = party.id_scenario")
                   , ("series",    " JOIN dr_map_series    AS series    ON s.id_game = series.id_game")
                   , ("leader",    " JOIN dr_map_leader    AS leader    ON s.id      = leader.id_scenario")
                   , ("engine",    " JOIN dr_map_engine    AS engine    ON s.id_game = engine.id_game")
                   , ("game",      "")
                   , ("latitude",  "")
                   , ("longitude", "")
                   , ("fromYear",  "")
                   , ("upToYear",  "")
                   , ("fromRange", "")
                   , ("upToRange", "")
                   ]

whereIncludeList :: ComponentMap
whereIncludeList = fromList[ ("author",    " AND author.id_author       = ?")
                           , ("publisher", " AND publisher.id_publisher = ?")
                           , ("theme",     " AND theme.id_theme         = ?")
                           , ("genre",     " AND genre.id_genre         = ?")
                           , ("mechanic",  " AND mechanic.id_mechanic   = ?")
                           , ("side",      " AND side.id_side           = ?")
                           , ("party",     " AND party.id_party         = ?")
                           , ("series",    " AND series.id_series       = ?")
                           , ("leader",    " AND leader.id_leader       = ?")
                           , ("engine",    " AND engine.id_engine       = ?")
                           , ("game",      " AND s.id_game              = ?")
                           , ("latitude",  " AND s.latitude_trunc       = ?")
                           , ("longitude", " AND s.longitude_trunc      = ?")
                           , ("fromYear",  " AND NOT s.year_upto        < ?")
                           , ("upToYear",  " AND NOT s.year_from        > ?")
                           , ("fromRange", " AND s.range               >= ?")
                           , ("upToRange", " AND s.range               <= ?")
                           ]

whereExcludeList :: ComponentMap
whereExcludeList = fromList [ ("author",    " AND NOT EXISTS (SELECT id_scenario FROM dr_map_author    WHERE id_scenario = s.id AND id_author         = (-1 * ?))")
                            , ("side",      " AND NOT EXISTS (SELECT id_scenario FROM dr_map_side      WHERE id_scenario = s.id AND id_side           = (-1 * ?))")
                            , ("party",     " AND NOT EXISTS (SELECT id_scenario FROM dr_map_party     WHERE id_scenario = s.id AND id_party          = (-1 * ?))")
                            , ("leader",    " AND NOT EXISTS (SELECT id_scenario FROM dr_map_leader    WHERE id_scenario = s.id AND id_leader         = (-1 * ?))")
                            , ("publisher", " AND NOT EXISTS (SELECT id_game     FROM dr_map_publisher WHERE id_game     = s.id_game AND id_publisher = (-1 * ?))")
                            , ("theme",     " AND NOT EXISTS (SELECT id_game     FROM dr_map_theme     WHERE id_game     = s.id_game AND id_theme     = (-1 * ?))")
                            , ("genre",     " AND NOT EXISTS (SELECT id_game     FROM dr_map_genre     WHERE id_game     = s.id_game AND id_genre     = (-1 * ?))")
                            , ("mechanic",  " AND NOT EXISTS (SELECT id_game     FROM dr_map_mechanic  WHERE id_game     = s.id_game AND id_mechanic  = (-1 * ?))")
                            , ("series",    " AND NOT EXISTS (SELECT id_game     FROM dr_map_series    WHERE id_game     = s.id_game AND id_series    = (-1 * ?))")
                            , ("engine",    " AND NOT EXISTS (SELECT id_game     FROM dr_map_engine    WHERE id_game     = s.id_game AND id_engine    = (-1 * ?))")
                            , ("game",      " AND s.id_game             != (-1 * ?)")
                            , ("latitude",  " AND s.latitude_trunc       = ?")
                            , ("longitude", " AND s.longitude_trunc      = ?")
                            , ("fromYear",  " AND NOT s.year_upto        < ?")
                            , ("upToYear",  " AND NOT s.year_from        > ?")
                            , ("fromRange", " AND s.range               >= ?")
                            , ("uptoRange", " AND s.range               <= ?")
                            ]

selectList :: ComponentMap
selectList = fromList[ ("author",    "SELECT author.id, author.author FROM scenario s")
                     , ("publisher", "SELECT publisher.id, publisher.publisher FROM scenario s")
                     , ("theme",     "SELECT theme.id, theme.theme FROM scenario s")
                     , ("genre",     "SELECT genre.id, genre.genre FROM scenario s")
                     , ("mechanic",  "SELECT mechanic.id, mechanic.mechanic FROM scenario s")
                     , ("side",      "SELECT side.id, side.side FROM scenario s")
                     , ("party",     "SELECT party.id, party.party FROM scenario s")
                     , ("series",    "SELECT series.id, series.series FROM scenario s")
                     , ("leader",    "SELECT leader.id, leader.leader FROM scenario s")
                     , ("engine",    "SELECT engine.id, engine.engine FROM scenario s")
                     , ("game",      "SELECT game.id, game.game FROM scenario s")
                     , ("latitude",  "SELECT latitude.id, latitude.latitude FROM scenario s")
                     , ("longitude", "SELECT longitude.id, longitude.longitude FROM scenario s")
                     , ("fromYear",  "SELECT year_from.id, year_from.year_from FROM scenario s")
                     , ("upToYear",  "SELECT year_upto.id, year_upto.year_upto FROM scenario s")
                     , ("fromRange", "SELECT range_from.id, range_from.range_from FROM scenario s")
                     , ("upToRange", "SELECT range_upto.id, range_upto.range_upto FROM scenario s")
                     ]

orderList :: ComponentMap
orderList = fromList[ ("author",    " AND author.grp = ? ORDER BY author.author")
                    , ("publisher", " AND publisher.grp = ? ORDER BY publisher.publisher")
                    , ("theme",     " AND theme.grp = ? ORDER BY theme.theme")
                    , ("genre",     " AND genre.grp = ? ORDER BY genre.genre")
                    , ("mechanic",  " AND mechanic.grp = ? ORDER BY mechanic.mechanic")
                    , ("side",      " AND side.grp = ? ORDER BY side.side")
                    , ("party",     " AND party.grp = ? ORDER BY party.party")
                    , ("series",    " AND series.grp = ? ORDER BY series.series")
                    , ("leader",    " AND leader.grp = ? ORDER BY leader.leader")
                    , ("engine",    " AND engine.grp = ? ORDER BY engine.engine")
                    , ("game",      " AND game.grp = ? ORDER BY game.title, game.subtitle")
                    , ("latitude",  " AND s.latitude_group = ? GROUP BY (s.latitude_trunc) ORDER BY s.latitude_trunc")
                    , ("longitude", " AND s.longitude_group = ? GROUP BY (s.longitude_trunc) ORDER BY s.longitude_trunc")
                    , ("fromYear",  " AND s.year_from_group = ? GROUP BY (s.year_from) ORDER BY s.year_from")
                    , ("upToYear",  " AND s.year_upto_group = ? GROUP BY (s.year_upto) ORDER BY s.year_upto")
                    , ("fromRange", " AND s.range_group = ? GROUP BY (s.range) ORDER BY s.range")
                    , ("upToRange", " AND s.range_group = ? GROUP BY (s.range) ORDER BY s.range")
                    ]

