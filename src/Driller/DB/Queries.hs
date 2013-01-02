{-# LANGUAGE OverloadedStrings #-}
module Driller.DB.Queries
    ( JoinMap
    , allAuthorsQuery
    , allEnginesQuery
    , allGamesQuery
    , allGenresQuery
    , allLatitudesQuery
    , allLeadersQuery
    , allLongitudesQuery
    , allMechanicsQuery
    , allPartiesQuery
    , allPublishersQuery
    , allFromRangesQuery
    , allUpToRangesQuery
    , allSeriesQuery
    , allSidesQuery
    , allThemesQuery
    , allFromYearsQuery
    , allUpToYearsQuery
    , authorQuery
    , authorsQuery
    , engineQuery
    , enginesQuery
    , scenarioListQuery
    , gameQuery
    , gamesQuery
    , genreQuery
    , genresQuery
    , initJoinMap
    , latitudeQuery
    , latitudesQuery
    , leaderQuery
    , leadersQuery
    , longitudeQuery
    , longitudesQuery
    , mechanicQuery
    , mechanicsQuery
    , partiesQuery
    , partyQuery
    , publisherQuery
    , publishersQuery
    , fromRangeQuery
    , fromRangesQuery
    , upToRangeQuery
    , upToRangesQuery
    , seriesQuery
    , seriessQuery
    , sideQuery
    , sidesQuery
    , themeQuery
    , themesQuery
    , fromYearQuery
    , fromYearsQuery
    , upToYearQuery
    , upToYearsQuery
    , scenariosQuery
    ) where

import Driller.Data ( JoinMap, Parameter )
import Database.PostgreSQL.Simple ( Query )
import Data.Monoid ( mappend )
import Data.List ( foldl' )
import Data.Text ( Text )
import qualified Data.DList as DL ( toList, fromList, append )
import qualified Data.HashMap.Strict as HM ( fromList, (!) )

authorQuery, genreQuery, engineQuery, themeQuery, mechanicQuery, sideQuery,
 partyQuery, publisherQuery, seriesQuery, leaderQuery :: Query
authorQuery        = "SELECT id, author    FROM dr_author    WHERE id = ?"
genreQuery         = "SELECT id, genre     FROM dr_genre     WHERE id = ?"
engineQuery        = "SELECT id, engine    FROM dr_engine    WHERE id = ?"
themeQuery         = "SELECT id, theme     FROM dr_theme     WHERE id = ?"
mechanicQuery      = "SELECT id, mechanic  FROM dr_mechanic  WHERE id = ?"
sideQuery          = "SELECT id, side      FROM dr_side      WHERE id = ?"
partyQuery         = "SELECT id, party     FROM dr_party     WHERE id = ?"
publisherQuery     = "SELECT id, publisher FROM dr_publisher WHERE id = ?"
seriesQuery        = "SELECT id, series    FROM dr_series    WHERE id = ?"
leaderQuery        = "SELECT id, leader    FROM dr_leader    WHERE id = ?"

authorsQuery, genresQuery, enginesQuery, themesQuery, mechanicsQuery, sidesQuery,
 partiesQuery, publishersQuery, seriessQuery, leadersQuery :: Query
genresQuery        = "SELECT d.id, d.genre     FROM dr_genre AS d     JOIN dr_map_genre AS m     ON m.id_genre = d.id     JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.genre     ORDER BY d.genre"
enginesQuery       = "SELECT d.id, d.engine    FROM dr_engine AS d    JOIN dr_map_engine AS m    ON m.id_engine = d.id    JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.engine    ORDER BY d.engine"
themesQuery        = "SELECT d.id, d.theme     FROM dr_theme AS d     JOIN dr_map_theme AS m     ON m.id_theme = d.id     JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.theme     ORDER BY d.theme"
mechanicsQuery     = "SELECT d.id, d.mechanic  FROM dr_mechanic AS d  JOIN dr_map_mechanic AS m  ON m.id_mechanic = d.id  JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.mechanic  ORDER BY d.mechanic"
publishersQuery    = "SELECT d.id, d.publisher FROM dr_publisher AS d JOIN dr_map_publisher AS m ON m.id_publisher = d.id JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.publisher ORDER BY d.publisher"
seriessQuery       = "SELECT d.id, d.series    FROM dr_series AS d    JOIN dr_map_series AS m    ON m.id_series = d.id    JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.series    ORDER BY d.series"
authorsQuery       = "SELECT d.id, d.author    FROM dr_author AS d    JOIN dr_map_author AS m    ON m.id_author = d.id    WHERE m.id_scenario IN ? GROUP BY d.id, d.author    ORDER BY d.author"
sidesQuery         = "SELECT d.id, d.side      FROM dr_side AS d      JOIN dr_map_side AS m      ON m.id_side = d.id      WHERE m.id_scenario IN ? GROUP BY d.id, d.side      ORDER BY d.side"
partiesQuery       = "SELECT d.id, d.party     FROM dr_party AS d     JOIN dr_map_party AS m     ON m.id_party = d.id     WHERE m.id_scenario IN ? GROUP BY d.id, d.party     ORDER BY d.party"
leadersQuery       = "SELECT d.id, d.leader    FROM dr_leader AS d    JOIN dr_map_leader AS m    ON m.id_leader = d.id    WHERE m.id_scenario IN ? GROUP BY d.id, d.leader    ORDER BY d.leader"

allAuthorsQuery, allGenresQuery, allEnginesQuery, allThemesQuery, allMechanicsQuery, allSidesQuery,
 allPartiesQuery, allPublishersQuery, allSeriesQuery, allLeadersQuery :: Query
allAuthorsQuery    = "SELECT id, author    FROM dr_author    ORDER BY author"
allGenresQuery     = "SELECT id, genre     FROM dr_genre     ORDER BY genre"
allEnginesQuery    = "SELECT id, engine    FROM dr_engine    ORDER BY engine"
allThemesQuery     = "SELECT id, theme     FROM dr_theme     ORDER BY theme"
allMechanicsQuery  = "SELECT id, mechanic  FROM dr_mechanic  ORDER BY mechanic"
allSidesQuery      = "SELECT id, side      FROM dr_side      ORDER BY side"
allPartiesQuery    = "SELECT id, party     FROM dr_party     ORDER BY party"
allPublishersQuery = "SELECT id, publisher FROM dr_publisher ORDER BY publisher"
allSeriesQuery     = "SELECT id, series    FROM dr_series    ORDER BY series"
allLeadersQuery    = "SELECT id, leader    FROM dr_leader    ORDER BY leader"

gameQuery, gamesQuery, allGamesQuery :: Query
gameQuery          = "SELECT id, title, subtitle FROM dr_game WHERE id = ?"
gamesQuery         = "SELECT g.id, g.title, g.subtitle FROM dr_game AS g JOIN dr_scenario AS s ON s.id_game = g.id WHERE s.id IN ? GROUP BY g.id, g.title, g.subtitle ORDER BY g.title, g.subtitle"
allGamesQuery      = "SELECT id, title, subtitle FROM dr_game ORDER BY title, subtitle"

scenariosQuery :: Query
scenariosQuery     = "SELECT sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario WHERE s.id IN ?"

latitudeQuery, longitudeQuery, fromYearQuery, upToYearQuery, fromRangeQuery, upToRangeQuery :: Query
latitudeQuery      = "SELECT latitude_trunc  FROM dr_scenario WHERE latitude_trunc = ?"
longitudeQuery     = "SELECT longitude_trunc FROM dr_scenario WHERE longitude_trunc = ?"
fromYearQuery      = "SELECT min(year_from)  FROM dr_scenario WHERE year_from >= ?"
upToYearQuery      = "SELECT max(year_upto)  FROM dr_scenario WHERE year_upto <= ?"
fromRangeQuery     = "SELECT min(range)      FROM dr_scenario WHERE range >= ?"
upToRangeQuery     = "SELECT max(range)      FROM dr_scenario WHERE range <= ?"

latitudesQuery, longitudesQuery, fromYearsQuery, upToYearsQuery, fromRangesQuery, upToRangesQuery :: Query
latitudesQuery     = "SELECT latitude_trunc  FROM dr_scenario WHERE id IN ? GROUP BY latitude_trunc  ORDER BY latitude_trunc"
longitudesQuery    = "SELECT longitude_trunc FROM dr_scenario WHERE id IN ? GROUP BY longitude_trunc ORDER BY longitude_trunc"
fromYearsQuery     = "SELECT year_from       FROM dr_scenario WHERE id IN ? GROUP BY year_from       ORDER BY year_from"
upToYearsQuery     = "SELECT year_upto       FROM dr_scenario WHERE id IN ? GROUP BY year_upto       ORDER BY year_upto"
fromRangesQuery    = "SELECT range           FROM dr_scenario WHERE id IN ? GROUP BY range           ORDER BY range"
upToRangesQuery    = "SELECT range           FROM dr_scenario WHERE id IN ? GROUP BY range           ORDER BY range"

allLatitudesQuery, allLongitudesQuery, allFromYearsQuery, allUpToYearsQuery, allFromRangesQuery, allUpToRangesQuery :: Query
allLatitudesQuery  = "SELECT latitude_trunc  FROM dr_scenario GROUP BY latitude_trunc  ORDER BY latitude_trunc"
allLongitudesQuery = "SELECT longitude_trunc FROM dr_scenario GROUP BY longitude_trunc ORDER BY longitude_trunc"
allFromYearsQuery  = "SELECT year_from       FROM dr_scenario GROUP BY year_from       ORDER BY year_from"
allUpToYearsQuery  = "SELECT year_upto       FROM dr_scenario GROUP BY year_upto       ORDER BY year_upto"
allFromRangesQuery = "SELECT range           FROM dr_scenario GROUP BY range           ORDER BY range"
allUpToRangesQuery = "SELECT range           FROM dr_scenario GROUP BY range           ORDER BY range"

scenarioListQuery :: JoinMap -> [Parameter] -> Query
scenarioListQuery joinMap pList = foldl' mappend prefix parts
             where prefix = "SELECT s.id FROM dr_scenario AS s"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, value) = (j, w)
                                        where (j, w1, w2) = (joinMap HM.!) key
                                              w = if value >= 0 then w1 else w2

initJoinMap :: JoinMap
initJoinMap = HM.fromList $ prepareList parameterList joinList whereIncludeList whereExcludeList

prepareList :: [Text] -> [Query] -> [Query] -> [Query] -> [(Text, (Query, Query, Query))]
prepareList (p:ps) (j:js) (wi:wis) (we:wes) = (p, (j, wi, we)) : prepareList ps js wis wes
prepareList _ _ _ _                         = []

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

joinList, whereIncludeList, whereExcludeList :: [Query]
joinList = [ " JOIN dr_map_author    AS author    ON s.id = author.id_scenario"
           , " JOIN dr_map_publisher AS publisher ON s.id_game = publisher.id_game"
           , " JOIN dr_map_theme     AS theme     ON s.id_game = theme.id_game"
           , " JOIN dr_map_genre     AS genre     ON s.id_game = genre.id_game"
           , " JOIN dr_map_mechanic  AS mechanic  ON s.id_game = mechanic.id_game"
           , " JOIN dr_map_side      AS side      ON s.id = side.id_scenario"
           , " JOIN dr_map_party     AS party     ON s.id = party.id_scenario"
           , " JOIN dr_map_series    AS series    ON s.id_game = series.id_game"
           , " JOIN dr_map_leader    AS leader    ON s.id = leader.id_scenario"
           , " JOIN dr_map_engine    AS engine    ON s.id_game = engine.id_game"
           , ""
           , ""
           , ""
           , ""
           , ""
           , ""
           , ""
           ]

whereIncludeList = [ " AND author.id_author       = ?"
                   , " AND publisher.id_publisher = ?"
                   , " AND theme.id_theme         = ?"
                   , " AND genre.id_genre         = ?"
                   , " AND mechanic.id_mechanic   = ?"
                   , " AND side.id_side           = ?"
                   , " AND party.id_party         = ?"
                   , " AND series.id_series       = ?"
                   , " AND leader.id_leader       = ?"
                   , " AND engine.id_engine       = ?"
                   , " AND s.id_game              = ?"
                   , " AND s.latitude_trunc       = ?"
                   , " AND s.longitude_trunc      = ?"
                   , " AND NOT s.year_upto        < ?"
                   , " AND NOT s.year_from        > ?"
                   , " AND s.range               >= ?"
                   , " AND s.range               <= ?"
                   ]

whereExcludeList = [ " AND NOT EXISTS (SELECT id_scenario FROM dr_map_author    WHERE id_scenario = s.id AND id_author    = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_scenario FROM dr_map_side      WHERE id_scenario = s.id AND id_side      = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_scenario FROM dr_map_party     WHERE id_scenario = s.id AND id_party     = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_scenario FROM dr_map_leader    WHERE id_scenario = s.id AND id_leader    = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_publisher WHERE id_game = s.id_game AND id_publisher = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_theme     WHERE id_game = s.id_game AND id_theme     = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_genre     WHERE id_game = s.id_game AND id_genre     = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_mechanic  WHERE id_game = s.id_game AND id_mechanic  = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_series    WHERE id_game = s.id_game AND id_series    = (-1 * ?))"
                   , " AND NOT EXISTS (SELECT id_game FROM dr_map_engine    WHERE id_game = s.id_game AND id_engine    = (-1 * ?))"
                   , " AND s.id_game             != (-1 * ?)"
                   , " AND s.latitude_trunc       = ?"
                   , " AND s.longitude_trunc      = ?"
                   , " AND NOT s.year_upto        < ?"
                   , " AND NOT s.year_from        > ?"
                   , " AND s.range               >= ?"
                   , " AND s.range               <= ?"
                   ]
