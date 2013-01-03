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
    , allRangesQuery
    , allTimescaleQuery
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
    , rangesQuery
    , upToRangeQuery
    , timescalesQuery
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
    , scenarioQuery
    , scenariosQuery
    , allScenariosQuery
    , authorGroupsQuery
    , authorGroupQuery
    , genreGroupsQuery
    , genreGroupQuery
    , seriesGroupsQuery
    , seriesGroupQuery
    , leaderGroupsQuery
    , leaderGroupQuery
    , sideGroupsQuery
    , sideGroupQuery
    , partieGroupsQuery
    , partyGroupQuery
    , gameGroupsQuery
    , gameGroupQuery
    , engineGroupsQuery
    , engineGroupQuery
    , mechanicGroupsQuery
    , mechanicGroupQuery
    , publisherGroupsQuery
    , publisherGroupQuery
    , themeGroupsQuery
    , themeGroupQuery
    , fromYearGroupsQuery
    , fromYearGroupQuery
    , upToYearGroupsQuery
    , upToYearGroupQuery
    , latitudeGroupsQuery
    , latitudeGroupQuery
    , longitudeGroupsQuery
    , longitudeGroupQuery
    , rangeGroupsQuery
    , rangeGroupQuery
    , timescaleGroupsQuery
    , timescaleGroupQuery
    ) where

import Driller.Data ( JoinMap, JoinComponentMap, Parameter )
import Database.PostgreSQL.Simple ( Query )
import Data.Monoid ( mappend )
import Data.List ( foldl' )
import Data.Text ( Text )
import qualified Data.DList as DL ( toList, fromList, append )
import Data.HashMap.Strict ( fromList, (!) )

authorQuery, genreQuery, engineQuery, themeQuery, mechanicQuery, sideQuery,
 partyQuery, publisherQuery, seriesQuery, leaderQuery :: Query
authorQuery        = "SELECT id, author    FROM dr_author    WHERE id = ?"
engineQuery        = "SELECT id, engine    FROM dr_engine    WHERE id = ?"
genreQuery         = "SELECT id, genre     FROM dr_genre     WHERE id = ?"
leaderQuery        = "SELECT id, leader    FROM dr_leader    WHERE id = ?"
mechanicQuery      = "SELECT id, mechanic  FROM dr_mechanic  WHERE id = ?"
partyQuery         = "SELECT id, party     FROM dr_party     WHERE id = ?"
publisherQuery     = "SELECT id, publisher FROM dr_publisher WHERE id = ?"
seriesQuery        = "SELECT id, series    FROM dr_series    WHERE id = ?"
sideQuery          = "SELECT id, side      FROM dr_side      WHERE id = ?"
themeQuery         = "SELECT id, theme     FROM dr_theme     WHERE id = ?"

authorsQuery, genresQuery, enginesQuery, themesQuery, mechanicsQuery, sidesQuery,
 partiesQuery, publishersQuery, seriessQuery, leadersQuery :: Query
authorsQuery       = "SELECT d.id, d.author    FROM dr_author AS d    JOIN dr_map_author AS m    ON m.id_author = d.id    WHERE m.id_scenario IN ? GROUP BY d.id, d.author    ORDER BY d.author"
enginesQuery       = "SELECT d.id, d.engine    FROM dr_engine AS d    JOIN dr_map_engine AS m    ON m.id_engine = d.id    JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.engine    ORDER BY d.engine"
genresQuery        = "SELECT d.id, d.genre     FROM dr_genre AS d     JOIN dr_map_genre AS m     ON m.id_genre = d.id     JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.genre     ORDER BY d.genre"
leadersQuery       = "SELECT d.id, d.leader    FROM dr_leader AS d    JOIN dr_map_leader AS m    ON m.id_leader = d.id    WHERE m.id_scenario IN ? GROUP BY d.id, d.leader    ORDER BY d.leader"
mechanicsQuery     = "SELECT d.id, d.mechanic  FROM dr_mechanic AS d  JOIN dr_map_mechanic AS m  ON m.id_mechanic = d.id  JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.mechanic  ORDER BY d.mechanic"
partiesQuery       = "SELECT d.id, d.party     FROM dr_party AS d     JOIN dr_map_party AS m     ON m.id_party = d.id     WHERE m.id_scenario IN ? GROUP BY d.id, d.party     ORDER BY d.party"
publishersQuery    = "SELECT d.id, d.publisher FROM dr_publisher AS d JOIN dr_map_publisher AS m ON m.id_publisher = d.id JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.publisher ORDER BY d.publisher"
seriessQuery       = "SELECT d.id, d.series    FROM dr_series AS d    JOIN dr_map_series AS m    ON m.id_series = d.id    JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.series    ORDER BY d.series"
sidesQuery         = "SELECT d.id, d.side      FROM dr_side AS d      JOIN dr_map_side AS m      ON m.id_side = d.id      WHERE m.id_scenario IN ? GROUP BY d.id, d.side      ORDER BY d.side"
themesQuery        = "SELECT d.id, d.theme     FROM dr_theme AS d     JOIN dr_map_theme AS m     ON m.id_theme = d.id     JOIN dr_scenario AS s ON s.id_game = m.id_game WHERE s.id IN ? GROUP BY d.id, d.theme     ORDER BY d.theme"

authorGroupsQuery, genreGroupsQuery, seriesGroupsQuery, leaderGroupsQuery, gameGroupsQuery, sideGroupsQuery, fromYearGroupsQuery, upToYearGroupsQuery,
 latitudeGroupsQuery, longitudeGroupsQuery, rangeGroupsQuery, timescaleGroupsQuery,
 engineGroupsQuery, mechanicGroupsQuery, publisherGroupsQuery, partieGroupsQuery, themeGroupsQuery :: Query
authorGroupsQuery    = "SELECT grp, count(id) FROM dr_author    GROUP BY grp ORDER BY grp"
engineGroupsQuery    = "SELECT grp, count(id) FROM dr_engine    GROUP BY grp ORDER BY grp"
gameGroupsQuery      = "SELECT grp, count(id) FROM dr_game      GROUP BY grp ORDER BY grp"
genreGroupsQuery     = "SELECT grp, count(id) FROM dr_genre     GROUP BY grp ORDER BY grp"
leaderGroupsQuery    = "SELECT grp, count(id) FROM dr_leader    GROUP BY grp ORDER BY grp"
mechanicGroupsQuery  = "SELECT grp, count(id) FROM dr_mechanic  GROUP BY grp ORDER BY grp"
partieGroupsQuery    = "SELECT grp, count(id) FROM dr_party     GROUP BY grp ORDER BY grp"
publisherGroupsQuery = "SELECT grp, count(id) FROM dr_publisher GROUP BY grp ORDER BY grp"
seriesGroupsQuery    = "SELECT grp, count(id) FROM dr_series    GROUP BY grp ORDER BY grp"
sideGroupsQuery      = "SELECT grp, count(id) FROM dr_side      GROUP BY grp ORDER BY grp"
themeGroupsQuery     = "SELECT grp, count(id) FROM dr_theme     GROUP BY grp ORDER BY grp"
fromYearGroupsQuery  = "SELECT year_from_group, count(distinct(year_from))       FROM dr_scenario  GROUP BY year_from_group ORDER BY year_from_group"
upToYearGroupsQuery  = "SELECT year_upto_group, count(distinct(year_upto))       FROM dr_scenario  GROUP BY year_upto_group ORDER BY year_upto_group"
latitudeGroupsQuery  = "SELECT latitude_group,  count(distinct(latitude_trunc))  FROM dr_scenario  GROUP BY latitude_group  ORDER BY latitude_group"
longitudeGroupsQuery = "SELECT longitude_group, count(distinct(longitude_trunc)) FROM dr_scenario  GROUP BY longitude_group ORDER BY longitude_group"
rangeGroupsQuery     = "SELECT range_group,     count(distinct(range))           FROM dr_scenario  GROUP BY range_group     ORDER BY range_group"
timescaleGroupsQuery = "SELECT timescale_group, count(distinct(timescale))       FROM dr_scenario  GROUP BY timescale_group ORDER BY timescale_group"

authorGroupQuery, genreGroupQuery, seriesGroupQuery, leaderGroupQuery, gameGroupQuery, fromYearGroupQuery, upToYearGroupQuery,
 latitudeGroupQuery, longitudeGroupQuery, rangeGroupQuery, timescaleGroupQuery,
 engineGroupQuery, mechanicGroupQuery, publisherGroupQuery, sideGroupQuery, partyGroupQuery, themeGroupQuery :: Query
authorGroupQuery    = "SELECT id, author          FROM dr_author    WHERE grp = ? ORDER BY author"
engineGroupQuery    = "SELECT id, engine          FROM dr_engine    WHERE grp = ? ORDER BY engine"
gameGroupQuery      = "SELECT id, title, subtitle FROM dr_game      WHERE grp = ? ORDER BY title"
genreGroupQuery     = "SELECT id, genre           FROM dr_genre     WHERE grp = ? ORDER BY genre"
leaderGroupQuery    = "SELECT id, leader          FROM dr_leader    WHERE grp = ? ORDER BY leader"
mechanicGroupQuery  = "SELECT id, mechanic        FROM dr_mechanic  WHERE grp = ? ORDER BY mechanic"
partyGroupQuery     = "SELECT id, party           FROM dr_party     WHERE grp = ? ORDER BY party"
publisherGroupQuery = "SELECT id, publisher       FROM dr_publisher WHERE grp = ? ORDER BY publisher"
seriesGroupQuery    = "SELECT id, series          FROM dr_series    WHERE grp = ? ORDER BY series"
sideGroupQuery      = "SELECT id, side            FROM dr_side      WHERE grp = ? ORDER BY side"
themeGroupQuery     = "SELECT id, theme           FROM dr_theme     WHERE grp = ? ORDER BY theme"
fromYearGroupQuery  = "SELECT year_from           FROM dr_scenario  WHERE year_from_group = ? GROUP BY year_from ORDER BY year_from"
upToYearGroupQuery  = "SELECT year_upto           FROM dr_scenario  WHERE year_upto_group = ? GROUP BY year_upto ORDER BY year_upto"
latitudeGroupQuery  = "SELECT latitude            FROM dr_scenario  WHERE latitude_group =  ? GROUP BY latitude  ORDER BY latitude"
longitudeGroupQuery = "SELECT longitude           FROM dr_scenario  WHERE longitude_group = ? GROUP BY longitude ORDER BY longitude"
rangeGroupQuery     = "SELECT range               FROM dr_scenario  WHERE range_group =     ? GROUP BY range     ORDER BY range"
timescaleGroupQuery = "SELECT timescale           FROM dr_scenario  WHERE timescale_group = ? GROUP BY timescale ORDER BY timescale"

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

scenarioQuery, scenariosQuery, allScenariosQuery :: Query
scenarioQuery      = "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario WHERE s.id = ?"
scenariosQuery     = "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario WHERE s.id IN ?"
allScenariosQuery  = "SELECT s.id, sd.title, sd.subtitle, s.year_from, s.year_upto FROM dr_scenario AS s JOIN dr_scenario_data AS sd ON s.id = sd.id_scenario ORDER BY sd.title, sd.subtitle"

latitudeQuery, longitudeQuery, fromYearQuery, upToYearQuery, fromRangeQuery, upToRangeQuery :: Query
latitudeQuery      = "SELECT latitude_trunc  FROM dr_scenario WHERE latitude_trunc = ?"
longitudeQuery     = "SELECT longitude_trunc FROM dr_scenario WHERE longitude_trunc = ?"
fromYearQuery      = "SELECT min(year_from)  FROM dr_scenario WHERE year_from >= ?"
upToYearQuery      = "SELECT max(year_upto)  FROM dr_scenario WHERE year_upto <= ?"
fromRangeQuery     = "SELECT min(range)      FROM dr_scenario WHERE range >= ?"
upToRangeQuery     = "SELECT max(range)      FROM dr_scenario WHERE range <= ?"

latitudesQuery, longitudesQuery, fromYearsQuery, upToYearsQuery, rangesQuery, timescalesQuery :: Query
latitudesQuery     = "SELECT latitude_trunc  FROM dr_scenario WHERE id IN ? GROUP BY latitude_trunc  ORDER BY latitude_trunc"
longitudesQuery    = "SELECT longitude_trunc FROM dr_scenario WHERE id IN ? GROUP BY longitude_trunc ORDER BY longitude_trunc"
fromYearsQuery     = "SELECT year_from       FROM dr_scenario WHERE id IN ? GROUP BY year_from       ORDER BY year_from"
upToYearsQuery     = "SELECT year_upto       FROM dr_scenario WHERE id IN ? GROUP BY year_upto       ORDER BY year_upto"
rangesQuery        = "SELECT range           FROM dr_scenario WHERE id IN ? GROUP BY range           ORDER BY range"
timescalesQuery    = "SELECT timescale       FROM dr_scenario WHERE id IN ? GROUP BY timescale       ORDER BY timescale"

allLatitudesQuery, allLongitudesQuery, allFromYearsQuery, allUpToYearsQuery, allRangesQuery, allTimescaleQuery :: Query
allLatitudesQuery  = "SELECT latitude_trunc  FROM dr_scenario GROUP BY latitude_trunc  ORDER BY latitude_trunc"
allLongitudesQuery = "SELECT longitude_trunc FROM dr_scenario GROUP BY longitude_trunc ORDER BY longitude_trunc"
allFromYearsQuery  = "SELECT year_from       FROM dr_scenario GROUP BY year_from       ORDER BY year_from"
allUpToYearsQuery  = "SELECT year_upto       FROM dr_scenario GROUP BY year_upto       ORDER BY year_upto"
allRangesQuery     = "SELECT range           FROM dr_scenario GROUP BY range           ORDER BY range"
allTimescaleQuery  = "SELECT timescale       FROM dr_scenario GROUP BY timescale       ORDER BY timescale"

scenarioListQuery :: JoinMap -> [Parameter] -> Query
scenarioListQuery joinMap pList = foldl' mappend prefix parts
             where prefix = "SELECT s.id FROM dr_scenario AS s"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, value) = (j, w)
                                        where (j, w1, w2) = (joinMap !) key
                                              w = if value >= 0 then w1 else w2

initJoinMap :: JoinMap
initJoinMap = fromList $ prepareList parameterList joinList whereIncludeList whereExcludeList

prepareList :: [Text] -> JoinComponentMap -> JoinComponentMap -> JoinComponentMap -> [(Text, (Query, Query, Query))]
prepareList (p:ps) j w1 w2 = (p, (j ! p, w1 ! p, w2 ! p)) : prepareList ps j w1 w2
prepareList _ _ _ _        = []

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

joinList, whereIncludeList, whereExcludeList :: JoinComponentMap
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
