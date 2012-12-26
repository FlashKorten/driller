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
    , gameListQuery
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
    ) where

import Driller.Data ( JoinMap, Parameter )
import Database.PostgreSQL.Simple ( Query )
import Data.Monoid ( mappend )
import Data.List ( foldl' )
import Data.Text ()
import qualified Data.DList as DL ( toList, fromList, append )
import qualified Data.HashMap.Strict as HM ( fromList, (!) )

authorQuery, genreQuery, engineQuery, themeQuery, mechanicQuery, sideQuery,
 partyQuery, publisherQuery, seriesQuery, leaderQuery :: Query
authorQuery        = "SELECT id, author    FROM nn_author    WHERE id = ?"
genreQuery         = "SELECT id, genre     FROM nn_genre     WHERE id = ?"
engineQuery        = "SELECT id, engine    FROM nn_engine    WHERE id = ?"
themeQuery         = "SELECT id, theme     FROM nn_theme     WHERE id = ?"
mechanicQuery      = "SELECT id, mechanic  FROM nn_mechanic  WHERE id = ?"
sideQuery          = "SELECT id, side      FROM nn_side      WHERE id = ?"
partyQuery         = "SELECT id, party     FROM nn_party     WHERE id = ?"
publisherQuery     = "SELECT id, publisher FROM nn_publisher WHERE id = ?"
seriesQuery        = "SELECT id, series    FROM nn_series    WHERE id = ?"
leaderQuery        = "SELECT id, leader    FROM nn_leader    WHERE id = ?"

authorsQuery, genresQuery, enginesQuery, themesQuery, mechanicsQuery, sidesQuery,
 partiesQuery, publishersQuery, seriessQuery, leadersQuery :: Query
authorsQuery       = "SELECT d.id, d.author    FROM nn_author AS d    JOIN nn_map_author AS m    ON m.id_author = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.author    ORDER BY d.author"
genresQuery        = "SELECT d.id, d.genre     FROM nn_genre AS d     JOIN nn_map_genre AS m     ON m.id_genre = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.genre     ORDER BY d.genre"
enginesQuery       = "SELECT d.id, d.engine    FROM nn_engine AS d    JOIN nn_map_engine AS m    ON m.id_engine = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.engine    ORDER BY d.engine"
themesQuery        = "SELECT d.id, d.theme     FROM nn_theme AS d     JOIN nn_map_theme AS m     ON m.id_theme = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.theme     ORDER BY d.theme"
mechanicsQuery     = "SELECT d.id, d.mechanic  FROM nn_mechanic AS d  JOIN nn_map_mechanic AS m  ON m.id_mechanic = d.id  WHERE m.id_game IN ? GROUP BY d.id, d.mechanic  ORDER BY d.mechanic"
sidesQuery         = "SELECT d.id, d.side      FROM nn_side AS d      JOIN nn_map_side AS m      ON m.id_side = d.id      WHERE m.id_game IN ? GROUP BY d.id, d.side      ORDER BY d.side"
partiesQuery       = "SELECT d.id, d.party     FROM nn_party AS d     JOIN nn_map_party AS m     ON m.id_party = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.party     ORDER BY d.party"
publishersQuery    = "SELECT d.id, d.publisher FROM nn_publisher AS d JOIN nn_map_publisher AS m ON m.id_publisher = d.id WHERE m.id_game IN ? GROUP BY d.id, d.publisher ORDER BY d.publisher"
seriessQuery       = "SELECT d.id, d.series    FROM nn_series AS d    JOIN nn_map_series AS m    ON m.id_series = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.series    ORDER BY d.series"
leadersQuery       = "SELECT d.id, d.leader    FROM nn_leader AS d    JOIN nn_map_leader AS m    ON m.id_leader = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.leader    ORDER BY d.leader"

allAuthorsQuery, allGenresQuery, allEnginesQuery, allThemesQuery, allMechanicsQuery, allSidesQuery,
 allPartiesQuery, allPublishersQuery, allSeriesQuery, allLeadersQuery :: Query
allAuthorsQuery    = "SELECT id, author    FROM nn_author    ORDER BY author"
allGenresQuery     = "SELECT id, genre     FROM nn_genre     ORDER BY genre"
allEnginesQuery    = "SELECT id, engine    FROM nn_engine    ORDER BY engine"
allThemesQuery     = "SELECT id, theme     FROM nn_theme     ORDER BY theme"
allMechanicsQuery  = "SELECT id, mechanic  FROM nn_mechanic  ORDER BY mechanic"
allSidesQuery      = "SELECT id, side      FROM nn_side      ORDER BY side"
allPartiesQuery    = "SELECT id, party     FROM nn_party     ORDER BY party"
allPublishersQuery = "SELECT id, publisher FROM nn_publisher ORDER BY publisher"
allSeriesQuery     = "SELECT id, series    FROM nn_series    ORDER BY series"
allLeadersQuery    = "SELECT id, leader    FROM nn_leader    ORDER BY leader"

gameQuery, gamesQuery, allGamesQuery :: Query
gameQuery          = "SELECT id, game, subtitle, players_min, players_max, id_bgg FROM nn_game WHERE id = ?"
gamesQuery         = "SELECT id, game, subtitle, players_min, players_max, id_bgg FROM nn_game WHERE id IN ?"
allGamesQuery      = "SELECT id, game, subtitle, players_min, players_max, id_bgg FROM nn_game ORDER BY game"

latitudeQuery, longitudeQuery, fromYearQuery, upToYearQuery, fromRangeQuery, upToRangeQuery :: Query
latitudeQuery      = "SELECT latitude_trunc  FROM nn_game WHERE latitude_trunc = ?"
longitudeQuery     = "SELECT longitude_trunc FROM nn_game WHERE longitude_trunc = ?"
fromYearQuery      = "SELECT min(year_from)  FROM nn_game WHERE year_from >= ?"
upToYearQuery      = "SELECT max(year_upto)  FROM nn_game WHERE year_upto <= ?"
fromRangeQuery     = "SELECT min(range)      FROM nn_game WHERE range >= ?"
upToRangeQuery     = "SELECT max(range)      FROM nn_game WHERE range <= ?"

latitudesQuery, longitudesQuery, fromYearsQuery, upToYearsQuery, fromRangesQuery, upToRangesQuery :: Query
latitudesQuery     = "SELECT latitude_trunc  FROM nn_game WHERE id IN ? GROUP BY latitude_trunc  ORDER BY latitude_trunc"
longitudesQuery    = "SELECT longitude_trunc FROM nn_game WHERE id IN ? GROUP BY longitude_trunc ORDER BY longitude_trunc"
fromYearsQuery     = "SELECT year_from       FROM nn_game WHERE id IN ? GROUP BY year_from       ORDER BY year_from"
upToYearsQuery     = "SELECT year_upto       FROM nn_game WHERE id IN ? GROUP BY year_upto       ORDER BY year_upto"
fromRangesQuery    = "SELECT range           FROM nn_game WHERE id IN ? GROUP BY range           ORDER BY range"
upToRangesQuery    = "SELECT range           FROM nn_game WHERE id IN ? GROUP BY range           ORDER BY range"

allLatitudesQuery, allLongitudesQuery, allFromYearsQuery, allUpToYearsQuery, allFromRangesQuery, allUpToRangesQuery :: Query
allLatitudesQuery  = "SELECT latitude_trunc  FROM nn_game GROUP BY latitude_trunc  ORDER BY latitude_trunc"
allLongitudesQuery = "SELECT longitude_trunc FROM nn_game GROUP BY longitude_trunc ORDER BY longitude_trunc"
allFromYearsQuery  = "SELECT year_from       FROM nn_game GROUP BY year_from       ORDER BY year_from"
allUpToYearsQuery  = "SELECT year_upto       FROM nn_game GROUP BY year_upto       ORDER BY year_upto"
allFromRangesQuery = "SELECT range           FROM nn_game GROUP BY range           ORDER BY range"
allUpToRangesQuery = "SELECT range           FROM nn_game GROUP BY range           ORDER BY range"

gameListQuery :: JoinMap -> [Parameter] -> Query
gameListQuery joinMap pList = foldl' mappend prefix parts
             where prefix = "SELECT id FROM nn_game AS g"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (getParameterQuery joinMap) pList

getParameterQuery :: JoinMap -> Parameter -> (Query, Query)
getParameterQuery joinMap (key, value) = (j, w)
                                        where (j, w1, w2) = (joinMap HM.!) key
                                              w = if value >= 0 then w1 else w2

initJoinMap :: JoinMap
initJoinMap = HM.fromList [("author"
                            ,(" JOIN nn_map_author AS author ON g.id = author.id_game"
                             ," AND author.id_author = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_author WHERE id_game = g.id AND id_author = abs(?))"))
                           ,("publisher"
                            ,(" JOIN nn_map_publisher AS publisher ON g.id = publisher.id_game"
                             ," AND publisher.id_publisher = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_publisher WHERE id_game = g.id AND id_publisher = abs(?))"))
                           ,("theme"
                            ,(" JOIN nn_map_theme AS theme ON g.id = theme.id_game"
                             ," AND theme.id_theme = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_theme WHERE id_game = g.id AND id_theme = abs(?))"))
                           ,("genre"
                            ,(" JOIN nn_map_genre AS genre ON g.id = genre.id_game"
                             ," AND genre.id_genre = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_genre WHERE id_game = g.id AND id_genre = abs(?))"))
                           ,("mechanic"
                            ,(" JOIN nn_map_mechanic AS mechanic ON g.id = mechanic.id_game"
                             ," AND mechanic.id_mechanic = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_mechanic WHERE id_game = g.id AND id_mechanic = abs(?))"))
                           ,("side"
                            ,(" JOIN nn_map_side AS side ON g.id = side.id_game"
                             ," AND side.id_side = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_side WHERE id_game = g.id AND id_side = abs(?))"))
                           ,("party"
                            ,(" JOIN nn_map_party AS party ON g.id = party.id_game"
                             ," AND party.id_party = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_party WHERE id_game = g.id AND id_party = abs(?))"))
                           ,("series"
                            ,(" JOIN nn_map_series AS series ON g.id = series.id_game"
                             ," AND series.id_series = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_series WHERE id_game = g.id AND id_series = abs(?))"))
                           ,("leader"
                            ,(" JOIN nn_map_leader AS leader ON g.id = leader.id_game"
                             ," AND leader.id_leader = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_leader WHERE id_game = g.id AND id_leader = abs(?))"))
                           ,("engine"
                            ,(" JOIN nn_map_engine AS engine ON g.id = engine.id_game"
                             ," AND engine.id_engine = ?"
                             ," AND NOT EXISTS (SELECT id_game FROM nn_map_engine WHERE id_game = g.id AND id_engine = abs(?))"))
                           ,("latitude"  ,("", " AND g.latitude_trunc = ?",  " AND g.latitude_trunc = ?"))
                           ,("longitude" ,("", " AND g.longitude_trunc = ?", " AND g.longitude_trunc = ?"))
                           ,("fromYear"  ,("", " AND NOT g.year_upto < ?",   " AND NOT g.year_upto < ?"))
                           ,("upToYear"  ,("", " AND NOT g.year_from > ?",   " AND NOT g.year_from > ?"))
                           ,("fromRange" ,("", " AND g.range >= ?",          " AND g.range >= ?"))
                           ,("upToRange" ,("", " AND g.range <= ?",          " AND g.range <= ?"))
                           ]

