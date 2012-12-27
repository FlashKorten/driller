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

import Driller.Data ( JoinMap )
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
authorsQuery       = "SELECT d.id, d.author    FROM dr_author AS d    JOIN dr_map_author AS m    ON m.id_author = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.author    ORDER BY d.author"
genresQuery        = "SELECT d.id, d.genre     FROM dr_genre AS d     JOIN dr_map_genre AS m     ON m.id_genre = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.genre     ORDER BY d.genre"
enginesQuery       = "SELECT d.id, d.engine    FROM dr_engine AS d    JOIN dr_map_engine AS m    ON m.id_engine = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.engine    ORDER BY d.engine"
themesQuery        = "SELECT d.id, d.theme     FROM dr_theme AS d     JOIN dr_map_theme AS m     ON m.id_theme = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.theme     ORDER BY d.theme"
mechanicsQuery     = "SELECT d.id, d.mechanic  FROM dr_mechanic AS d  JOIN dr_map_mechanic AS m  ON m.id_mechanic = d.id  WHERE m.id_game IN ? GROUP BY d.id, d.mechanic  ORDER BY d.mechanic"
sidesQuery         = "SELECT d.id, d.side      FROM dr_side AS d      JOIN dr_map_side AS m      ON m.id_side = d.id      WHERE m.id_game IN ? GROUP BY d.id, d.side      ORDER BY d.side"
partiesQuery       = "SELECT d.id, d.party     FROM dr_party AS d     JOIN dr_map_party AS m     ON m.id_party = d.id     WHERE m.id_game IN ? GROUP BY d.id, d.party     ORDER BY d.party"
publishersQuery    = "SELECT d.id, d.publisher FROM dr_publisher AS d JOIN dr_map_publisher AS m ON m.id_publisher = d.id WHERE m.id_game IN ? GROUP BY d.id, d.publisher ORDER BY d.publisher"
seriessQuery       = "SELECT d.id, d.series    FROM dr_series AS d    JOIN dr_map_series AS m    ON m.id_series = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.series    ORDER BY d.series"
leadersQuery       = "SELECT d.id, d.leader    FROM dr_leader AS d    JOIN dr_map_leader AS m    ON m.id_leader = d.id    WHERE m.id_game IN ? GROUP BY d.id, d.leader    ORDER BY d.leader"

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
gameQuery          = "SELECT id, title, subtitle, players_min, players_max, id_bgg FROM dr_game, dr_game_data WHERE id_game = id AND id = ?"
gamesQuery         = "SELECT id, title, subtitle, players_min, players_max, id_bgg FROM dr_game, dr_game_data WHERE id_game = id AND id IN ?"
allGamesQuery      = "SELECT id, title, subtitle, players_min, players_max, id_bgg FROM dr_game, dr_game_data WHERE id_game = id ORDER BY title"

latitudeQuery, longitudeQuery, fromYearQuery, upToYearQuery, fromRangeQuery, upToRangeQuery :: Query
latitudeQuery      = "SELECT latitude_trunc  FROM dr_game WHERE latitude_trunc = ?"
longitudeQuery     = "SELECT longitude_trunc FROM dr_game WHERE longitude_trunc = ?"
fromYearQuery      = "SELECT min(year_from)  FROM dr_game WHERE year_from >= ?"
upToYearQuery      = "SELECT max(year_upto)  FROM dr_game WHERE year_upto <= ?"
fromRangeQuery     = "SELECT min(range)      FROM dr_game WHERE range >= ?"
upToRangeQuery     = "SELECT max(range)      FROM dr_game WHERE range <= ?"

latitudesQuery, longitudesQuery, fromYearsQuery, upToYearsQuery, fromRangesQuery, upToRangesQuery :: Query
latitudesQuery     = "SELECT latitude_trunc  FROM dr_game WHERE id IN ? GROUP BY latitude_trunc  ORDER BY latitude_trunc"
longitudesQuery    = "SELECT longitude_trunc FROM dr_game WHERE id IN ? GROUP BY longitude_trunc ORDER BY longitude_trunc"
fromYearsQuery     = "SELECT year_from       FROM dr_game WHERE id IN ? GROUP BY year_from       ORDER BY year_from"
upToYearsQuery     = "SELECT year_upto       FROM dr_game WHERE id IN ? GROUP BY year_upto       ORDER BY year_upto"
fromRangesQuery    = "SELECT range           FROM dr_game WHERE id IN ? GROUP BY range           ORDER BY range"
upToRangesQuery    = "SELECT range           FROM dr_game WHERE id IN ? GROUP BY range           ORDER BY range"

allLatitudesQuery, allLongitudesQuery, allFromYearsQuery, allUpToYearsQuery, allFromRangesQuery, allUpToRangesQuery :: Query
allLatitudesQuery  = "SELECT latitude_trunc  FROM dr_game GROUP BY latitude_trunc  ORDER BY latitude_trunc"
allLongitudesQuery = "SELECT longitude_trunc FROM dr_game GROUP BY longitude_trunc ORDER BY longitude_trunc"
allFromYearsQuery  = "SELECT year_from       FROM dr_game GROUP BY year_from       ORDER BY year_from"
allUpToYearsQuery  = "SELECT year_upto       FROM dr_game GROUP BY year_upto       ORDER BY year_upto"
allFromRangesQuery = "SELECT range           FROM dr_game GROUP BY range           ORDER BY range"
allUpToRangesQuery = "SELECT range           FROM dr_game GROUP BY range           ORDER BY range"

gameListQuery :: JoinMap -> [Text] -> Query
gameListQuery joinMap pList = foldl' mappend prefix parts
             where prefix = "SELECT id FROM dr_game AS g"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList $ " WHERE 1=1":wheres)
                   (joins, wheres) = unzip $ map (joinMap HM.!) pList

initJoinMap :: JoinMap
initJoinMap = HM.fromList $ prepareList parameterList joinList whereList

prepareList :: [Text] -> [Query] -> [Query] -> [(Text, (Query, Query))]
prepareList (p:ps) (j:js) (w:ws) = (p, (j, w)) : prepareList ps js ws
prepareList _ _ _                = []

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
                , "latitude"
                , "longitude"
                , "fromYear"
                , "upToYear"
                , "fromRange"
                , "upToRange"
                ]

joinList, whereList :: [Query]
joinList = [ " JOIN dr_map_author    AS author    ON g.id = author.id_game"
           , " JOIN dr_map_publisher AS publisher ON g.id = publisher.id_game"
           , " JOIN dr_map_theme     AS theme     ON g.id = theme.id_game"
           , " JOIN dr_map_genre     AS genre     ON g.id = genre.id_game"
           , " JOIN dr_map_mechanic  AS mechanic  ON g.id = mechanic.id_game"
           , " JOIN dr_map_side      AS side      ON g.id = side.id_game"
           , " JOIN dr_map_party     AS party     ON g.id = party.id_game"
           , " JOIN dr_map_series    AS series    ON g.id = series.id_game"
           , " JOIN dr_map_leader    AS leader    ON g.id = leader.id_game"
           , " JOIN dr_map_engine    AS engine    ON g.id = engine.id_game"
           , ""
           , ""
           , ""
           , ""
           , ""
           , ""
           ]

whereList = [ " AND author.id_author       = ?"
            , " AND publisher.id_publisher = ?"
            , " AND theme.id_theme         = ?"
            , " AND genre.id_genre         = ?"
            , " AND mechanic.id_mechanic   = ?"
            , " AND side.id_side           = ?"
            , " AND party.id_party         = ?"
            , " AND series.id_series       = ?"
            , " AND leader.id_leader       = ?"
            , " AND engine.id_engine       = ?"
            , " AND g.latitude_trunc       = ?"
            , " AND g.longitude_trunc      = ?"
            , " AND NOT g.year_upto        < ?"
            , " AND NOT g.year_from        > ?"
            , " AND g.range               >= ?"
            , " AND g.range               <= ?"
            ]
