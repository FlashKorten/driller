{-# LANGUAGE OverloadedStrings #-}
module Driller.Queries where

import Database.PostgreSQL.Simple (Query)
import Data.Monoid (mappend)
import qualified Data.Text as Text

authorQuery, authorsQuery, allAuthorsQuery :: Query
authorQuery        = "SELECT id, author FROM nn_author WHERE id = ?"
authorsQuery       = "SELECT d.id, d.author FROM nn_author AS d JOIN nn_map_author AS m ON m.id_author = d.id WHERE m.id_game IN ?"
allAuthorsQuery    = "SELECT id, author FROM nn_author ORDER BY author"
genreQuery, genresQuery, allGenresQuery :: Query
genreQuery         = "SELECT id, genre FROM nn_genre WHERE id = ?"
genresQuery        = "SELECT d.id, d.genre FROM nn_genre AS d JOIN nn_map_genre AS m ON m.id_genre = d.id WHERE m.id_game IN ?"
allGenresQuery     = "SELECT id, genre FROM nn_genre ORDER BY genre"
engineQuery, enginesQuery, allEnginesQuery :: Query
engineQuery        = "SELECT id, engine FROM nn_engine WHERE id = ?"
enginesQuery       = "SELECT d.id, d.engine FROM nn_engine AS d JOIN nn_map_engine AS m ON m.id_engine = d.id WHERE m.id_game IN ?"
allEnginesQuery    = "SELECT id, engine FROM nn_engine ORDER BY engine"
themeQuery, themesQuery, allThemesQuery :: Query
themeQuery         = "SELECT id, theme FROM nn_theme WHERE id = ?"
themesQuery        = "SELECT d.id, d.theme FROM nn_theme AS d JOIN nn_map_theme AS m ON m.id_theme = d.id WHERE m.id_game IN ?"
allThemesQuery     = "SELECT id, theme FROM nn_theme ORDER BY theme"
mechanicQuery, mechanicsQuery, allMechanicsQuery :: Query
mechanicQuery      = "SELECT id, mechanic FROM nn_mechanic WHERE id = ?"
mechanicsQuery     = "SELECT d.id, d.mechanic FROM nn_mechanic AS d JOIN nn_map_mechanic AS m ON m.id_mechanic = d.id WHERE m.id_game IN ?"
allMechanicsQuery  = "SELECT id, mechanic FROM nn_mechanic ORDER BY mechanic"
sideQuery, sidesQuery, allSidesQuery :: Query
sideQuery          = "SELECT id, side FROM nn_side WHERE id = ?"
sidesQuery         = "SELECT d.id, d.side FROM nn_side AS d JOIN nn_map_side AS m ON m.id_side = d.id WHERE m.id_game IN ?"
allSidesQuery      = "SELECT id, side FROM nn_side ORDER BY side"
partyQuery, partiesQuery, allPartiesQuery :: Query
partyQuery         = "SELECT id, party FROM nn_party WHERE id = ?"
partiesQuery       = "SELECT d.id, d.party FROM nn_party AS d JOIN nn_map_party AS m ON m.id_party = d.id WHERE m.id_game IN ?"
allPartiesQuery    = "SELECT id, party FROM nn_party ORDER BY party"
publisherQuery, publishersQuery, allPublishersQuery :: Query
publisherQuery     = "SELECT id, publisher FROM nn_publisher WHERE id = ?"
publishersQuery    = "SELECT d.id, d.publisher FROM nn_publisher AS d JOIN nn_map_publisher AS m ON m.id_publisher = d.id WHERE m.id_game IN ?"
allPublishersQuery = "SELECT id, publisher FROM nn_publisher ORDER BY publisher"
areaQuery, areasQuery, allAreasQuery :: Query
areaQuery          = "SELECT id, area FROM nn_area WHERE id = ?"
areasQuery         = "SELECT d.id, d.area FROM nn_area AS d JOIN nn_map_area AS m ON m.id_area = d.id WHERE m.id_game IN ?"
allAreasQuery      = "SELECT id, area FROM nn_area ORDER BY area"
gameQuery, gamesQuery, allGamesQuery :: Query
gameQuery          = "SELECT id, game, subtitle, num_players_min, num_players_max, gametime_start, gametime_end, id_bgg FROM nn_game WHERE id = ?"
gamesQuery         = "SELECT d.id, d.game, d. subtitle, d.num_players_min, d.num_players_max, d.gametime_start, d.gametime_end, d.id_bgg FROM nn_game AS d JOIN nn_map_game AS m ON m.id_game = d.id WHERE m.id_game IN ?"
allGamesQuery      = "SELECT id, game, subtitle, num_players_min, num_players_max, gametime_start, gametime_end, id_bgg FROM nn_game ORDER BY game"

gameListQuery :: [Text.Text] -> Query
gameListQuery pList = foldl mappend prefix parts
            where prefix = "SELECT id FROM nn_game where 1=1 "
                  parts = undefined -- map over map of (params, sqlpart)

