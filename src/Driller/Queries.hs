{-# LANGUAGE OverloadedStrings #-}
module Driller.Queries where

import Database.PostgreSQL.Simple (Query)

authorQuery, authorsQuery, allAuthorsQuery :: Query
authorQuery        = "SELECT id, author FROM nn_author WHERE id = ?"
authorsQuery       = "SELECT t.id, t.author FROM nn_author AS t JOIN nn_map_author AS m ON m.id_author = t.id WHERE m.id_game IN ?"
allAuthorsQuery    = "SELECT id, author FROM nn_author ORDER BY author"
genreQuery, genresQuery, allGenresQuery :: Query
genreQuery         = "SELECT id, genre FROM nn_genre WHERE id = ?"
genresQuery        = "SELECT t.id, t.genre FROM nn_genre AS t JOIN nn_map_genre AS m ON m.id_genre = t.id WHERE m.id_game IN ?"
allGenresQuery     = "SELECT id, genre FROM nn_genre ORDER BY genre"
engineQuery, enginesQuery, allEnginesQuery :: Query
engineQuery        = "SELECT id, engine FROM nn_engine WHERE id = ?"
enginesQuery       = "SELECT t.id, t.engine FROM nn_engine AS t JOIN nn_map_engine AS m ON m.id_engine = t.id WHERE m.id_game IN ?"
allEnginesQuery    = "SELECT id, engine FROM nn_engine ORDER BY engine"
themeQuery, themesQuery, allThemesQuery :: Query
themeQuery         = "SELECT id, theme FROM nn_theme WHERE id = ?"
themesQuery        = "SELECT t.id, t.theme FROM nn_theme AS t JOIN nn_map_theme AS m ON m.id_theme = t.id WHERE m.id_game IN ?"
allThemesQuery     = "SELECT id, theme FROM nn_theme ORDER BY theme"
mechanicQuery, mechanicsQuery, allMechanicsQuery :: Query
mechanicQuery      = "SELECT id, mechanic FROM nn_mechanic WHERE id = ?"
mechanicsQuery     = "SELECT t.id, t.mechanic FROM nn_mechanic AS t JOIN nn_map_mechanic AS m ON m.id_mechanic = t.id WHERE m.id_game IN ?"
allMechanicsQuery  = "SELECT id, mechanic FROM nn_mechanic ORDER BY mechanic"
sideQuery, sidesQuery, allSidesQuery :: Query
sideQuery          = "SELECT id, side FROM nn_side WHERE id = ?"
sidesQuery         = "SELECT t.id, t.side FROM nn_side AS t JOIN nn_map_side AS m ON m.id_side = t.id WHERE m.id_game IN ?"
allSidesQuery      = "SELECT id, side FROM nn_side ORDER BY side"
partyQuery, partiesQuery, allPartiesQuery :: Query
partyQuery         = "SELECT id, party FROM nn_party WHERE id = ?"
partiesQuery       = "SELECT t.id, t.party FROM nn_party AS t JOIN nn_map_party AS m ON m.id_party = t.id WHERE m.id_game IN ?"
allPartiesQuery    = "SELECT id, party FROM nn_party ORDER BY party"
publisherQuery, publishersQuery, allPublishersQuery :: Query
publisherQuery     = "SELECT id, publisher FROM nn_publisher WHERE id = ?"
publishersQuery    = "SELECT t.id, t.publisher FROM nn_publisher AS t JOIN nn_map_publisher AS m ON m.id_publisher = t.id WHERE m.id_game IN ?"
allPublishersQuery = "SELECT id, publisher FROM nn_publisher ORDER BY publisher"
areaQuery, areasQuery, allAreasQuery :: Query
areaQuery          = "SELECT id, area FROM nn_area WHERE id = ?"
areasQuery         = "SELECT t.id, t.area FROM nn_area AS t JOIN nn_map_area AS m ON m.id_area = t.id WHERE m.id_game IN ?"
allAreasQuery      = "SELECT id, area FROM nn_area ORDER BY area"
