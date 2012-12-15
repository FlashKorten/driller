{-# LANGUAGE OverloadedStrings #-}
module Driller.Queries where

import Database.PostgreSQL.Simple (Query)
import Data.Monoid (mappend)
import qualified Data.Text.Lazy.Internal as TL
import Data.List (foldl')
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM

type QueryMap = HM.HashMap TL.Text Query
type JoinMap = HM.HashMap TL.Text (Query, Query)
data QueryType = Mono | Poly | Omni deriving Show

buildQuery :: QueryType -> Query -> (Query, [Query])
buildQuery Mono field = ("SELECT id, ", [ field
                                        , " FROM nn_"
                                        , field
                                        , " WHERE id = ?"
                                        ])
buildQuery Poly field = ("SELECT d.id, d.", [ field
                                            , "FROM nn_"
                                            , field
                                            , " AS d JOIN nn_map_"
                                            , field
                                            , " AS m ON m.id_"
                                            , field
                                            , " = d.id WHERE m.id_game IN ? GROUP BY d.id, d."
                                            , field
                                            , " ORDER BY d."
                                            , field
                                            ])
buildQuery Omni field = ("SELECT id, ", [ field
                                        , " FROM nn_"
                                        , field
                                        , " ORDER BY "
                                        , field])

query :: QueryType -> Query -> Query
query qType field = foldl' mappend prefix rest
                         where (prefix, rest) = buildQuery qType field

initQueryMap :: QueryMap
initQueryMap = HM.fromList [(read $ show f ++ show qT, query qT f) | f <- fieldList, qT <- [Mono, Poly, Omni]]

fieldList :: [Query]
fieldList = ["author"
            ,"genre"
            ,"engine"
            ,"theme"
            ,"mechanic"
            ,"side"
            ,"party"
            ,"publisher"
            ,"area"
            ]

authorQuery, authorsQuery, allAuthorsQuery :: Query
authorQuery        = "SELECT id, author FROM nn_author WHERE id = ?"
authorsQuery       = "SELECT d.id, d.author FROM nn_author AS d JOIN nn_map_author AS m ON m.id_author = d.id WHERE m.id_game IN ? GROUP BY d.id, d.author ORDER BY d.author"
allAuthorsQuery    = "SELECT id, author FROM nn_author ORDER BY author"
genreQuery, genresQuery, allGenresQuery :: Query
genreQuery         = "SELECT id, genre FROM nn_genre WHERE id = ?"
genresQuery        = "SELECT d.id, d.genre FROM nn_genre AS d JOIN nn_map_genre AS m ON m.id_genre = d.id WHERE m.id_game IN ? GROUP BY d.id, d.genre ORDER BY d.genre"
allGenresQuery     = "SELECT id, genre FROM nn_genre ORDER BY genre"
engineQuery, enginesQuery, allEnginesQuery :: Query
engineQuery        = "SELECT id, engine FROM nn_engine WHERE id = ?"
enginesQuery       = "SELECT d.id, d.engine FROM nn_engine AS d JOIN nn_map_engine AS m ON m.id_engine = d.id WHERE m.id_game IN ? GROUP BY d.id, d.engine ORDER BY d.engine"
allEnginesQuery    = "SELECT id, engine FROM nn_engine ORDER BY engine"
themeQuery, themesQuery, allThemesQuery :: Query
themeQuery         = "SELECT id, theme FROM nn_theme WHERE id = ?"
themesQuery        = "SELECT d.id, d.theme FROM nn_theme AS d JOIN nn_map_theme AS m ON m.id_theme = d.id WHERE m.id_game IN ? GROUP BY d.id, d.theme ORDER BY d.theme"
allThemesQuery     = "SELECT id, theme FROM nn_theme ORDER BY theme"
mechanicQuery, mechanicsQuery, allMechanicsQuery :: Query
mechanicQuery      = "SELECT id, mechanic FROM nn_mechanic WHERE id = ?"
mechanicsQuery     = "SELECT d.id, d.mechanic FROM nn_mechanic AS d JOIN nn_map_mechanic AS m ON m.id_mechanic = d.id WHERE m.id_game IN ? GROUP BY d.id, d.mechanic ORDER BY d.mechanic"
allMechanicsQuery  = "SELECT id, mechanic FROM nn_mechanic ORDER BY mechanic"
sideQuery, sidesQuery, allSidesQuery :: Query
sideQuery          = "SELECT id, side FROM nn_side WHERE id = ?"
sidesQuery         = "SELECT d.id, d.side FROM nn_side AS d JOIN nn_map_side AS m ON m.id_side = d.id WHERE m.id_game IN ? GROUP BY d.id, d.side ORDER BY d.side"
allSidesQuery      = "SELECT id, side FROM nn_side ORDER BY side"
partyQuery, partiesQuery, allPartiesQuery :: Query
partyQuery         = "SELECT id, party FROM nn_party WHERE id = ?"
partiesQuery       = "SELECT d.id, d.party FROM nn_party AS d JOIN nn_map_party AS m ON m.id_party = d.id WHERE m.id_game IN ? GROUP BY d.id, d.party ORDER BY d.party"
allPartiesQuery    = "SELECT id, party FROM nn_party ORDER BY party"
publisherQuery, publishersQuery, allPublishersQuery :: Query
publisherQuery     = "SELECT id, publisher FROM nn_publisher WHERE id = ?"
publishersQuery    = "SELECT d.id, d.publisher FROM nn_publisher AS d JOIN nn_map_publisher AS m ON m.id_publisher = d.id WHERE m.id_game IN ? GROUP BY d.id, d.publisher ORDER BY d.publisher"
allPublishersQuery = "SELECT id, publisher FROM nn_publisher ORDER BY publisher"
areaQuery, areasQuery, allAreasQuery :: Query
areaQuery          = "SELECT id, area FROM nn_area WHERE id = ?"
areasQuery         = "SELECT d.id, d.area FROM nn_area AS d JOIN nn_map_area AS m ON m.id_area = d.id WHERE m.id_game IN ? GROUP BY d.id, d.area ORDER BY d.area"
allAreasQuery      = "SELECT id, area FROM nn_area ORDER BY area"
gameQuery, gamesQuery, allGamesQuery :: Query
gameQuery          = "SELECT id, game, subtitle, num_players_min, num_players_max, gametime_start, gametime_end, id_bgg FROM nn_game WHERE id = ?"
gamesQuery         = "SELECT id, game, subtitle, num_players_min, num_players_max, gametime_start, gametime_end, id_bgg FROM nn_game WHERE id IN ?"
allGamesQuery      = "SELECT id, game, subtitle, num_players_min, num_players_max, gametime_start, gametime_end, id_bgg FROM nn_game ORDER BY game"

gameListQuery :: JoinMap -> [TL.Text] -> Query
gameListQuery joinMap pList = foldl' mappend prefix parts
             where prefix = "SELECT id FROM nn_game AS g"
                   parts = DL.toList $ DL.append (DL.fromList joins) (DL.fromList wheres)
                   (joins, wheres) = unzip $ map (joinMap HM.!) pList

initJoinMap :: JoinMap
initJoinMap = HM.fromList [("author"    , (" JOIN nn_map_author AS author ON g.id = author.id_game",          " AND author.id_author = ?"))
                          ,("publisher" , (" JOIN nn_map_publisher AS publisher ON g.id = publisher.id_game", " AND publisher.id_publisher = ?"))
                          ,("theme"     , (" JOIN nn_map_theme AS theme ON g.id = theme.id_game",             " AND theme.id_theme = ?"))
                          ,("genre"     , (" JOIN nn_map_genre AS genre ON g.id = genre.id_game",             " AND genre.id_genre = ?"))
                          ,("mechanic"  , (" JOIN nn_map_mechanic AS mechanic ON g.id = mechanic.id_game",    " AND mechanic.id_mechanic = ?"))
                          ,("side"      , (" JOIN nn_map_side AS side ON g.id = side.id_game",                " AND side.id_side = ?"))
                          ,("party"     , (" JOIN nn_map_party AS party ON g.id = party.id_game",             " AND party.id_party = ?"))
                          ,("area"      , (" JOIN nn_map_area AS area ON g.id = area.id_game",                " AND area.id_area = ?"))
                          ,("engine"    , (" JOIN nn_map_engine AS engine ON g.id = engine.id_game",          " AND engine.id_engine = ?"))
                          ]

