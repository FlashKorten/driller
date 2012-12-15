module Driller.DB
    ( fetchTheme
    , fetchSide
    , fetchPublisher
    , fetchParty
    , fetchMechanic
    , fetchGenre
    , fetchGame
    , fetchEngine
    , fetchDrilledGameResult
    , fetchAuthor
    , fetchArea
    , fetchAllThemes
    , fetchAllSides
    , fetchAllPublishers
    , fetchAllParties
    , fetchAllMechanics
    , fetchAllGenres
    , fetchAllGames
    , fetchAllEngines
    , fetchAllAuthors
    , fetchAllAreas
    , connectionInfo
    , initJoinMap
    ) where

import Driller.Data
import Driller.DB.Queries

import Web.Scotty ( Param )
import Database.PostgreSQL.Simple
    ( Only(Only),
      In(In),
      Connection,
      ConnectInfo(connectDatabase, connectPassword, connectUser),
      defaultConnectInfo,
      query_,
      query )

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser = "nemesis"
                                    , connectPassword = "nemesis"
                                    , connectDatabase = "nn"
                                    }

fetchAuthor :: Connection -> Int -> IO [Author]
fetchAuthor c = query c authorQuery

fetchAuthors :: Connection -> [Int] -> IO [Author]
fetchAuthors c ids = query c authorsQuery (Only (In ids))

fetchAllAuthors :: Connection -> IO [Author]
fetchAllAuthors c = query_ c allAuthorsQuery

fetchGenre :: Connection -> Int -> IO [Genre]
fetchGenre c = query c genreQuery

fetchGenres :: Connection -> [Int] -> IO [Genre]
fetchGenres c ids = query c genresQuery (Only (In ids))

fetchAllGenres :: Connection -> IO [Genre]
fetchAllGenres c = query_ c allGenresQuery

fetchEngine :: Connection -> Int -> IO [Engine]
fetchEngine c = query c engineQuery

fetchEngines :: Connection -> [Int] -> IO [Engine]
fetchEngines c ids = query c enginesQuery (Only (In ids))

fetchAllEngines :: Connection -> IO [Engine]
fetchAllEngines c = query_ c allEnginesQuery

fetchTheme :: Connection -> Int -> IO [Theme]
fetchTheme c = query c themeQuery

fetchThemes :: Connection -> [Int] -> IO [Theme]
fetchThemes c ids = query c themesQuery (Only (In ids))

fetchAllThemes :: Connection -> IO [Theme]
fetchAllThemes c = query_ c allThemesQuery

fetchMechanic :: Connection -> Int -> IO [Mechanic]
fetchMechanic c = query c mechanicQuery

fetchMechanics :: Connection -> [Int] -> IO [Mechanic]
fetchMechanics c ids = query c mechanicsQuery (Only (In ids))

fetchAllMechanics :: Connection -> IO [Mechanic]
fetchAllMechanics c = query_ c allMechanicsQuery

fetchSide :: Connection -> Int -> IO [Side]
fetchSide c = query c sideQuery

fetchSides :: Connection -> [Int] -> IO [Side]
fetchSides c ids = query c sidesQuery (Only (In ids))

fetchAllSides :: Connection -> IO [Side]
fetchAllSides c = query_ c allSidesQuery

fetchParty :: Connection -> Int -> IO [Party]
fetchParty c = query c partyQuery

fetchParties :: Connection -> [Int] -> IO [Party]
fetchParties c ids = query c partiesQuery (Only (In ids))

fetchAllParties :: Connection -> IO [Party]
fetchAllParties c = query_ c allPartiesQuery

fetchPublisher :: Connection -> Int -> IO [Publisher]
fetchPublisher c = query c publisherQuery

fetchPublishers :: Connection -> [Int] -> IO [Publisher]
fetchPublishers c ids = query c publishersQuery (Only (In ids))

fetchAllPublishers :: Connection -> IO [Publisher]
fetchAllPublishers c = query_ c allPublishersQuery

fetchArea :: Connection -> Int -> IO [Area]
fetchArea c = query c areaQuery

fetchAreas :: Connection -> [Int] -> IO [Area]
fetchAreas c ids = query c areasQuery (Only (In ids))

fetchAllAreas :: Connection -> IO [Area]
fetchAllAreas c = query_ c allAreasQuery

fetchGame :: Connection -> Int -> IO [Game]
fetchGame c = query c gameQuery

fetchGames :: Connection -> [Int] -> IO [Game]
fetchGames c ids = query c gamesQuery (Only (In ids))

fetchAllGames :: Connection -> IO [Game]
fetchAllGames c = query_ c allGamesQuery

fetchDrilledGameResult :: JoinMap -> Connection -> [Param] -> IO GameResult
fetchDrilledGameResult joinMap c p = do
    let (keys, values) = unzip p
        que = gameListQuery joinMap keys
    ids        <- query c que values
    games      <- fetchGames c ids
    genres     <- fetchGenres c ids
    themes     <- fetchThemes c ids
    mechanics  <- fetchMechanics c ids
    sides      <- fetchSides c ids
    parties    <- fetchParties c ids
    publishers <- fetchPublishers c ids
    areas      <- fetchAreas c ids
    authors    <- fetchAuthors c ids
    engines    <- fetchEngines c ids
    return GameResult { getGames      = games
                      , getGenres     = genres
                      , getThemes     = themes
                      , getMechanics  = mechanics
                      , getSides      = sides
                      , getParties    = parties
                      , getPublishers = publishers
                      , getAreas      = areas
                      , getAuthors    = authors
                      , getEngines    = engines
                      }
