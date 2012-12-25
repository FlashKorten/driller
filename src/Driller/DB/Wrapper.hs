module Driller.DB.Wrapper
    ( fetchAuthor, fetchAuthors, fetchAllAuthors
    , fetchGenre, fetchGenres, fetchAllGenres
    , fetchEngine, fetchEngines, fetchAllEngines
    , fetchTheme, fetchThemes, fetchAllThemes
    , fetchMechanic, fetchMechanics, fetchAllMechanics
    , fetchSide, fetchSides, fetchAllSides
    , fetchParty, fetchParties, fetchAllParties
    , fetchPublisher, fetchPublishers, fetchAllPublishers
    , fetchSeries, fetchSeriess, fetchAllSeries
    , fetchGame, fetchGames, fetchAllGames
    , fetchLeader, fetchLeaders, fetchAllLeaders
    , fetchLatitude, fetchLatitudes, fetchAllLatitudes
    , fetchLongitude, fetchLongitudes, fetchAllLongitudes
    , fetchFromYear, fetchFromYears, fetchAllFromYears
    , fetchUpToYear, fetchUpToYears, fetchAllUpToYears
    , fetchFromRange, fetchFromRanges, fetchAllFromRanges
    , fetchUpToRange, fetchUpToRanges, fetchAllUpToRanges
    ) where

import Driller.Data
import Driller.DB.Queries
import Database.PostgreSQL.Simple
    ( Only(Only)
    , In(In)
    , Connection
    , query_
    , query
    )

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

fetchSeries :: Connection -> Int -> IO [Series]
fetchSeries c = query c seriesQuery

fetchSeriess :: Connection -> [Int] -> IO [Series]
fetchSeriess c ids = query c seriessQuery (Only (In ids))

fetchAllSeries :: Connection -> IO [Series]
fetchAllSeries c = query_ c allSeriesQuery

fetchGame :: Connection -> Int -> IO [Game]
fetchGame c = query c gameQuery

fetchGames :: Connection -> [Int] -> IO [Game]
fetchGames c ids = query c gamesQuery (Only (In ids))

fetchAllGames :: Connection -> IO [Game]
fetchAllGames c = query_ c allGamesQuery

fetchLeader :: Connection -> Int -> IO [Leader]
fetchLeader c = query c leaderQuery

fetchLeaders :: Connection -> [Int] -> IO [Leader]
fetchLeaders c ids = query c leadersQuery (Only (In ids))

fetchAllLeaders :: Connection -> IO [Leader]
fetchAllLeaders c = query_ c allLeadersQuery

fetchLatitude :: Connection -> Int -> IO [Latitude]
fetchLatitude c = query c latitudeQuery

fetchLatitudes :: Connection -> [Int] -> IO [Latitude]
fetchLatitudes c ids = query c latitudesQuery (Only (In ids))

fetchAllLatitudes :: Connection -> IO [Latitude]
fetchAllLatitudes c = query_ c allLatitudesQuery

fetchLongitude :: Connection -> Int -> IO [Longitude]
fetchLongitude c = query c longitudeQuery

fetchLongitudes :: Connection -> [Int] -> IO [Longitude]
fetchLongitudes c ids = query c longitudesQuery (Only (In ids))

fetchAllLongitudes :: Connection -> IO [Longitude]
fetchAllLongitudes c = query_ c allLongitudesQuery

fetchFromYear :: Connection -> Int -> IO [FromYear]
fetchFromYear c = query c fromYearQuery

fetchFromYears :: Connection -> [Int] -> IO [FromYear]
fetchFromYears c ids = query c fromYearsQuery (Only (In ids))

fetchAllFromYears :: Connection -> IO [FromYear]
fetchAllFromYears c = query_ c allFromYearsQuery

fetchUpToYear :: Connection -> Int -> IO [UpToYear]
fetchUpToYear c = query c upToYearQuery

fetchUpToYears :: Connection -> [Int] -> IO [UpToYear]
fetchUpToYears c ids = query c upToYearsQuery (Only (In ids))

fetchAllUpToYears :: Connection -> IO [UpToYear]
fetchAllUpToYears c = query_ c allUpToYearsQuery

fetchFromRange :: Connection -> Int -> IO [FromRange]
fetchFromRange c = query c fromRangeQuery

fetchFromRanges :: Connection -> [Int] -> IO [FromRange]
fetchFromRanges c ids = query c fromRangesQuery (Only (In ids))

fetchAllFromRanges :: Connection -> IO [FromRange]
fetchAllFromRanges c = query_ c allFromRangesQuery

fetchUpToRange :: Connection -> Int -> IO [UpToRange]
fetchUpToRange c = query c upToRangeQuery

fetchUpToRanges :: Connection -> [Int] -> IO [UpToRange]
fetchUpToRanges c ids = query c upToRangesQuery (Only (In ids))

fetchAllUpToRanges :: Connection -> IO [UpToRange]
fetchAllUpToRanges c = query_ c allUpToRangesQuery

