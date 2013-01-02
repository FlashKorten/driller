module Driller.DB.Wrapper
    ( fetchAuthor,    fetchAuthors,    fetchAuthorSection,    fetchAuthorsToc,    fetchAllAuthors
    , fetchGenre,     fetchGenres,     fetchGenreSection,     fetchGenresToc,     fetchAllGenres
    , fetchEngine,    fetchEngines,    fetchEngineSection,    fetchEnginesToc,    fetchAllEngines
    , fetchTheme,     fetchThemes,     fetchThemeSection,     fetchThemesToc,     fetchAllThemes
    , fetchMechanic,  fetchMechanics,  fetchMechanicSection,  fetchMechanicsToc,  fetchAllMechanics
    , fetchSide,      fetchSides,      fetchSideSection,      fetchSidesToc,      fetchAllSides
    , fetchParty,     fetchParties,    fetchPartySection,     fetchPartiesToc,    fetchAllParties
    , fetchPublisher, fetchPublishers, fetchPublisherSection, fetchPublishersToc, fetchAllPublishers
    , fetchSeries,    fetchSeriess,    fetchSeriesSection,    fetchSeriesToc,     fetchAllSeries
    , fetchGame,      fetchGames,      fetchGameSection,      fetchGamesToc,      fetchAllGames
    , fetchLeader,    fetchLeaders,    fetchLeaderSection,    fetchLeadersToc,    fetchAllLeaders
    , fetchLatitude,  fetchLatitudes,  fetchAllLatitudes
    , fetchLongitude, fetchLongitudes, fetchAllLongitudes
    , fetchFromYear,  fetchFromYears,  fetchAllFromYears
    , fetchUpToYear,  fetchUpToYears,  fetchAllUpToYears
    , fetchFromRange, fetchFromRanges, fetchAllFromRanges
    , fetchUpToRange, fetchUpToRanges, fetchAllUpToRanges
    , fetchScenario,  fetchScenarios,  fetchAllScenarios
    , fetchScenarioIds
    ) where

import Driller.Data
import Driller.DB.Queries
import qualified Data.Text.Lazy as TL ( Text(), toStrict )
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

fetchAuthorSection :: Connection -> TL.Text -> IO [Author]
fetchAuthorSection c = query c authorsSectionQuery . TL.toStrict

fetchAuthorsToc :: Connection -> IO [SectionAlph]
fetchAuthorsToc c = query_ c authorsTocQuery

fetchAllAuthors :: Connection -> IO [Author]
fetchAllAuthors c = query_ c allAuthorsQuery

fetchGenre :: Connection -> Int -> IO [Genre]
fetchGenre c = query c genreQuery

fetchGenres :: Connection -> [Int] -> IO [Genre]
fetchGenres c ids = query c genresQuery (Only (In ids))

fetchGenreSection :: Connection -> TL.Text -> IO [Genre]
fetchGenreSection c = query c genreSectionQuery . TL.toStrict

fetchGenresToc :: Connection -> IO [SectionAlph]
fetchGenresToc c = query_ c genresTocQuery

fetchAllGenres :: Connection -> IO [Genre]
fetchAllGenres c = query_ c allGenresQuery

fetchEngine :: Connection -> Int -> IO [Engine]
fetchEngine c = query c engineQuery

fetchEngines :: Connection -> [Int] -> IO [Engine]
fetchEngines c ids = query c enginesQuery (Only (In ids))

fetchEngineSection :: Connection -> TL.Text -> IO [Engine]
fetchEngineSection c = query c engineSectionQuery . TL.toStrict

fetchEnginesToc :: Connection -> IO [SectionAlph]
fetchEnginesToc c = query_ c enginesTocQuery

fetchAllEngines :: Connection -> IO [Engine]
fetchAllEngines c = query_ c allEnginesQuery

fetchTheme :: Connection -> Int -> IO [Theme]
fetchTheme c = query c themeQuery

fetchThemes :: Connection -> [Int] -> IO [Theme]
fetchThemes c ids = query c themesQuery (Only (In ids))

fetchThemeSection :: Connection -> TL.Text -> IO [Theme]
fetchThemeSection c = query c themeSectionQuery . TL.toStrict

fetchThemesToc :: Connection -> IO [SectionAlph]
fetchThemesToc c = query_ c themesTocQuery

fetchAllThemes :: Connection -> IO [Theme]
fetchAllThemes c = query_ c allThemesQuery

fetchMechanic :: Connection -> Int -> IO [Mechanic]
fetchMechanic c = query c mechanicQuery

fetchMechanics :: Connection -> [Int] -> IO [Mechanic]
fetchMechanics c ids = query c mechanicsQuery (Only (In ids))

fetchMechanicSection :: Connection -> TL.Text -> IO [Mechanic]
fetchMechanicSection c = query c mechanicSectionQuery . TL.toStrict

fetchMechanicsToc :: Connection -> IO [SectionAlph]
fetchMechanicsToc c = query_ c mechanicsTocQuery

fetchAllMechanics :: Connection -> IO [Mechanic]
fetchAllMechanics c = query_ c allMechanicsQuery

fetchSide :: Connection -> Int -> IO [Side]
fetchSide c = query c sideQuery

fetchSides :: Connection -> [Int] -> IO [Side]
fetchSides c ids = query c sidesQuery (Only (In ids))

fetchSideSection :: Connection -> TL.Text -> IO [Side]
fetchSideSection c = query c sideSectionQuery . TL.toStrict

fetchSidesToc :: Connection -> IO [SectionAlph]
fetchSidesToc c = query_ c sidesTocQuery

fetchAllSides :: Connection -> IO [Side]
fetchAllSides c = query_ c allSidesQuery

fetchParty :: Connection -> Int -> IO [Party]
fetchParty c = query c partyQuery

fetchParties :: Connection -> [Int] -> IO [Party]
fetchParties c ids = query c partiesQuery (Only (In ids))

fetchPartySection :: Connection -> TL.Text -> IO [Party]
fetchPartySection c = query c partySectionQuery . TL.toStrict

fetchPartiesToc :: Connection -> IO [SectionAlph]
fetchPartiesToc c = query_ c partiesTocQuery

fetchAllParties :: Connection -> IO [Party]
fetchAllParties c = query_ c allPartiesQuery

fetchPublisher :: Connection -> Int -> IO [Publisher]
fetchPublisher c = query c publisherQuery

fetchPublishers :: Connection -> [Int] -> IO [Publisher]
fetchPublishers c ids = query c publishersQuery (Only (In ids))

fetchPublisherSection :: Connection -> TL.Text -> IO [Publisher]
fetchPublisherSection c = query c publisherSectionQuery . TL.toStrict

fetchPublishersToc :: Connection -> IO [SectionAlph]
fetchPublishersToc c = query_ c publishersTocQuery

fetchAllPublishers :: Connection -> IO [Publisher]
fetchAllPublishers c = query_ c allPublishersQuery

fetchSeries :: Connection -> Int -> IO [Series]
fetchSeries c = query c seriesQuery

fetchSeriess :: Connection -> [Int] -> IO [Series]
fetchSeriess c ids = query c seriessQuery (Only (In ids))

fetchSeriesSection :: Connection -> TL.Text -> IO [Series]
fetchSeriesSection c = query c seriesSectionQuery . TL.toStrict

fetchSeriesToc :: Connection -> IO [SectionAlph]
fetchSeriesToc c = query_ c seriesTocQuery

fetchAllSeries :: Connection -> IO [Series]
fetchAllSeries c = query_ c allSeriesQuery

fetchGame :: Connection -> Int -> IO [Game]
fetchGame c = query c gameQuery

fetchGames :: Connection -> [Int] -> IO [Game]
fetchGames c ids = query c gamesQuery (Only (In ids))

fetchGameSection :: Connection -> TL.Text -> IO [Game]
fetchGameSection c = query c gameSectionQuery . TL.toStrict

fetchGamesToc :: Connection -> IO [SectionAlph]
fetchGamesToc c = query_ c gamesTocQuery

fetchAllGames :: Connection -> IO [Game]
fetchAllGames c = query_ c allGamesQuery

fetchLeader :: Connection -> Int -> IO [Leader]
fetchLeader c = query c leaderQuery

fetchLeaders :: Connection -> [Int] -> IO [Leader]
fetchLeaders c ids = query c leadersQuery (Only (In ids))

fetchLeaderSection :: Connection -> TL.Text -> IO [Leader]
fetchLeaderSection c = query c leaderSectionQuery . TL.toStrict

fetchLeadersToc :: Connection -> IO [SectionAlph]
fetchLeadersToc c = query_ c leadersTocQuery

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

fetchScenario :: Connection -> Int -> IO [Scenario]
fetchScenario c = query c scenarioQuery

fetchAllScenarios :: Connection -> IO [Scenario]
fetchAllScenarios c = query_ c allScenariosQuery

fetchScenarios :: Connection -> [Int] -> IO [Scenario]
fetchScenarios c ids = query c scenariosQuery (Only (In ids))

fetchScenarioIds :: Connection -> JoinMap -> [Parameter] -> IO [Int]
fetchScenarioIds c joinMap p = query c (scenarioListQuery joinMap p) (map snd p)
