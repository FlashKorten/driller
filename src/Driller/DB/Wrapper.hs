module Driller.DB.Wrapper
    ( fetchAuthor,    fetchAuthors,    fetchAuthorGroup,    fetchAuthorGroups,    fetchAllAuthors
    , fetchGenre,     fetchGenres,     fetchGenreGroup,     fetchGenreGroups,     fetchAllGenres
    , fetchEngine,    fetchEngines,    fetchEngineGroup,    fetchEngineGroups,    fetchAllEngines
    , fetchTheme,     fetchThemes,     fetchThemeGroup,     fetchThemeGroups,     fetchAllThemes
    , fetchMechanic,  fetchMechanics,  fetchMechanicGroup,  fetchMechanicGroups,  fetchAllMechanics
    , fetchSide,      fetchSides,      fetchSideGroup,      fetchSideGroups,      fetchAllSides
    , fetchParty,     fetchParties,    fetchPartyGroup,     fetchPartieGroups,    fetchAllParties
    , fetchPublisher, fetchPublishers, fetchPublisherGroup, fetchPublisherGroups, fetchAllPublishers
    , fetchSeries,    fetchSeriess,    fetchSeriesGroup,    fetchSeriesGroups,    fetchAllSeries
    , fetchGame,      fetchGames,      fetchGameGroup,      fetchGameGroups,      fetchAllGames
    , fetchLeader,    fetchLeaders,    fetchLeaderGroup,    fetchLeaderGroups,    fetchAllLeaders
    , fetchLatitude,  fetchLatitudes,  fetchLatitudeGroup,  fetchLatitudeGroups,  fetchAllLatitudes
    , fetchLongitude, fetchLongitudes, fetchLongitudeGroup, fetchLongitudeGroups, fetchAllLongitudes
    , fetchFromYear,  fetchFromYears,  fetchFromYearGroup,  fetchFromYearGroups,  fetchAllFromYears
    , fetchUpToYear,  fetchUpToYears,  fetchUpToYearGroup,  fetchUpToYearGroups,  fetchAllUpToYears
    , fetchFromRange, fetchFromRanges, fetchAllFromRanges
    , fetchUpToRange, fetchUpToRanges, fetchAllUpToRanges
    , fetchRangeGroup, fetchRangeGroups
    , fetchFromTimescale, fetchFromTimescales, fetchAllFromTimescales
    , fetchUpToTimescale, fetchUpToTimescales, fetchAllUpToTimescales
    , fetchTimescaleGroup, fetchTimescaleGroups
    , fetchScenario,  fetchScenarios,  fetchAllScenarios
    , fetchScenarioIds
    , fetchAuthorsForSelection, fetchManyAuthorsForSelection
    , fetchManyGamesForSelection
    , fetchManyGenresForSelection
    , fetchManyEnginesForSelection
    , fetchManyThemesForSelection
    , fetchManyMechanicsForSelection
    , fetchManySidesForSelection
    , fetchManyPartiesForSelection
    , fetchManyPublishersForSelection
    , fetchManySeriessForSelection
    , fetchManyLeadersForSelection
    , fromYearManyGroupsQuery
    , upToYearManyGroupsQuery
    , latitudeManyGroupsQuery
    , longitudeManyGroupsQuery
    , rangeManyGroupsQuery
    , timescaleManyGroupsQuery
    , fetchManyFromYearsForSelection
    , fetchManyUpToYearsForSelection
    , fetchManyFromTimescalesForSelection
    , fetchManyUpToTimescalesForSelection
    , fetchManyFromRangesForSelection
    , fetchManyUpToRangesForSelection
    , fetchManyLatitudesForSelection
    , fetchManyLongitudesForSelection
    ) where

import Driller.Data
import Driller.DB.Queries
import qualified Data.Text as T ( Text(), length )
import qualified Data.Text.Lazy as TL ( Text(), toStrict )
import qualified Data.Text.Read as TR ( signed, decimal )
import Control.Monad ( liftM )
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

fetchAuthorGroup :: Connection -> TL.Text -> IO [Author]
fetchAuthorGroup c = query c authorGroupQuery . TL.toStrict

fetchAuthorGroups :: Connection -> IO [GroupLetter]
fetchAuthorGroups c = query_ c authorGroupsQuery

fetchAllAuthors :: Connection -> IO [Author]
fetchAllAuthors c = query_ c allAuthorsQuery

fetchAuthorsForSelection :: Connection -> IO AuthorList
fetchAuthorsForSelection c = do
    count <- countAuthors c
    if head count < (30 :: Int)
        then liftM Right $ fetchAllAuthors c
        else liftM Left  $ fetchAuthorGroups c

countAuthors :: Connection -> IO [Int]
countAuthors c = query_ c authorsCountQuery

fetchManyAuthorsForSelection :: Connection -> Int -> [Int] -> IO AuthorList
fetchManyAuthorsForSelection c limit ids = do
    count <- query c authorsCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchAuthors c ids
      else liftM Left  $ query c authorManyGroupsQuery (Only (In ids))

fetchManyGenresForSelection :: Connection -> Int -> [Int] -> IO GenreList
fetchManyGenresForSelection c limit ids = do
    count <- query c genresCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchGenres c ids
      else liftM Left  $ query c genreManyGroupsQuery (Only (In ids))

fetchManyGamesForSelection :: Connection -> Int -> [Int] -> IO GameList
fetchManyGamesForSelection c limit ids = do
    count <- query c gamesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchGames c ids
      else liftM Left  $ query c gameManyGroupsQuery (Only (In ids))

fetchManyPublishersForSelection :: Connection -> Int -> [Int] -> IO PublisherList
fetchManyPublishersForSelection c limit ids = do
    count <- query c publishersCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchPublishers c ids
      else liftM Left  $ query c publisherManyGroupsQuery (Only (In ids))

fetchManySidesForSelection :: Connection -> Int -> [Int] -> IO SideList
fetchManySidesForSelection c limit ids = do
    count <- query c sidesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchSides c ids
      else liftM Left  $ query c sideManyGroupsQuery (Only (In ids))

fetchManySeriessForSelection :: Connection -> Int -> [Int] -> IO SeriesList
fetchManySeriessForSelection c limit ids = do
    count <- query c seriesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchSeriess c ids
      else liftM Left  $ query c seriesManyGroupsQuery (Only (In ids))

fetchManyPartiesForSelection :: Connection -> Int -> [Int] -> IO PartyList
fetchManyPartiesForSelection c limit ids = do
    count <- query c partiesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchParties c ids
      else liftM Left  $ query c partyManyGroupsQuery (Only (In ids))

fetchManyThemesForSelection :: Connection -> Int -> [Int] -> IO ThemeList
fetchManyThemesForSelection c limit ids = do
    count <- query c themesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchThemes c ids
      else liftM Left  $ query c themeManyGroupsQuery (Only (In ids))

fetchManyEnginesForSelection :: Connection -> Int -> [Int] -> IO EngineList
fetchManyEnginesForSelection c limit ids = do
    count <- query c enginesCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchEngines c ids
      else liftM Left  $ query c engineManyGroupsQuery (Only (In ids))

fetchManyLeadersForSelection :: Connection -> Int -> [Int] -> IO LeaderList
fetchManyLeadersForSelection c limit ids = do
    count <- query c leadersCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchLeaders c ids
      else liftM Left  $ query c leaderManyGroupsQuery (Only (In ids))

fetchManyMechanicsForSelection :: Connection -> Int -> [Int] -> IO MechanicList
fetchManyMechanicsForSelection c limit ids = do
    count <- query c mechanicsCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchMechanics c ids
      else liftM Left  $ query c mechanicManyGroupsQuery (Only (In ids))

fetchManyFromYearsForSelection :: Connection -> Int -> [Int] -> IO FromYearList
fetchManyFromYearsForSelection c limit ids = do
    count <- query c fromYearCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchFromYears c ids
      else liftM Left  $ query c fromYearManyGroupsQuery (Only (In ids))

fetchManyUpToYearsForSelection :: Connection -> Int -> [Int] -> IO UpToYearList
fetchManyUpToYearsForSelection c limit ids = do
    count <- query c upToYearCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchUpToYears c ids
      else liftM Left  $ query c upToYearManyGroupsQuery (Only (In ids))

fetchManyFromTimescalesForSelection :: Connection -> Int -> [Int] -> IO FromTimescaleList
fetchManyFromTimescalesForSelection c limit ids = do
    count <- query c timescaleCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchFromTimescales c ids
      else liftM Left  $ query c timescaleManyGroupsQuery (Only (In ids))

fetchManyUpToTimescalesForSelection :: Connection -> Int -> [Int] -> IO UpToTimescaleList
fetchManyUpToTimescalesForSelection c limit ids = do
    count <- query c timescaleCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchUpToTimescales c ids
      else liftM Left  $ query c timescaleManyGroupsQuery (Only (In ids))

fetchManyFromRangesForSelection :: Connection -> Int -> [Int] -> IO FromRangeList
fetchManyFromRangesForSelection c limit ids = do
    count <- query c rangeCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchFromRanges c ids
      else liftM Left  $ query c rangeManyGroupsQuery (Only (In ids))

fetchManyUpToRangesForSelection :: Connection -> Int -> [Int] -> IO UpToRangeList
fetchManyUpToRangesForSelection c limit ids = do
    count <- query c rangeCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchUpToRanges c ids
      else liftM Left  $ query c rangeManyGroupsQuery (Only (In ids))

fetchManyLatitudesForSelection :: Connection -> Int -> [Int] -> IO LatitudeList
fetchManyLatitudesForSelection c limit ids = do
    count <- query c latitudeCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchLatitudes c ids
      else liftM Left  $ query c latitudeManyGroupsQuery (Only (In ids))

fetchManyLongitudesForSelection :: Connection -> Int -> [Int] -> IO LongitudeList
fetchManyLongitudesForSelection c limit ids = do
    count <- query c longitudeCountManyQuery (Only (In ids))
    if head count < limit
      then liftM Right $ fetchLongitudes c ids
      else liftM Left  $ query c longitudeManyGroupsQuery (Only (In ids))

fetchGenre :: Connection -> Int -> IO [Genre]
fetchGenre c = query c genreQuery

fetchGenres :: Connection -> [Int] -> IO [Genre]
fetchGenres c ids = query c genresQuery (Only (In ids))

fetchGenreGroup :: Connection -> TL.Text -> IO [Genre]
fetchGenreGroup c = query c genreGroupQuery . TL.toStrict

fetchGenreGroups :: Connection -> IO [GroupLetter]
fetchGenreGroups c = query_ c genreGroupsQuery

fetchAllGenres :: Connection -> IO [Genre]
fetchAllGenres c = query_ c allGenresQuery

fetchEngine :: Connection -> Int -> IO [Engine]
fetchEngine c = query c engineQuery

fetchEngines :: Connection -> [Int] -> IO [Engine]
fetchEngines c ids = query c enginesQuery (Only (In ids))

fetchEngineGroup :: Connection -> TL.Text -> IO [Engine]
fetchEngineGroup c = query c engineGroupQuery . TL.toStrict

fetchEngineGroups :: Connection -> IO [GroupLetter]
fetchEngineGroups c = query_ c engineGroupsQuery

fetchAllEngines :: Connection -> IO [Engine]
fetchAllEngines c = query_ c allEnginesQuery

fetchTheme :: Connection -> Int -> IO [Theme]
fetchTheme c = query c themeQuery

fetchThemes :: Connection -> [Int] -> IO [Theme]
fetchThemes c ids = query c themesQuery (Only (In ids))

fetchThemeGroup :: Connection -> TL.Text -> IO [Theme]
fetchThemeGroup c = query c themeGroupQuery . TL.toStrict

fetchThemeGroups :: Connection -> IO [GroupLetter]
fetchThemeGroups c = query_ c themeGroupsQuery

fetchAllThemes :: Connection -> IO [Theme]
fetchAllThemes c = query_ c allThemesQuery

fetchMechanic :: Connection -> Int -> IO [Mechanic]
fetchMechanic c = query c mechanicQuery

fetchMechanics :: Connection -> [Int] -> IO [Mechanic]
fetchMechanics c ids = query c mechanicsQuery (Only (In ids))

fetchMechanicGroup :: Connection -> TL.Text -> IO [Mechanic]
fetchMechanicGroup c = query c mechanicGroupQuery . TL.toStrict

fetchMechanicGroups :: Connection -> IO [GroupLetter]
fetchMechanicGroups c = query_ c mechanicGroupsQuery

fetchAllMechanics :: Connection -> IO [Mechanic]
fetchAllMechanics c = query_ c allMechanicsQuery

fetchSide :: Connection -> Int -> IO [Side]
fetchSide c = query c sideQuery

fetchSides :: Connection -> [Int] -> IO [Side]
fetchSides c ids = query c sidesQuery (Only (In ids))

fetchSideGroup :: Connection -> TL.Text -> IO [Side]
fetchSideGroup c = query c sideGroupQuery . TL.toStrict

fetchSideGroups :: Connection -> IO [GroupLetter]
fetchSideGroups c = query_ c sideGroupsQuery

fetchAllSides :: Connection -> IO [Side]
fetchAllSides c = query_ c allSidesQuery

fetchParty :: Connection -> Int -> IO [Party]
fetchParty c = query c partyQuery

fetchParties :: Connection -> [Int] -> IO [Party]
fetchParties c ids = query c partiesQuery (Only (In ids))

fetchPartyGroup :: Connection -> TL.Text -> IO [Party]
fetchPartyGroup c = query c partyGroupQuery . TL.toStrict

fetchPartieGroups :: Connection -> IO [GroupLetter]
fetchPartieGroups c = query_ c partieGroupsQuery

fetchAllParties :: Connection -> IO [Party]
fetchAllParties c = query_ c allPartiesQuery

fetchPublisher :: Connection -> Int -> IO [Publisher]
fetchPublisher c = query c publisherQuery

fetchPublishers :: Connection -> [Int] -> IO [Publisher]
fetchPublishers c ids = query c publishersQuery (Only (In ids))

fetchPublisherGroup :: Connection -> TL.Text -> IO [Publisher]
fetchPublisherGroup c = query c publisherGroupQuery . TL.toStrict

fetchPublisherGroups :: Connection -> IO [GroupLetter]
fetchPublisherGroups c = query_ c publisherGroupsQuery

fetchAllPublishers :: Connection -> IO [Publisher]
fetchAllPublishers c = query_ c allPublishersQuery

fetchSeries :: Connection -> Int -> IO [Series]
fetchSeries c = query c seriesQuery

fetchSeriess :: Connection -> [Int] -> IO [Series]
fetchSeriess c ids = query c seriessQuery (Only (In ids))

fetchSeriesGroup :: Connection -> TL.Text -> IO [Series]
fetchSeriesGroup c = query c seriesGroupQuery . TL.toStrict

fetchSeriesGroups :: Connection -> IO [GroupLetter]
fetchSeriesGroups c = query_ c seriesGroupsQuery

fetchAllSeries :: Connection -> IO [Series]
fetchAllSeries c = query_ c allSeriesQuery

fetchGame :: Connection -> Int -> IO [Game]
fetchGame c = query c gameQuery

fetchGames :: Connection -> [Int] -> IO [Game]
fetchGames c ids = query c gamesQuery (Only (In ids))

fetchGameGroup :: Connection -> TL.Text -> IO [Game]
fetchGameGroup c = query c gameGroupQuery . TL.toStrict

fetchGameGroups :: Connection -> IO [GroupLetter]
fetchGameGroups c = query_ c gameGroupsQuery

fetchAllGames :: Connection -> IO [Game]
fetchAllGames c = query_ c allGamesQuery

fetchLeader :: Connection -> Int -> IO [Leader]
fetchLeader c = query c leaderQuery

fetchLeaders :: Connection -> [Int] -> IO [Leader]
fetchLeaders c ids = query c leadersQuery (Only (In ids))

fetchLeaderGroup :: Connection -> TL.Text -> IO [Leader]
fetchLeaderGroup c = query c leaderGroupQuery . TL.toStrict

fetchLeaderGroups :: Connection -> IO [GroupLetter]
fetchLeaderGroups c = query_ c leaderGroupsQuery

fetchAllLeaders :: Connection -> IO [Leader]
fetchAllLeaders c = query_ c allLeadersQuery

fetchLatitude :: Connection -> Int -> IO [Latitude]
fetchLatitude c = query c latitudeQuery

fetchLatitudes :: Connection -> [Int] -> IO [Latitude]
fetchLatitudes c ids = query c latitudesQuery (Only (In ids))

fetchLatitudeGroup :: Connection -> TL.Text -> IO [Latitude]
fetchLatitudeGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c latitudeGroupQuery n
                             Nothing -> query c latitudeGroupQuery (0 :: Int)

fetchLatitudeGroups :: Connection -> IO [GroupNumber]
fetchLatitudeGroups c = query_ c latitudeGroupsQuery

fetchAllLatitudes :: Connection -> IO [Latitude]
fetchAllLatitudes c = query_ c allLatitudesQuery

fetchLongitude :: Connection -> Int -> IO [Longitude]
fetchLongitude c = query c longitudeQuery

fetchLongitudes :: Connection -> [Int] -> IO [Longitude]
fetchLongitudes c ids = query c longitudesQuery (Only (In ids))

fetchLongitudeGroup :: Connection -> TL.Text -> IO [Longitude]
fetchLongitudeGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c longitudeGroupQuery n
                             Nothing -> query c longitudeGroupQuery (0 :: Int)

fetchLongitudeGroups :: Connection -> IO [GroupNumber]
fetchLongitudeGroups c = query_ c longitudeGroupsQuery

fetchAllLongitudes :: Connection -> IO [Longitude]
fetchAllLongitudes c = query_ c allLongitudesQuery

fetchFromYear :: Connection -> Int -> IO [FromYear]
fetchFromYear c = query c fromYearQuery

fetchFromYears :: Connection -> [Int] -> IO [FromYear]
fetchFromYears c ids = query c fromYearsQuery (Only (In ids))

fetchFromYearGroup :: Connection -> TL.Text -> IO [FromYear]
fetchFromYearGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c fromYearGroupQuery n
                             Nothing -> query c fromYearGroupQuery (0 :: Int)

fetchFromYearGroups :: Connection -> IO [GroupNumber]
fetchFromYearGroups c = query_ c fromYearGroupsQuery

fetchAllFromYears :: Connection -> IO [FromYear]
fetchAllFromYears c = query_ c allFromYearsQuery

fetchUpToYear :: Connection -> Int -> IO [UpToYear]
fetchUpToYear c = query c upToYearQuery

fetchUpToYears :: Connection -> [Int] -> IO [UpToYear]
fetchUpToYears c ids = query c upToYearsQuery (Only (In ids))

fetchUpToYearGroup :: Connection -> TL.Text -> IO [UpToYear]
fetchUpToYearGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c upToYearGroupQuery n
                             Nothing -> query c upToYearGroupQuery (0 :: Int)

fetchUpToYearGroups :: Connection -> IO [GroupNumber]
fetchUpToYearGroups c = query_ c upToYearGroupsQuery

fetchAllUpToYears :: Connection -> IO [UpToYear]
fetchAllUpToYears c = query_ c allUpToYearsQuery

fetchFromTimescale :: Connection -> Int -> IO [FromTimescale]
fetchFromTimescale c = query c fromTimescaleQuery

fetchUpToTimescale :: Connection -> Int -> IO [UpToTimescale]
fetchUpToTimescale c = query c upToTimescaleQuery

fetchFromTimescales :: Connection -> [Int] -> IO [FromTimescale]
fetchFromTimescales c ids = query c timescalesQuery (Only (In ids))

fetchUpToTimescales :: Connection -> [Int] -> IO [UpToTimescale]
fetchUpToTimescales c ids = query c timescalesQuery (Only (In ids))

fetchTimescaleGroup :: Connection -> TL.Text -> IO [UpToTimescale]
fetchTimescaleGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c timescaleGroupQuery n
                             Nothing -> query c timescaleGroupQuery (0 :: Int)

fetchTimescaleGroups :: Connection -> IO [GroupNumber]
fetchTimescaleGroups c = query_ c timescaleGroupsQuery

fetchAllFromTimescales :: Connection -> IO [FromTimescale]
fetchAllFromTimescales c = query_ c allTimescalesQuery

fetchAllUpToTimescales :: Connection -> IO [UpToTimescale]
fetchAllUpToTimescales c = query_ c allTimescalesQuery

fetchFromRange :: Connection -> Int -> IO [FromRange]
fetchFromRange c = query c fromRangeQuery

fetchUpToRange :: Connection -> Int -> IO [UpToRange]
fetchUpToRange c = query c upToRangeQuery

fetchFromRanges :: Connection -> [Int] -> IO [FromRange]
fetchFromRanges c ids = query c rangesQuery (Only (In ids))

fetchUpToRanges :: Connection -> [Int] -> IO [UpToRange]
fetchUpToRanges c ids = query c rangesQuery (Only (In ids))

fetchRangeGroup :: Connection -> TL.Text -> IO [UpToRange]
fetchRangeGroup c t = case getFromParser (TR.signed TR.decimal (TL.toStrict t)) of
                             Just n  -> query c rangeGroupQuery n
                             Nothing -> query c rangeGroupQuery (0 :: Int)

fetchRangeGroups :: Connection -> IO [GroupNumber]
fetchRangeGroups c = query_ c rangeGroupsQuery

fetchAllFromRanges :: Connection -> IO [FromRange]
fetchAllFromRanges c = query_ c allRangesQuery

fetchAllUpToRanges :: Connection -> IO [UpToRange]
fetchAllUpToRanges c = query_ c allRangesQuery

fetchScenario :: Connection -> Int -> IO [Scenario]
fetchScenario c = query c scenarioQuery

fetchAllScenarios :: Connection -> IO [Scenario]
fetchAllScenarios c = query_ c allScenariosQuery

fetchScenarios :: Connection -> [Int] -> IO [Scenario]
fetchScenarios c ids = query c scenariosQuery (Only (In ids))

fetchScenarioIds :: Connection -> JoinMap -> [Parameter] -> IO [Int]
fetchScenarioIds c joinMap p = query c (scenarioListQuery joinMap p) (map snd p)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r)) | T.length r == 0 = Just n
                             | otherwise       = Nothing

