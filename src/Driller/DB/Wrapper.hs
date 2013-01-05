{-# LANGUAGE NoMonomorphismRestriction #-}
module Driller.DB.Wrapper
    ( fetchScenarios
    , fetchScenarioIds
    , fetchManyForSelection
    , fetchEntry
    , fetchAllEntries
    , fetchGroup
    , fetchGroups
    , fetchAuthorGroup
    , fetchAuthorEntry
    , fetchGameGroup
    , fetchGameEntry
    , fetchGenreGroup
    , fetchGenreEntry
    , fetchThemeGroup
    , fetchThemeEntry
    , fetchMechanicGroup
    , fetchMechanicEntry
    , fetchSideGroup
    , fetchSideEntry
    , fetchPartyGroup
    , fetchPartyEntry
    , fetchPublisherGroup
    , fetchPublisherEntry
    , fetchSeriesGroup
    , fetchSeriesEntry
    , fetchEngineGroup
    , fetchEngineEntry
    , fetchLeaderGroup
    , fetchLeaderEntry
    , fetchLatitudeGroup
    , fetchLatitudeEntry
    , fetchLongitudeGroup
    , fetchLongitudeEntry
    , fetchFromYearGroup
    , fetchFromYearEntry
    , fetchUpToYearGroup
    , fetchUpToYearEntry
    , fetchRangeGroup
    , fetchFromRangeEntry
    , fetchUpToRangeEntry
    , fetchTimescaleGroup
    , fetchFromTimescaleEntry
    , fetchUpToTimescaleEntry
    , fetchManyAuthorsForSelection
    , fetchManyGamesForSelection
    , fetchManyGenresForSelection
    , fetchManyThemesForSelection
    , fetchManyMechanicsForSelection
    , fetchManySidesForSelection
    , fetchManyPartiesForSelection
    , fetchManyPublishersForSelection
    , fetchManySeriesForSelection
    , fetchManyEnginesForSelection
    , fetchManyLeadersForSelection
    , fetchManyLatitudesForSelection
    , fetchManyLongitudesForSelection
    , fetchManyFromYearsForSelection
    , fetchManyUpToYearsForSelection
    , fetchManyFromRangesForSelection
    , fetchManyUpToRangesForSelection
    , fetchManyFromTimescalesForSelection
    , fetchManyUpToTimescalesForSelection
    ) where

import Driller.Data
import Driller.DB.Queries ( scenarioListQuery )
import qualified Data.Text as T ( Text(), length )
import qualified Data.Text.Lazy as TL ( Text(), toStrict )
import qualified Data.Text.Read as TR ( signed, decimal )
import Data.Maybe ( fromMaybe )
import Data.HashMap.Strict ( (!) )
import Control.Monad ( liftM )
import Database.PostgreSQL.Simple.ToRow ( ToRow )
import Database.PostgreSQL.Simple
    ( Only(Only), In(In), Connection, query_, query )

fetchManyForSelection :: (FromRow a, FromRow b) => QueryCategory -> QueryMap -> Connection -> Int -> [Int] -> IO (AnswerList [a] [b])
fetchManyForSelection category queryMap c limit ids = do
    count <- query c (queryMap ! (category, COUNT, POLY)) (Only (In ids))
    if head count < limit
      then liftM Entries $ query c (queryMap ! (category, ENTRY, POLY)) (Only (In ids))
      else liftM Groups  $ query c (queryMap ! (category, GROUP, POLY)) (Only (In ids))

fetchManyAuthorsForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO AuthorList
fetchManyAuthorsForSelection = fetchManyForSelection AUTHOR

fetchManyGamesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO GameList
fetchManyGamesForSelection = fetchManyForSelection GAME

fetchManyGenresForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO GenreList
fetchManyGenresForSelection = fetchManyForSelection GENRE

fetchManyThemesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO ThemeList
fetchManyThemesForSelection = fetchManyForSelection THEME

fetchManyMechanicsForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO MechanicList
fetchManyMechanicsForSelection = fetchManyForSelection MECHANIC

fetchManySidesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO SideList
fetchManySidesForSelection = fetchManyForSelection SIDE

fetchManyPartiesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO PartyList
fetchManyPartiesForSelection = fetchManyForSelection PARTY

fetchManyPublishersForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO PublisherList
fetchManyPublishersForSelection = fetchManyForSelection PUBLISHER

fetchManySeriesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO SeriesList
fetchManySeriesForSelection = fetchManyForSelection SERIES

fetchManyEnginesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO EngineList
fetchManyEnginesForSelection = fetchManyForSelection ENGINE

fetchManyLeadersForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO LeaderList
fetchManyLeadersForSelection = fetchManyForSelection LEADER

fetchManyLatitudesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO LatitudeList
fetchManyLatitudesForSelection = fetchManyForSelection LATITUDE

fetchManyLongitudesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO LongitudeList
fetchManyLongitudesForSelection = fetchManyForSelection LONGITUDE

fetchManyFromYearsForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO FromYearList
fetchManyFromYearsForSelection = fetchManyForSelection FROM_YEAR

fetchManyUpToYearsForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO UpToYearList
fetchManyUpToYearsForSelection = fetchManyForSelection UPTO_YEAR

fetchManyFromRangesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO FromRangeList
fetchManyFromRangesForSelection = fetchManyForSelection FROM_RANGE

fetchManyUpToRangesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO UpToRangeList
fetchManyUpToRangesForSelection = fetchManyForSelection UPTO_RANGE

fetchManyFromTimescalesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO FromTimescaleList
fetchManyFromTimescalesForSelection = fetchManyForSelection FROM_TIMESCALE

fetchManyUpToTimescalesForSelection :: QueryMap -> Connection -> Int -> [Int] -> IO UpToTimescaleList
fetchManyUpToTimescalesForSelection = fetchManyForSelection UPTO_TIMESCALE

fetchAuthorGroup, fetchAuthorEntry :: Connection -> QueryMap -> TL.Text -> IO [Author]
fetchAuthorGroup c queryMap = fetchGroup AUTHOR c queryMap . TL.toStrict
fetchAuthorEntry c queryMap = fetchEntry AUTHOR c queryMap . TL.toStrict

fetchGameGroup, fetchGameEntry :: Connection -> QueryMap -> TL.Text -> IO [Game]
fetchGameGroup c queryMap = fetchGroup GAME c queryMap . TL.toStrict
fetchGameEntry c queryMap = fetchEntry GAME c queryMap . TL.toStrict

fetchGenreGroup, fetchGenreEntry :: Connection -> QueryMap -> TL.Text -> IO [Genre]
fetchGenreGroup c queryMap = fetchGroup GENRE c queryMap . TL.toStrict
fetchGenreEntry c queryMap = fetchEntry GENRE c queryMap . TL.toStrict

fetchThemeGroup, fetchThemeEntry :: Connection -> QueryMap -> TL.Text -> IO [Theme]
fetchThemeGroup c queryMap = fetchGroup THEME c queryMap . TL.toStrict
fetchThemeEntry c queryMap = fetchEntry THEME c queryMap . TL.toStrict

fetchMechanicGroup, fetchMechanicEntry :: Connection -> QueryMap -> TL.Text -> IO [Mechanic]
fetchMechanicGroup c queryMap = fetchGroup MECHANIC c queryMap . TL.toStrict
fetchMechanicEntry c queryMap = fetchEntry MECHANIC c queryMap . TL.toStrict

fetchSideGroup, fetchSideEntry :: Connection -> QueryMap -> TL.Text -> IO [Side]
fetchSideGroup c queryMap = fetchGroup SIDE c queryMap . TL.toStrict
fetchSideEntry c queryMap = fetchEntry SIDE c queryMap . TL.toStrict

fetchPartyGroup, fetchPartyEntry :: Connection -> QueryMap -> TL.Text -> IO [Party]
fetchPartyGroup c queryMap = fetchGroup PARTY c queryMap . TL.toStrict
fetchPartyEntry c queryMap = fetchEntry PARTY c queryMap . TL.toStrict

fetchPublisherGroup, fetchPublisherEntry :: Connection -> QueryMap -> TL.Text -> IO [Publisher]
fetchPublisherGroup c queryMap = fetchGroup PUBLISHER c queryMap . TL.toStrict
fetchPublisherEntry c queryMap = fetchEntry PUBLISHER c queryMap . TL.toStrict

fetchSeriesGroup, fetchSeriesEntry :: Connection -> QueryMap -> TL.Text -> IO [Series]
fetchSeriesGroup c queryMap = fetchGroup SERIES c queryMap . TL.toStrict
fetchSeriesEntry c queryMap = fetchEntry SERIES c queryMap . TL.toStrict

fetchEngineGroup, fetchEngineEntry :: Connection -> QueryMap -> TL.Text -> IO [Engine]
fetchEngineGroup c queryMap = fetchGroup ENGINE c queryMap . TL.toStrict
fetchEngineEntry c queryMap = fetchEntry ENGINE c queryMap . TL.toStrict

fetchLeaderGroup, fetchLeaderEntry :: Connection -> QueryMap -> TL.Text -> IO [Leader]
fetchLeaderGroup c queryMap = fetchGroup LEADER c queryMap . TL.toStrict
fetchLeaderEntry c queryMap = fetchEntry LEADER c queryMap . TL.toStrict

fetchEntry, fetchGroup :: (ToRow f, FromRow r) => QueryCategory -> Connection -> QueryMap -> f -> IO [r]
fetchEntry cat c qm = query c $ qm ! (cat, ENTRY, MONO)
fetchGroup cat c qm = query c (qm ! (cat, GROUP, MONO))

fetchGroups, fetchAllEntries :: (FromRow r) => QueryCategory -> Connection -> QueryMap -> IO [r]
fetchGroups cat c qm = query_ c (qm ! (cat, GROUP, OMNI))
fetchAllEntries cat c qm = query_ c (qm ! (cat, ENTRY, OMNI))

fetchNumberGroup :: FromRow r => Either String (Int, T.Text) -> Connection -> QueryCategory -> QueryMap -> IO [r]
fetchNumberGroup p c cat qm = query c (qm ! (cat, GROUP, MONO)) $ fromMaybe 0 $ getFromParser p

fetchLatitudeGroup, fetchLatitudeEntry :: Connection -> QueryMap -> TL.Text -> IO [Latitude]
fetchLatitudeGroup c queryMap t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c LATITUDE queryMap
fetchLatitudeEntry c queryMap = fetchEntry LATITUDE c queryMap . TL.toStrict

fetchLongitudeGroup, fetchLongitudeEntry :: Connection -> QueryMap -> TL.Text -> IO [Longitude]
fetchLongitudeGroup c queryMap t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c LONGITUDE queryMap
fetchLongitudeEntry c queryMap = fetchEntry LONGITUDE c queryMap . TL.toStrict

fetchFromYearGroup, fetchFromYearEntry :: Connection -> QueryMap -> TL.Text -> IO [FromYear]
fetchFromYearGroup c queryMap t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c FROM_YEAR queryMap
fetchFromYearEntry c queryMap = fetchEntry FROM_YEAR c queryMap . TL.toStrict

fetchUpToYearGroup, fetchUpToYearEntry :: Connection -> QueryMap -> TL.Text -> IO [UpToYear]
fetchUpToYearGroup c queryMap t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c UPTO_YEAR queryMap
fetchUpToYearEntry c queryMap = fetchEntry UPTO_YEAR c queryMap . TL.toStrict

fetchTimescaleGroup, fetchFromTimescaleEntry, fetchUpToTimescaleEntry :: Connection -> QueryMap -> TL.Text -> IO [UpToTimescale]
fetchTimescaleGroup c queryMap t = fetchNumberGroup (TR.decimal (TL.toStrict t)) c TIMESCALE queryMap
fetchFromTimescaleEntry c queryMap = fetchEntry FROM_TIMESCALE c queryMap . TL.toStrict
fetchUpToTimescaleEntry c queryMap = fetchEntry UPTO_TIMESCALE c queryMap . TL.toStrict

fetchRangeGroup, fetchFromRangeEntry, fetchUpToRangeEntry :: Connection -> QueryMap -> TL.Text -> IO [UpToRange]
fetchRangeGroup c queryMap t = fetchNumberGroup (TR.decimal (TL.toStrict t)) c RANGE queryMap
fetchFromRangeEntry c queryMap = fetchEntry FROM_RANGE c queryMap . TL.toStrict
fetchUpToRangeEntry c queryMap = fetchEntry UPTO_RANGE c queryMap . TL.toStrict

fetchScenarios :: Connection -> QueryMap -> [Int] -> IO [Scenario]
fetchScenarios c qm ids = query c (qm ! (SCENARIO, ENTRY, POLY)) (Only (In ids))

fetchScenarioIds :: Connection -> JoinMap -> [Parameter] -> IO [Int]
fetchScenarioIds c joinMap p = query c (scenarioListQuery joinMap p) (map snd p)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r)) | T.length r == 0 = Just n
                             | otherwise       = Nothing
