{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Driller.DB.Wrapper
    ( fetchScenarios
    , fetchScenarioIds
    , fetchDrilledAuthorGroupEntries
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
    , fetchEntriesForAuthorGroup
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
import Driller.DB.Queries ( scenarioListQuery, groupQuery )
import qualified Data.Text as T ( Text(), length )
import qualified Data.Text.Lazy as TL ( Text(), toStrict )
import qualified Data.Text.Read as TR ( signed, decimal )
import Data.Maybe ( fromMaybe )
import Data.HashMap.Strict ( (!) )
import Control.Monad ( liftM )
import Database.PostgreSQL.Simple.ToRow ( ToRow )
import Database.PostgreSQL.Simple
    ( Only(Only), In(In), query_, query )

fetchManyForSelection :: (FromRow a, FromRow b) => QueryCategory -> Config -> Int -> [Int] -> IO (AnswerList [a] [b])
fetchManyForSelection category config limit ids = do
    count <- query c (qm ! (category, COUNT, POLY)) (Only (In ids))
    if head count < limit
      then liftM Entries $ query c (qm ! (category, ENTRY, POLY)) (Only (In ids))
      else liftM Groups  $ query c (qm ! (category, GROUP, POLY)) (Only (In ids))
    where c  = getDBConnection config
          qm = getQueryMap config

fetchEntriesForGroup :: (FromRow a, FromRow b) => QueryCategory -> Config -> [Int] -> IO (AnswerList [a] [b])
fetchEntriesForGroup category config ids =
    liftM Entries $ query (getDBConnection config) (getQueryMap config ! (category, ENTRY, POLY)) (Only (In ids))

fetchEntriesForAuthorGroup :: Config -> [Int] -> IO AuthorList
fetchEntriesForAuthorGroup = fetchEntriesForGroup AUTHOR

fetchManyAuthorsForSelection :: Config -> Int -> [Int] -> IO AuthorList
fetchManyAuthorsForSelection = fetchManyForSelection AUTHOR

fetchManyGamesForSelection :: Config -> Int -> [Int] -> IO GameList
fetchManyGamesForSelection = fetchManyForSelection GAME

fetchManyGenresForSelection :: Config -> Int -> [Int] -> IO GenreList
fetchManyGenresForSelection = fetchManyForSelection GENRE

fetchManyThemesForSelection :: Config -> Int -> [Int] -> IO ThemeList
fetchManyThemesForSelection = fetchManyForSelection THEME

fetchManyMechanicsForSelection :: Config -> Int -> [Int] -> IO MechanicList
fetchManyMechanicsForSelection = fetchManyForSelection MECHANIC

fetchManySidesForSelection :: Config -> Int -> [Int] -> IO SideList
fetchManySidesForSelection = fetchManyForSelection SIDE

fetchManyPartiesForSelection :: Config -> Int -> [Int] -> IO PartyList
fetchManyPartiesForSelection = fetchManyForSelection PARTY

fetchManyPublishersForSelection :: Config -> Int -> [Int] -> IO PublisherList
fetchManyPublishersForSelection = fetchManyForSelection PUBLISHER

fetchManySeriesForSelection :: Config -> Int -> [Int] -> IO SeriesList
fetchManySeriesForSelection = fetchManyForSelection SERIES

fetchManyEnginesForSelection :: Config -> Int -> [Int] -> IO EngineList
fetchManyEnginesForSelection = fetchManyForSelection ENGINE

fetchManyLeadersForSelection :: Config -> Int -> [Int] -> IO LeaderList
fetchManyLeadersForSelection = fetchManyForSelection LEADER

fetchManyLatitudesForSelection :: Config -> Int -> [Int] -> IO LatitudeList
fetchManyLatitudesForSelection = fetchManyForSelection LATITUDE

fetchManyLongitudesForSelection :: Config -> Int -> [Int] -> IO LongitudeList
fetchManyLongitudesForSelection = fetchManyForSelection LONGITUDE

fetchManyFromYearsForSelection :: Config -> Int -> [Int] -> IO FromYearList
fetchManyFromYearsForSelection = fetchManyForSelection FROM_YEAR

fetchManyUpToYearsForSelection :: Config -> Int -> [Int] -> IO UpToYearList
fetchManyUpToYearsForSelection = fetchManyForSelection UPTO_YEAR

fetchManyFromRangesForSelection :: Config -> Int -> [Int] -> IO FromRangeList
fetchManyFromRangesForSelection = fetchManyForSelection FROM_RANGE

fetchManyUpToRangesForSelection :: Config -> Int -> [Int] -> IO UpToRangeList
fetchManyUpToRangesForSelection = fetchManyForSelection UPTO_RANGE

fetchManyFromTimescalesForSelection :: Config -> Int -> [Int] -> IO FromTimescaleList
fetchManyFromTimescalesForSelection = fetchManyForSelection FROM_TIMESCALE

fetchManyUpToTimescalesForSelection :: Config -> Int -> [Int] -> IO UpToTimescaleList
fetchManyUpToTimescalesForSelection = fetchManyForSelection UPTO_TIMESCALE

fetchAuthorGroup, fetchAuthorEntry :: Config -> TL.Text -> IO [Author]
fetchAuthorGroup c = fetchGroup c AUTHOR . TL.toStrict
fetchAuthorEntry c = fetchEntry c AUTHOR . TL.toStrict

fetchGameGroup, fetchGameEntry :: Config -> TL.Text -> IO [Game]
fetchGameGroup c = fetchGroup c GAME . TL.toStrict
fetchGameEntry c = fetchEntry c GAME . TL.toStrict

fetchGenreGroup, fetchGenreEntry :: Config -> TL.Text -> IO [Genre]
fetchGenreGroup c = fetchGroup c GENRE . TL.toStrict
fetchGenreEntry c = fetchEntry c GENRE . TL.toStrict

fetchThemeGroup, fetchThemeEntry :: Config -> TL.Text -> IO [Theme]
fetchThemeGroup c = fetchGroup c THEME . TL.toStrict
fetchThemeEntry c = fetchEntry c THEME . TL.toStrict

fetchMechanicGroup, fetchMechanicEntry :: Config -> TL.Text -> IO [Mechanic]
fetchMechanicGroup c = fetchGroup c MECHANIC . TL.toStrict
fetchMechanicEntry c = fetchEntry c MECHANIC . TL.toStrict

fetchSideGroup, fetchSideEntry :: Config -> TL.Text -> IO [Side]
fetchSideGroup c = fetchGroup c SIDE . TL.toStrict
fetchSideEntry c = fetchEntry c SIDE . TL.toStrict

fetchPartyGroup, fetchPartyEntry :: Config -> TL.Text -> IO [Party]
fetchPartyGroup c = fetchGroup c PARTY . TL.toStrict
fetchPartyEntry c = fetchEntry c PARTY . TL.toStrict

fetchPublisherGroup, fetchPublisherEntry :: Config -> TL.Text -> IO [Publisher]
fetchPublisherGroup c = fetchGroup c PUBLISHER . TL.toStrict
fetchPublisherEntry c = fetchEntry c PUBLISHER . TL.toStrict

fetchSeriesGroup, fetchSeriesEntry :: Config -> TL.Text -> IO [Series]
fetchSeriesGroup c = fetchGroup c SERIES . TL.toStrict
fetchSeriesEntry c = fetchEntry c SERIES . TL.toStrict

fetchEngineGroup, fetchEngineEntry :: Config -> TL.Text -> IO [Engine]
fetchEngineGroup c = fetchGroup c ENGINE . TL.toStrict
fetchEngineEntry c = fetchEntry c ENGINE . TL.toStrict

fetchLeaderGroup, fetchLeaderEntry :: Config -> TL.Text -> IO [Leader]
fetchLeaderGroup c = fetchGroup c LEADER . TL.toStrict
fetchLeaderEntry c = fetchEntry c LEADER . TL.toStrict

fetchEntry, fetchGroup :: (ToRow f, FromRow r) => Config -> QueryCategory -> f -> IO [r]
fetchEntry config cat = query (getDBConnection config) $ getQueryMap config ! (cat, ENTRY, MONO)
fetchGroup config cat = query (getDBConnection config) $ getQueryMap config ! (cat, GROUP, MONO)

fetchGroups, fetchAllEntries :: (FromRow r) => QueryCategory -> Config -> IO [r]
fetchGroups cat c = query_ (getDBConnection c) (getQueryMap c ! (cat, GROUP, OMNI))
fetchAllEntries cat c = query_ (getDBConnection c) (getQueryMap c ! (cat, ENTRY, OMNI))

fetchNumberGroup :: FromRow r => Either String (Int, T.Text) -> Config -> QueryCategory -> IO [r]
fetchNumberGroup p c cat = query (getDBConnection c) (getQueryMap c ! (cat, GROUP, MONO)) $ fromMaybe 0 $ getFromParser p

fetchLatitudeGroup, fetchLatitudeEntry :: Config -> TL.Text -> IO [Latitude]
fetchLatitudeGroup c t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c LATITUDE
fetchLatitudeEntry c = fetchEntry c LATITUDE . TL.toStrict

fetchLongitudeGroup, fetchLongitudeEntry :: Config -> TL.Text -> IO [Longitude]
fetchLongitudeGroup c t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c LONGITUDE
fetchLongitudeEntry c = fetchEntry c LONGITUDE . TL.toStrict

fetchFromYearGroup, fetchFromYearEntry :: Config -> TL.Text -> IO [FromYear]
fetchFromYearGroup c t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c FROM_YEAR
fetchFromYearEntry c = fetchEntry c FROM_YEAR . TL.toStrict

fetchUpToYearGroup, fetchUpToYearEntry :: Config -> TL.Text -> IO [UpToYear]
fetchUpToYearGroup c t = fetchNumberGroup (TR.signed TR.decimal (TL.toStrict t)) c UPTO_YEAR
fetchUpToYearEntry c = fetchEntry c UPTO_YEAR . TL.toStrict

fetchTimescaleGroup, fetchFromTimescaleEntry, fetchUpToTimescaleEntry :: Config -> TL.Text -> IO [UpToTimescale]
fetchTimescaleGroup c t = fetchNumberGroup (TR.decimal (TL.toStrict t)) c TIMESCALE
fetchFromTimescaleEntry c = fetchEntry c FROM_TIMESCALE . TL.toStrict
fetchUpToTimescaleEntry c = fetchEntry c UPTO_TIMESCALE . TL.toStrict

fetchRangeGroup, fetchFromRangeEntry, fetchUpToRangeEntry :: Config -> TL.Text -> IO [UpToRange]
fetchRangeGroup c t = fetchNumberGroup (TR.decimal (TL.toStrict t)) c RANGE
fetchFromRangeEntry c = fetchEntry c FROM_RANGE . TL.toStrict
fetchUpToRangeEntry c = fetchEntry c UPTO_RANGE . TL.toStrict

fetchScenarios :: Config -> [Int] -> IO [Scenario]
fetchScenarios c ids = query (getDBConnection c) (getQueryMap c ! (SCENARIO, ENTRY, POLY)) (Only (In ids))

fetchDrilledAuthorGroupEntries :: Config -> [Parameter] -> IO [Author]
fetchDrilledAuthorGroupEntries c ps = query (getDBConnection c) (groupQuery c "author" ps) (map snd ps)

fetchScenarioIds :: Config -> [Parameter] -> IO [Int]
fetchScenarioIds c ps = query (getDBConnection c) (scenarioListQuery c ps) (map snd ps)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r)) | T.length r == 0 = Just n
                             | otherwise       = Nothing
