{-# LANGUAGE OverloadedStrings #-}
module Driller.DB
    ( connectionInfo
    , fetchDrilledGameResult
    , fetchDrilledAuthorGroup
    , fetchDrilledEngineGroup
    , fetchDrilledGameGroup
    , fetchDrilledGenreGroup
    , fetchDrilledLeaderGroup
    , fetchDrilledMechanicGroup
    , fetchDrilledPartyGroup
    , fetchDrilledPublisherGroup
    , fetchDrilledSeriesGroup
    , fetchDrilledSideGroup
    , fetchDrilledThemeGroup
    , fetchDrilledFromYearGroup
    , fetchDrilledUpToYearGroup
    , fetchDrilledLatitudeGroup
    , fetchDrilledLongitudeGroup
    , fetchDrilledFromRangeGroup
    , fetchDrilledFromTimescaleGroup
    , fetchDrilledUpToRangeGroup
    , fetchDrilledUpToTimescaleGroup
    , initJoinMap
    , initQueryMap
    , initGroupMap
    , convertValue
    , fetchScenarios
    , fetchScenarioIds
    , fetchDrilledGroupEntries
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
    , fetchAllAuthorEntries
    , fetchAllEngineEntries
    , fetchAllFromRangeEntries
    , fetchAllFromTimescaleEntries
    , fetchAllFromYearEntries
    , fetchAllGameEntries
    , fetchAllGenreEntries
    , fetchAllLatitudeEntries
    , fetchAllLeaderEntries
    , fetchAllLongitudeEntries
    , fetchAllMechanicEntries
    , fetchAllPartyEntries
    , fetchAllPublisherEntries
    , fetchAllSeriesEntries
    , fetchAllSideEntries
    , fetchAllThemeEntries
    , fetchAllUpToRangeEntries
    , fetchAllUpToTimescaleEntries
    , fetchAllUpToYearEntries
    , fetchAuthorGroups
    , fetchEngineGroups
    , fetchFromYearGroups
    , fetchGameGroups
    , fetchGenreGroups
    , fetchLatitudeGroups
    , fetchLeaderGroups
    , fetchLongitudeGroups
    , fetchMechanicGroups
    , fetchPartyGroups
    , fetchPublisherGroups
    , fetchRangeGroups
    , fetchSeriesGroups
    , fetchSideGroups
    , fetchThemeGroups
    , fetchTimescaleGroups
    , fetchUpToYearGroups
    ) where

import Driller.Data
import qualified Driller.Error as Error
import Driller.DB.Queries ( initJoinMap, initQueryMap, initGroupMap, scenarioListQuery, groupQuery )
import Control.Monad ( liftM )
import Data.Hashable ()
import Data.HashMap.Strict ( (!) )
import Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
import Data.Text.Lazy.Internal ()
import qualified Data.Text as T ( Text(), length )
import qualified Data.Text.Read as TR ( signed, decimal )
import qualified Data.Text.Lazy as TL ( Text(), toStrict )
import qualified Data.HashMap.Strict as HM
    ( fromList, lookup, member )
import Web.Scotty ( Param )
import Database.PostgreSQL.Simple.ToRow ( ToRow )
import Database.PostgreSQL.Simple
    ( ConnectInfo(connectDatabase, connectPassword, connectUser)
    , defaultConnectInfo
    , Only(Only), In(In), query_, query
    )

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser     = "driller"
                                    , connectPassword = "driller"
                                    , connectDatabase = "dr"
                                    }

filterParameters :: [Param] -> JoinMap -> [Parameter] -> Either Error.ParameterError [Parameter]
filterParameters [] _ result        = Right result
filterParameters ((k, v):ps) jm tmp | not $ HM.member key jm = Left $ Error.unknownParameter key
                                    | isNothing value        = Left $ Error.illegalValue key
                                    | alreadySeen key tmp    = Left $ Error.duplicateParameter key
                                    | otherwise              = filterParameters ps jm ((key, Number $ fromJust value):tmp)
                                   where key   = TL.toStrict k
                                         value = convertValue key (TL.toStrict v)

prepareGroupParameters :: Bool -> [Param] -> JoinMap -> Either Error.ParameterError [Parameter]
prepareGroupParameters _ [] _                   = Left Error.noGroupIdFound
prepareGroupParameters isNumeric (p:ps) joinMap = result
                                       where result = if not isNumeric || isJust num
                                                        then fmap (groupId :) $ filterParameters ps joinMap []
                                                        else Left $ Error.illegalGroupId rawValue
                                             groupId = ("id" :: T.Text, value)
                                             rawValue = TL.toStrict $ snd p
                                             num = getFromParser (TR.signed TR.decimal rawValue)
                                             value = if isNumeric
                                                          then Number $ fromJust num
                                                          else GroupID rawValue

alreadySeen :: T.Text -> [Parameter] -> Bool
alreadySeen _ []        = False
alreadySeen name (p:ps) | name == fst p = True
                        | otherwise     = alreadySeen name ps

convertValue :: T.Text -> T.Text -> Maybe Int
convertValue "latitude"  t = getFromParser (TR.signed TR.decimal t) >>= filterWithinLimits (negate 90) 90
convertValue "longitude" t = getFromParser (TR.signed TR.decimal t) >>= filterWithinLimits (negate 180) 180
convertValue "fromRange" t = getFromParser (TR.signed TR.decimal t) >>= filterPositive
convertValue "upToRange" t = getFromParser (TR.signed TR.decimal t) >>= filterPositive
convertValue _ t           = getFromParser (TR.signed TR.decimal t)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r)) | T.length r == 0 = Just n
                             | otherwise       = Nothing

filterWithinLimits :: Int -> Int -> Int -> Maybe Int
filterWithinLimits lower upper value | value >= lower && value <= upper = Just value
                                     | otherwise                        = Nothing

filterPositive :: Int -> Maybe Int
filterPositive value | value > 0 = Just value
                     | otherwise = Nothing

fetchDrilledGroup :: FromRow a => Bool -> T.Text -> Config -> [Param] -> IO [a]
fetchDrilledGroup isNumeric category config p =
    case prepareGroupParameters isNumeric p (getJoinMap config) of
        Left _      -> return []
        Right pList -> fetchDrilledGroupEntries config category pList

fetchDrilledGameResult :: Config -> [Param] -> IO Answer
fetchDrilledGameResult config p =
    case filterParameters p (getJoinMap config) [] of
        Left e      -> return $ Left e
        Right pList -> fetchPositiveAnswer config pList

fetchPositiveAnswer :: Config -> [Parameter] -> IO Answer
fetchPositiveAnswer config p = do
    ids <- fetchScenarioIds config p
    if null ids
       then return $ Right emptyResult
       else prepareResult config{getParameterMap = HM.fromList p} ids

fetchForResult ::
  (FromRow r, MarkExclusive r) => Config -> T.Text -> QueryCategory
       -> (Config -> Int -> [Int] -> IO (AnswerList a [r]))
       -> [Int] -> IO (AnswerList a [r])

fetchForResult config key cat f ids
    = case HM.lookup key $ getParameterMap config of
        Just (Number value) -> if value >= 0
                                 then liftM createEntries $ fetchEntry config cat value
                                 else liftM (createEntries .  markExclusive) (fetchEntry config cat (negate value))
        _ -> f config 25 ids

createEntries x = Entries x []
createGroups x = Groups x []

fetchSimpleValuesForResult ::
  (FromInt r) => Config -> T.Text
      -> (Config -> Int -> [Int] -> IO (AnswerList a [r]))
      -> [Int] -> IO (AnswerList a [r])

fetchSimpleValuesForResult config key f ids
    = case HM.lookup key $ getParameterMap config of
        Just (Number value) -> return $ Entries [fromInt value] []
        _ -> f config 25 ids

prepareResult :: Config -> [Int] -> IO Answer
prepareResult config ids = do
    let numberOfResults = length ids
    scenarios  <- if numberOfResults > 50 then return [] else fetchScenarios config ids
    games      <- fetchForResult config "game"      GAME      fetchManyGamesForSelection ids
    genres     <- fetchForResult config "genre"     GENRE     fetchManyGenresForSelection ids
    themes     <- fetchForResult config "theme"     THEME     fetchManyThemesForSelection ids
    mechanics  <- fetchForResult config "mechanic"  MECHANIC  fetchManyMechanicsForSelection ids
    sides      <- fetchForResult config "side"      SIDE      fetchManySidesForSelection ids
    parties    <- fetchForResult config "party"     PARTY     fetchManyPartiesForSelection ids
    publishers <- fetchForResult config "publisher" PUBLISHER fetchManyPublishersForSelection ids
    series     <- fetchForResult config "series"    SERIES    fetchManySeriesForSelection ids
    authors    <- fetchForResult config "author"    AUTHOR    fetchManyAuthorsForSelection ids
    engines    <- fetchForResult config "engine"    ENGINE    fetchManyEnginesForSelection ids
    leaders    <- fetchForResult config "leader"    LEADER    fetchManyLeadersForSelection ids
    latitudes  <- fetchSimpleValuesForResult config "latitude"  fetchManyLatitudesForSelection ids
    longitudes <- fetchSimpleValuesForResult config "longitude" fetchManyLongitudesForSelection ids
    fromYears  <- fetchSimpleValuesForResult config "fromYear"  fetchManyFromYearsForSelection ids
    upToYears  <- fetchSimpleValuesForResult config "upToYear"  fetchManyUpToYearsForSelection ids
    fromRanges <- fetchSimpleValuesForResult config "fromRange" fetchManyFromRangesForSelection ids
    upToRanges <- fetchSimpleValuesForResult config "upToRange" fetchManyUpToRangesForSelection ids
    fromTimescales <- fetchSimpleValuesForResult config "fromTimescale" fetchManyFromTimescalesForSelection ids
    upToTimescales <- fetchSimpleValuesForResult config "upToTimescale" fetchManyUpToTimescalesForSelection ids
    return $ Right Result { getNoResults  = numberOfResults
                              , getGames      = games
                              , getGenres     = genres
                              , getThemes     = themes
                              , getMechanics  = mechanics
                              , getSides      = sides
                              , getParties    = parties
                              , getPublishers = publishers
                              , getSeries     = series
                              , getAuthors    = authors
                              , getEngines    = engines
                              , getLeaders    = leaders
                              , getScenarios  = scenarios
                              , getLatitudes  = latitudes
                              , getLongitudes = longitudes
                              , getFromYears  = fromYears
                              , getUpToYears  = upToYears
                              , getFromRanges = fromRanges
                              , getUpToRanges = upToRanges
                              , getFromTimescales = fromTimescales
                              , getUpToTimescales = upToTimescales
                              }

fetchManyFromMapForSelection :: (FromRow a, FromRow b) => QueryCategory -> Config -> Int -> [Int] -> IO (AnswerList [a] [b])
fetchManyFromMapForSelection category config limit ids = do
    matches <- query c (qm ! (category, ENTRY, POLYB)) params
    count <- query c (qm ! (category, COUNT, POLY)) (Only (In ids))
    if head count < limit
      then liftM (\x -> Entries x matches) $ query c (qm ! (category, ENTRY, POLYA)) params
      else liftM (\x -> Groups x matches) $ query c (qm ! (category, GROUP, POLY)) params
    where c  = getDBConnection config
          qm = getQueryMap config
          params = (In ids, length ids)

fetchManyForSelection :: (FromRow a, FromRow b) => QueryCategory -> Config -> Int -> [Int] -> IO (AnswerList [a] [b])
fetchManyForSelection category config limit ids = do
    count <- query c (qm ! (category, COUNT, POLY)) params
    if head count < limit
      then liftM createEntries $ query c (qm ! (category, ENTRY, POLY)) params
      else liftM createGroups $ query c (qm ! (category, GROUP, POLY)) params
    where c  = getDBConnection config
          qm = getQueryMap config
          params = Only (In ids)

fetchDrilledAuthorGroup :: Config -> [Param] -> IO [Author]
fetchDrilledAuthorGroup = fetchDrilledLetterGroup "author"

fetchDrilledPublisherGroup :: Config -> [Param] -> IO [Publisher]
fetchDrilledPublisherGroup = fetchDrilledLetterGroup "publisher"

fetchDrilledGameGroup :: Config -> [Param] -> IO [Game]
fetchDrilledGameGroup = fetchDrilledLetterGroup "game"

fetchDrilledSeriesGroup :: Config -> [Param] -> IO [Series]
fetchDrilledSeriesGroup = fetchDrilledLetterGroup "series"

fetchDrilledSideGroup :: Config -> [Param] -> IO [Side]
fetchDrilledSideGroup = fetchDrilledLetterGroup "side"

fetchDrilledPartyGroup :: Config -> [Param] -> IO [Party]
fetchDrilledPartyGroup = fetchDrilledLetterGroup "party"

fetchDrilledMechanicGroup :: Config -> [Param] -> IO [Mechanic]
fetchDrilledMechanicGroup = fetchDrilledLetterGroup "mechanic"

fetchDrilledThemeGroup :: Config -> [Param] -> IO [Theme]
fetchDrilledThemeGroup = fetchDrilledLetterGroup "theme"

fetchDrilledLeaderGroup :: Config -> [Param] -> IO [Leader]
fetchDrilledLeaderGroup = fetchDrilledLetterGroup "leader"

fetchDrilledGenreGroup :: Config -> [Param] -> IO [Genre]
fetchDrilledGenreGroup = fetchDrilledLetterGroup "genre"

fetchDrilledEngineGroup :: Config -> [Param] -> IO [Engine]
fetchDrilledEngineGroup = fetchDrilledLetterGroup "engine"

fetchDrilledFromYearGroup :: Config -> [Param] -> IO [FromYear]
fetchDrilledFromYearGroup = fetchDrilledNumberGroup "fromYear"

fetchDrilledUpToYearGroup :: Config -> [Param] -> IO [UpToYear]
fetchDrilledUpToYearGroup = fetchDrilledNumberGroup "upToYear"

fetchDrilledLatitudeGroup :: Config -> [Param] -> IO [Latitude]
fetchDrilledLatitudeGroup = fetchDrilledNumberGroup "latitude"

fetchDrilledLongitudeGroup :: Config -> [Param] -> IO [Longitude]
fetchDrilledLongitudeGroup = fetchDrilledNumberGroup "longitude"

fetchDrilledUpToRangeGroup :: Config -> [Param] -> IO [UpToRange]
fetchDrilledUpToRangeGroup = fetchDrilledNumberGroup "upToRange"

fetchDrilledUpToTimescaleGroup :: Config -> [Param] -> IO [FromTimescale]
fetchDrilledUpToTimescaleGroup = fetchDrilledNumberGroup "fromTimescale"

fetchDrilledFromRangeGroup :: Config -> [Param] -> IO [UpToRange]
fetchDrilledFromRangeGroup = fetchDrilledNumberGroup "upToRange"

fetchDrilledFromTimescaleGroup :: Config -> [Param] -> IO [FromTimescale]
fetchDrilledFromTimescaleGroup = fetchDrilledNumberGroup "fromTimescale"

fetchDrilledLetterGroup :: FromRow a => T.Text -> Config -> [Param] -> IO [a]
fetchDrilledLetterGroup = fetchDrilledGroup False

fetchDrilledNumberGroup :: FromRow a => T.Text -> Config -> [Param] -> IO [a]
fetchDrilledNumberGroup = fetchDrilledGroup True

fetchManyAuthorsForSelection :: Config -> Int -> [Int] -> IO AuthorList
fetchManyAuthorsForSelection = fetchManyFromMapForSelection AUTHOR

fetchManyGamesForSelection :: Config -> Int -> [Int] -> IO GameList
fetchManyGamesForSelection = fetchManyForSelection GAME

fetchManyGenresForSelection :: Config -> Int -> [Int] -> IO GenreList
fetchManyGenresForSelection = fetchManyFromMapForSelection GENRE

fetchManyThemesForSelection :: Config -> Int -> [Int] -> IO ThemeList
fetchManyThemesForSelection = fetchManyFromMapForSelection THEME

fetchManyMechanicsForSelection :: Config -> Int -> [Int] -> IO MechanicList
fetchManyMechanicsForSelection = fetchManyFromMapForSelection MECHANIC

fetchManySidesForSelection :: Config -> Int -> [Int] -> IO SideList
fetchManySidesForSelection = fetchManyFromMapForSelection SIDE

fetchManyPartiesForSelection :: Config -> Int -> [Int] -> IO PartyList
fetchManyPartiesForSelection = fetchManyFromMapForSelection PARTY

fetchManyPublishersForSelection :: Config -> Int -> [Int] -> IO PublisherList
fetchManyPublishersForSelection = fetchManyFromMapForSelection PUBLISHER

fetchManySeriesForSelection :: Config -> Int -> [Int] -> IO SeriesList
fetchManySeriesForSelection = fetchManyFromMapForSelection SERIES

fetchManyEnginesForSelection :: Config -> Int -> [Int] -> IO EngineList
fetchManyEnginesForSelection = fetchManyFromMapForSelection ENGINE

fetchManyLeadersForSelection :: Config -> Int -> [Int] -> IO LeaderList
fetchManyLeadersForSelection = fetchManyFromMapForSelection LEADER

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

fetchAllAuthorEntries :: Config -> IO [Author]
fetchAllAuthorEntries = fetchAllEntries AUTHOR

fetchAllEngineEntries :: Config -> IO [Engine]
fetchAllEngineEntries = fetchAllEntries ENGINE

fetchAllFromRangeEntries :: Config -> IO [FromRange]
fetchAllFromRangeEntries = fetchAllEntries FROM_RANGE

fetchAllFromTimescaleEntries :: Config -> IO [FromTimescale]
fetchAllFromTimescaleEntries = fetchAllEntries FROM_TIMESCALE

fetchAllFromYearEntries :: Config -> IO [FromYear]
fetchAllFromYearEntries = fetchAllEntries FROM_YEAR

fetchAllGameEntries :: Config -> IO [Game]
fetchAllGameEntries = fetchAllEntries GAME

fetchAllGenreEntries :: Config -> IO [Genre]
fetchAllGenreEntries = fetchAllEntries GENRE

fetchAllLatitudeEntries :: Config -> IO [Latitude]
fetchAllLatitudeEntries = fetchAllEntries LATITUDE

fetchAllLeaderEntries :: Config -> IO [Leader]
fetchAllLeaderEntries = fetchAllEntries LEADER

fetchAllLongitudeEntries :: Config -> IO [Longitude]
fetchAllLongitudeEntries = fetchAllEntries LONGITUDE

fetchAllMechanicEntries :: Config -> IO [Mechanic]
fetchAllMechanicEntries = fetchAllEntries MECHANIC

fetchAllPartyEntries :: Config -> IO [Party]
fetchAllPartyEntries = fetchAllEntries PARTY

fetchAllPublisherEntries :: Config -> IO [Publisher]
fetchAllPublisherEntries = fetchAllEntries PUBLISHER

fetchAllSeriesEntries :: Config -> IO [Series]
fetchAllSeriesEntries = fetchAllEntries SERIES

fetchAllSideEntries :: Config -> IO [Side]
fetchAllSideEntries = fetchAllEntries SIDE

fetchAllThemeEntries :: Config -> IO [Theme]
fetchAllThemeEntries = fetchAllEntries THEME

fetchAllUpToRangeEntries :: Config -> IO [UpToRange]
fetchAllUpToRangeEntries = fetchAllEntries UPTO_RANGE

fetchAllUpToTimescaleEntries :: Config -> IO [UpToTimescale]
fetchAllUpToTimescaleEntries = fetchAllEntries UPTO_TIMESCALE

fetchAllUpToYearEntries :: Config -> IO [UpToYear]
fetchAllUpToYearEntries = fetchAllEntries UPTO_YEAR

fetchAuthorGroups :: Config -> IO [GroupLetter]
fetchAuthorGroups = fetchGroups AUTHOR

fetchEngineGroups :: Config -> IO [GroupLetter]
fetchEngineGroups = fetchGroups ENGINE

fetchFromYearGroups :: Config -> IO [GroupNumber]
fetchFromYearGroups = fetchGroups FROM_YEAR

fetchGameGroups :: Config -> IO [GroupLetter]
fetchGameGroups = fetchGroups GAME

fetchGenreGroups :: Config -> IO [GroupLetter]
fetchGenreGroups = fetchGroups GENRE

fetchLatitudeGroups :: Config -> IO [GroupNumber]
fetchLatitudeGroups = fetchGroups LATITUDE

fetchLeaderGroups :: Config -> IO [GroupLetter]
fetchLeaderGroups = fetchGroups LEADER

fetchLongitudeGroups :: Config -> IO [GroupNumber]
fetchLongitudeGroups = fetchGroups LONGITUDE

fetchMechanicGroups :: Config -> IO [GroupLetter]
fetchMechanicGroups = fetchGroups MECHANIC

fetchPartyGroups :: Config -> IO [GroupLetter]
fetchPartyGroups = fetchGroups PARTY

fetchPublisherGroups :: Config -> IO [GroupLetter]
fetchPublisherGroups = fetchGroups PUBLISHER

fetchRangeGroups :: Config -> IO [GroupNumber]
fetchRangeGroups = fetchGroups RANGE

fetchSeriesGroups :: Config -> IO [GroupLetter]
fetchSeriesGroups = fetchGroups SERIES

fetchSideGroups :: Config -> IO [GroupLetter]
fetchSideGroups = fetchGroups SIDE

fetchThemeGroups :: Config -> IO [GroupLetter]
fetchThemeGroups = fetchGroups THEME

fetchTimescaleGroups :: Config -> IO [GroupNumber]
fetchTimescaleGroups = fetchGroups TIMESCALE

fetchUpToYearGroups :: Config -> IO [UpToYear]
fetchUpToYearGroups = fetchGroups UPTO_YEAR

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

fetchDrilledGroupEntries :: FromRow r => Config -> T.Text -> [Parameter] -> IO [r]
fetchDrilledGroupEntries c cat ps = query (getDBConnection c) (groupQuery c cat ps) (map snd ps)

fetchScenarioIds :: Config -> [Parameter] -> IO [Int]
fetchScenarioIds c ps = query (getDBConnection c) (scenarioListQuery c ps) (map snd ps)
