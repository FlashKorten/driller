{-# LANGUAGE OverloadedStrings #-}
module Driller.DB
    ( connectionInfo
    , fetchDrilledGameResult
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
    , fetchEntry
    , fetchAllEntries
    , fetchGroup
    , fetchGroups
    , initJoinMap
    , initQueryMap
    , convertValue
    ) where

import Driller.Data
import qualified Driller.Error as Error
import Driller.DB.Wrapper
import Driller.DB.Queries ( initJoinMap, initQueryMap )
import Control.Monad ( liftM )
import Data.Hashable ()
import Data.Maybe ( isNothing, fromJust )
import Data.Text.Lazy.Internal ()
import qualified Data.Text as T ( Text, length )
import qualified Data.Text.Read as TR ( signed, decimal )
import qualified Data.Text.Lazy as TL ( toStrict )
import qualified Data.HashMap.Strict as HM
    ( fromList, lookup, member )
import Web.Scotty ( Param )
import Database.PostgreSQL.Simple
    ( Connection
    , ConnectInfo(connectDatabase, connectPassword, connectUser)
    , defaultConnectInfo
    )

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser     = "driller"
                                    , connectPassword = "driller"
                                    , connectDatabase = "dr"
                                    }

filterParameters :: [Param] -> JoinMap -> Either Error.ParameterError [Parameter]
filterParameters p jm = filterParameters' p jm []

filterParameters' :: [Param] -> JoinMap -> [Parameter] -> Either Error.ParameterError [Parameter]
filterParameters' [] _ result        = Right result
filterParameters' ((k, v):ps) jm tmp | not $ HM.member key jm = Left $ Error.unknownParameter key
                                     | isNothing value        = Left $ Error.illegalValue key
                                     | alreadySeen key tmp    = Left $ Error.duplicateParameter key
                                     | otherwise              = filterParameters' ps jm ((key, fromJust value):tmp)
                                    where key   = TL.toStrict k
                                          value = convertValue key (TL.toStrict v)

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

fetchDrilledGameResult :: Connection -> JoinMap -> QueryMap -> [Param] -> IO Answer
fetchDrilledGameResult c joinMap queryMap p =
    case filterParameters p joinMap of
        Left e      -> return $ Left e
        Right pList -> fetchPositiveAnswer c joinMap queryMap pList

fetchPositiveAnswer :: Connection -> JoinMap -> QueryMap -> [Parameter] -> IO Answer
fetchPositiveAnswer c joinMap queryMap p = do
    ids <- fetchScenarioIds c joinMap p
    if null ids
       then return $ Right emptyResult
       else prepareResult (HM.fromList p) queryMap c ids

fetchForResult ::
  (FromRow r, MarkExclusive r) =>
     ParameterMap -> QueryMap -> T.Text -> QueryCategory
       -> (QueryMap -> Connection -> Int -> [Int] -> IO (AnswerList a [r])) -> Connection -> [Int] -> IO (AnswerList a [r])

fetchForResult parameterMap queryMap key cat f c ids
    = case HM.lookup key parameterMap of
        Just value -> if value >= 0
                        then liftM Entries $ fetchEntry cat c queryMap value
                        else liftM (Entries .  markExclusive) (fetchEntry cat c queryMap (negate value))
        Nothing    -> f queryMap c 25 ids

fetchSimpleValuesForResult ::
  (Monad m, FromInt t1) =>
    ParameterMap -> QueryMap -> T.Text
      -> (QueryMap -> Connection -> Int -> [Int] -> m (AnswerList a [t1])) -> Connection -> [Int] -> m (AnswerList a [t1])

fetchSimpleValuesForResult parameterMap queryMap key f c ids
    = case HM.lookup key parameterMap of
        Just value -> return $ Entries [fromInt value]
        Nothing    -> f queryMap c 25 ids

prepareResult :: ParameterMap -> QueryMap -> Connection -> [Int] -> IO Answer
prepareResult parameterMap queryMap c ids = do
    let numberOfResults = length ids
    scenarios  <- if numberOfResults > 50 then return [] else fetchScenarios c queryMap ids
    games      <- fetchForResult parameterMap queryMap "game"      GAME      fetchManyGamesForSelection c ids
    genres     <- fetchForResult parameterMap queryMap "genre"     GENRE     fetchManyGenresForSelection c ids
    themes     <- fetchForResult parameterMap queryMap "theme"     THEME     fetchManyThemesForSelection c ids
    mechanics  <- fetchForResult parameterMap queryMap "mechanic"  MECHANIC  fetchManyMechanicsForSelection c ids
    sides      <- fetchForResult parameterMap queryMap "side"      SIDE      fetchManySidesForSelection c ids
    parties    <- fetchForResult parameterMap queryMap "party"     PARTY     fetchManyPartiesForSelection c ids
    publishers <- fetchForResult parameterMap queryMap "publisher" PUBLISHER fetchManyPublishersForSelection c ids
    series     <- fetchForResult parameterMap queryMap "series"    SERIES    fetchManySeriesForSelection c ids
    authors    <- fetchForResult parameterMap queryMap "author"    AUTHOR    fetchManyAuthorsForSelection c ids
    engines    <- fetchForResult parameterMap queryMap "engine"    ENGINE    fetchManyEnginesForSelection c ids
    leaders    <- fetchForResult parameterMap queryMap "leader"    LEADER    fetchManyLeadersForSelection c ids
    latitudes  <- fetchSimpleValuesForResult parameterMap queryMap "latitude"  fetchManyLatitudesForSelection c ids
    longitudes <- fetchSimpleValuesForResult parameterMap queryMap "longitude" fetchManyLongitudesForSelection c ids
    fromYears  <- fetchSimpleValuesForResult parameterMap queryMap "fromYear"  fetchManyFromYearsForSelection c ids
    upToYears  <- fetchSimpleValuesForResult parameterMap queryMap "upToYear"  fetchManyUpToYearsForSelection c ids
    fromRanges <- fetchSimpleValuesForResult parameterMap queryMap "fromRange" fetchManyFromRangesForSelection c ids
    upToRanges <- fetchSimpleValuesForResult parameterMap queryMap "upToRange" fetchManyUpToRangesForSelection c ids
    fromTimescales <- fetchSimpleValuesForResult parameterMap queryMap "fromTimescale" fetchManyFromTimescalesForSelection c ids
    upToTimescales <- fetchSimpleValuesForResult parameterMap queryMap "upToTimescale" fetchManyUpToTimescalesForSelection c ids
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
