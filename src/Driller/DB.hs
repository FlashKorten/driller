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
    ( ConnectInfo(connectDatabase, connectPassword, connectUser)
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

fetchDrilledGameResult :: Config -> [Param] -> IO Answer
fetchDrilledGameResult config p =
    case filterParameters p (getJoinMap config) of
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
        Just value -> if value >= 0
                        then liftM Entries $ fetchEntry config cat value
                        else liftM (Entries .  markExclusive) (fetchEntry config cat (negate value))
        Nothing    -> f config 25 ids

fetchSimpleValuesForResult ::
  (FromInt r) => Config -> T.Text
      -> (Config -> Int -> [Int] -> IO (AnswerList a [r]))
      -> [Int] -> IO (AnswerList a [r])

fetchSimpleValuesForResult config key f ids
    = case HM.lookup key $ getParameterMap config of
        Just value -> return $ Entries [fromInt value]
        Nothing    -> f config 25 ids

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
