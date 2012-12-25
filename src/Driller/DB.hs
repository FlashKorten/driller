{-# LANGUAGE OverloadedStrings #-}
module Driller.DB
    ( connectionInfo
    , fetchAllAuthors
    , fetchAllEngines
    , fetchAllGames
    , fetchAllGenres
    , fetchAllLatitudes
    , fetchAllLeaders
    , fetchAllLongitudes
    , fetchAllMechanics
    , fetchAllParties
    , fetchAllPublishers
    , fetchAllFromRanges
    , fetchAllUpToRanges
    , fetchAllSeries
    , fetchAllSides
    , fetchAllThemes
    , fetchAllFromYears
    , fetchAllUpToYears
    , fetchAuthor
    , fetchDrilledGameResult
    , fetchEngine
    , fetchGame
    , fetchGenre
    , fetchLatitude
    , fetchLatitudes
    , fetchLeader
    , fetchLongitude
    , fetchLongitudes
    , fetchMechanic
    , fetchParty
    , fetchPublisher
    , fetchFromRange
    , fetchFromRanges
    , fetchUpToRange
    , fetchUpToRanges
    , fetchSeries
    , fetchSide
    , fetchTheme
    , fetchFromYear
    , fetchFromYears
    , fetchUpToYear
    , fetchUpToYears
    , initJoinMap
    ) where

import Driller.Data
import qualified Driller.Error as Error ( ParameterError, unknownParameter, illegalValue )
import Driller.DB.Wrapper
import Driller.DB.Queries (gameListQuery, initJoinMap)
import Control.Monad (liftM)
import Data.Hashable ()
import Data.Maybe ( isNothing, fromJust )
import Data.Text.Lazy.Internal ()
import qualified Data.Text as T ( Text, length )
import qualified Data.Text.Read as TR ( signed, decimal )
import qualified Data.Text.Lazy as TL ( toStrict )
import qualified Data.HashMap.Strict as HM ( fromList, lookup, member )
import Web.Scotty ( Param )
import Database.PostgreSQL.Simple
    ( Connection
    , ConnectInfo(connectDatabase, connectPassword, connectUser)
    , defaultConnectInfo
    , query
    )

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser = "nemesis"
                                    , connectPassword = "nemesis"
                                    , connectDatabase = "nn"
                                    }

fetchForResult :: (Monad m, MarkExclusive b)
               => ParameterMap -> T.Text -> (Connection -> Int -> m [b]) -> (Connection -> [Int] -> m [b]) -> Connection -> [Int] -> m [b]
fetchForResult parameterMap key fetchOne fetchMany c ids
    = case HM.lookup key parameterMap of
        Just value -> if value >= 0
                        then fetchOne c value
                        else liftM markExclusive (fetchOne c (negate value))
        Nothing    -> fetchMany c ids

fetchSimpleValuesForResult :: (FromInt t, Monad m) => ParameterMap -> T.Text -> (Connection -> [Int] -> m [t]) -> Connection -> [Int] -> m [t]
fetchSimpleValuesForResult parameterMap key fetchMany c ids
    = case HM.lookup key parameterMap of
        Just value -> return [fromInt value]
        Nothing    -> fetchMany c ids

filterParameters :: [Param] -> JoinMap -> Either Error.ParameterError [Parameter]
filterParameters [] _ = Right []
filterParameters p jm = filterParameters' p jm []

filterParameters' :: [Param] -> JoinMap -> [Parameter] -> Either Error.ParameterError [Parameter]
filterParameters' [] _ result        = Right result
filterParameters' ((k, v):ps) jm tmp | not $ HM.member key jm = Left $ Error.unknownParameter key
                                     | isNothing value        = Left $ Error.illegalValue key
                                     | otherwise              = filterParameters' ps jm ((key, fromJust value):tmp)
                                    where key   = TL.toStrict k
                                          value = convertValue key (TL.toStrict v)

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

fetchDrilledGameResult :: Connection -> JoinMap -> [Param] -> IO Answer
fetchDrilledGameResult c joinMap p =
    case filterParameters p joinMap of
        Left e      -> return $ Left e
        Right pList -> fetchPositiveAnswer c joinMap pList

fetchPositiveAnswer :: Connection -> JoinMap -> [Parameter] -> IO Answer
fetchPositiveAnswer c joinMap p = do
    ids <- query c (gameListQuery joinMap p) (map snd p)
    if null ids
       then return $ Right emptyGameResult
       else prepareResult (HM.fromList p) c ids

prepareResult :: ParameterMap -> Connection -> [Int] -> IO Answer
prepareResult parameterMap c ids = do
    games      <- if length ids > 50 then return [] else fetchGames c ids
    genres     <- fetchForResult parameterMap "genre"     fetchGenre     fetchGenres     c ids
    themes     <- fetchForResult parameterMap "theme"     fetchTheme     fetchThemes     c ids
    mechanics  <- fetchForResult parameterMap "mechanic"  fetchMechanic  fetchMechanics  c ids
    sides      <- fetchForResult parameterMap "side"      fetchSide      fetchSides      c ids
    parties    <- fetchForResult parameterMap "party"     fetchParty     fetchParties    c ids
    publishers <- fetchForResult parameterMap "publisher" fetchPublisher fetchPublishers c ids
    series     <- fetchForResult parameterMap "series"    fetchSeries    fetchSeriess    c ids
    authors    <- fetchForResult parameterMap "author"    fetchAuthor    fetchAuthors    c ids
    engines    <- fetchForResult parameterMap "engine"    fetchEngine    fetchEngines    c ids
    leaders    <- fetchForResult parameterMap "leader"    fetchLeader    fetchLeaders    c ids
    latitudes  <- fetchSimpleValuesForResult parameterMap "latitude"  fetchLatitudes  c ids
    longitudes <- fetchSimpleValuesForResult parameterMap "longitude" fetchLongitudes c ids
    fromYears  <- fetchSimpleValuesForResult parameterMap "fromYear"  fetchFromYears  c ids
    upToYears  <- fetchSimpleValuesForResult parameterMap "upToYear"  fetchUpToYears  c ids
    fromRanges <- fetchSimpleValuesForResult parameterMap "fromRange" fetchFromRanges c ids
    upToRanges <- fetchSimpleValuesForResult parameterMap "upToRange" fetchUpToRanges c ids
    return $ Right GameResult { getGames      = games
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
                              , getLatitudes  = latitudes
                              , getLongitudes = longitudes
                              , getFromYears  = fromYears
                              , getUpToYears  = upToYears
                              , getFromRanges = fromRanges
                              , getUpToRanges = upToRanges
                              }
