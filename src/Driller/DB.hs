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
import qualified Driller.Error as Error
import Driller.DB.Queries

import Data.Hashable()
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy as TL ( toStrict )
import Data.Text.Lazy.Internal()
import qualified Data.HashMap.Strict as HM ( fromList, lookup, member )
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

fetchForResult :: ParameterMap -> T.Text -> (Connection -> Int -> t) -> (Connection -> [Int] -> t) -> Connection -> [Int] -> t
fetchForResult parameterMap key fetchOne fetchMany c ids
    = case HM.lookup key parameterMap of
        Just value  -> fetchOne c value
        Nothing     -> fetchMany c ids

fetchSimpleValuesForResult :: (FromInt t, Monad m) => ParameterMap -> T.Text -> (Connection -> [Int] -> m [t]) -> Connection -> [Int] -> m [t]
fetchSimpleValuesForResult parameterMap key fetchMany c ids
    = case HM.lookup key parameterMap of
        Just value  -> return [fromInt value]
        Nothing     -> fetchMany c ids

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
convertValue "latitude"  t = getFromParser (TR.signed TR.decimal t) >>= withinLimits (negate 90) 90
convertValue "longitude" t = getFromParser (TR.signed TR.decimal t) >>= withinLimits (negate 180) 180
convertValue _ t           = getFromParser (TR.decimal t)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r))
           | T.length r == 0 = Just n
           | otherwise       = Nothing

withinLimits :: Int -> Int -> Int -> Maybe Int
withinLimits lower upper value | value >= lower && value <= upper = Just value
                               | otherwise                    = Nothing

fetchDrilledGameResult :: JoinMap -> Connection -> [Param] -> IO Answer
fetchDrilledGameResult joinMap c p = do
    let filteredParameters = filterParameters p joinMap
    case filteredParameters of
        Left e          -> return $ Left e
        Right parameter -> fetchPositiveAnswer joinMap c parameter

fetchPositiveAnswer :: JoinMap -> Connection -> [Parameter] -> IO Answer
fetchPositiveAnswer joinMap c p = do
    let (keys, values)     = unzip p
        que = gameListQuery joinMap keys
    ids        <- query c que values
    if null ids
       then return $ Right emptyGameResult
       else prepareResult (HM.fromList p) c ids

prepareResult :: ParameterMap -> Connection -> [Int] -> IO Answer
prepareResult parameterMap c ids = do
    games      <- fetchGames c ids
    genres     <- fetchForResult parameterMap "genre" fetchGenre fetchGenres c ids
    themes     <- fetchForResult parameterMap "theme" fetchTheme fetchThemes c ids
    mechanics  <- fetchForResult parameterMap "mechanic" fetchMechanic fetchMechanics c ids
    sides      <- fetchForResult parameterMap "side" fetchSide fetchSides c ids
    parties    <- fetchForResult parameterMap "party" fetchParty fetchParties c ids
    publishers <- fetchForResult parameterMap "publisher" fetchPublisher fetchPublishers c ids
    series     <- fetchForResult parameterMap "series" fetchSeries fetchSeriess c ids
    authors    <- fetchForResult parameterMap "author" fetchAuthor fetchAuthors c ids
    engines    <- fetchForResult parameterMap "engine" fetchEngine fetchEngines c ids
    leaders    <- fetchForResult parameterMap "leader" fetchLeader fetchLeaders c ids
    latitudes  <- fetchSimpleValuesForResult parameterMap "latitude" fetchLatitudes c ids
    longitudes <- fetchSimpleValuesForResult parameterMap "longitude" fetchLongitudes c ids
    fromYears  <- fetchSimpleValuesForResult parameterMap "fromYear" fetchFromYears c ids
    upToYears  <- fetchSimpleValuesForResult parameterMap "upToYear" fetchUpToYears c ids
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
