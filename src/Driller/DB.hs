{-# LANGUAGE OverloadedStrings #-}
module Driller.DB
    ( fetchTheme
    , fetchSide
    , fetchPublisher
    , fetchParty
    , fetchMechanic
    , fetchGenre
    , fetchGame
    , fetchEngine
    , fetchDrilledGameResult
    , fetchAuthor
    , fetchSeries
    , fetchAllThemes
    , fetchAllSides
    , fetchAllPublishers
    , fetchAllParties
    , fetchAllMechanics
    , fetchAllGenres
    , fetchAllGames
    , fetchAllEngines
    , fetchAllAuthors
    , fetchAllSeries
    , connectionInfo
    , initJoinMap
    ) where

import Driller.Data
import qualified Driller.Error as Error
import Driller.DB.Queries

import qualified Data.Hashable as H
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy as TL ( toStrict )
import Data.Text.Lazy.Internal()
import qualified Data.HashMap.Strict as HM ( HashMap, fromList, lookup, member )
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

fetchForResult :: (Eq k, H.Hashable k) => HM.HashMap k v -> k -> (t1 -> v -> t) -> (t1 -> t2 -> t) -> t1 -> t2 -> t
fetchForResult parameterMap key fetchOne fetchMany c ids
    = case HM.lookup key parameterMap of
        Just value  -> fetchOne c value
        Nothing     -> fetchMany c ids

filterParameters :: [Param] -> JoinMap -> Either Error.ParameterError [(T.Text, Int)]
filterParameters [] _ = Right []
filterParameters p jm = filterParameters' p jm []

filterParameters' :: [Param] -> JoinMap -> [(T.Text, Int)] -> Either Error.ParameterError [(T.Text, Int)]
filterParameters' [] _ result        = Right result
filterParameters' ((k, v):ps) jm tmp | not $ HM.member key jm = Left $ Error.unknownParameter key
                                     | isNothing value        = Left $ Error.illegalValueFormat key
                                     | otherwise              = filterParameters' ps jm ((key, fromJust value):tmp)
                                    where key   = TL.toStrict k
                                          value = convertValue key (TL.toStrict v)

convertValue :: T.Text -> T.Text -> Maybe Int
convertValue "latitude"  t = getFromParser (TR.signed TR.decimal t)
convertValue "longitude" t = getFromParser (TR.signed TR.decimal t)
convertValue _ t           = getFromParser (TR.decimal t)

getFromParser :: Either String (Int, T.Text) -> Maybe Int
getFromParser (Left _)       = Nothing
getFromParser (Right (n, r))
           | T.length r == 0 = Just n
           | otherwise       = Nothing


fetchDrilledGameResult :: JoinMap -> Connection -> [Param] -> IO Answer
fetchDrilledGameResult joinMap c p = do
    let filteredParameters = filterParameters p joinMap
    case filteredParameters of
        Left e          -> return $ Left e
        Right parameter -> fetchPositiveAnswer joinMap c parameter

fetchPositiveAnswer :: JoinMap -> Connection -> [(T.Text, Int)] -> IO Answer
fetchPositiveAnswer joinMap c p = do
    let (keys, values)     = unzip p
        que = gameListQuery joinMap keys
    ids        <- query c que values
    if null ids
       then return $ Right emptyGameResult
       else prepareResult (HM.fromList p) c ids

prepareResult :: HM.HashMap T.Text Int -> Connection -> [Int] -> IO Answer
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
                      }
