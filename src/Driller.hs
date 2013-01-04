{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Driller.DB as DB
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Database.PostgreSQL.Simple ( connect )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( ToJSON )
-- import System.Remote.Monitoring ( forkServer )
import Web.Scotty
    ( ScottyM
    , RoutePattern
    , Parsable
    , get
    , text
    , params
    , param
    , json
    , scotty
    , middleware
    )

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

main :: IO ()
main = do
  -- _ <- forkServer "localhost" 8000
  conn <- connect DB.connectionInfo
  let joinMap = DB.initJoinMap
  scotty 3003 $ do
    middleware logStdoutDev

    get "/q" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledGameResult conn joinMap p
      json result
    get "/"                                        $ text "No service at this level."
    getRouteWithoutParameter "/authorsPD"          $ DB.fetchAuthorsForSelection conn
    getRouteWithoutParameter "/authors"            $ DB.fetchAllAuthors conn
    getRouteWithParameter    "/author/:id"         $ DB.fetchAuthor conn
    getRouteWithoutParameter "/authorGroups"       $ DB.fetchAuthorGroups conn
    getRouteWithParameter    "/authorGroup/:id"    $ DB.fetchAuthorGroup conn
    getRouteWithoutParameter "/engines"            $ DB.fetchAllEngines conn
    getRouteWithParameter    "/engine/:id"         $ DB.fetchEngine conn
    getRouteWithoutParameter "/engineGroups"       $ DB.fetchEngineGroups conn
    getRouteWithParameter    "/engineGroup/:id"    $ DB.fetchEngineGroup conn
    getRouteWithoutParameter "/sides"              $ DB.fetchAllSides conn
    getRouteWithParameter    "/side/:id"           $ DB.fetchSide conn
    getRouteWithoutParameter "/sideGroups"         $ DB.fetchSideGroups conn
    getRouteWithParameter    "/sideGroup/:id"      $ DB.fetchSideGroup conn
    getRouteWithoutParameter "/publishers"         $ DB.fetchAllPublishers conn
    getRouteWithParameter    "/publisher/:id"      $ DB.fetchPublisher conn
    getRouteWithoutParameter "/publisherGroups"    $ DB.fetchPublisherGroups conn
    getRouteWithParameter    "/publisherGroup/:id" $ DB.fetchPublisherGroup conn
    getRouteWithoutParameter "/mechanics"          $ DB.fetchAllMechanics conn
    getRouteWithParameter    "/mechanic/:id"       $ DB.fetchMechanic conn
    getRouteWithoutParameter "/mechanicGroups"     $ DB.fetchMechanicGroups conn
    getRouteWithParameter    "/mechanicGroup/:id"  $ DB.fetchMechanicGroup conn
    getRouteWithoutParameter "/games"              $ DB.fetchAllGames conn
    getRouteWithParameter    "/game/:id"           $ DB.fetchGame conn
    getRouteWithoutParameter "/gameGroups"         $ DB.fetchGameGroups conn
    getRouteWithParameter    "/gameGroup/:id"      $ DB.fetchGameGroup conn
    getRouteWithoutParameter "/genres"             $ DB.fetchAllGenres conn
    getRouteWithParameter    "/genre/:id"          $ DB.fetchGenre conn
    getRouteWithoutParameter "/genreGroups"        $ DB.fetchGenreGroups conn
    getRouteWithParameter    "/genreGroup/:id"     $ DB.fetchGenreGroup conn
    getRouteWithoutParameter "/themes"             $ DB.fetchAllThemes conn
    getRouteWithParameter    "/theme/:id"          $ DB.fetchTheme conn
    getRouteWithoutParameter "/themeGroups"        $ DB.fetchThemeGroups conn
    getRouteWithParameter    "/themeGroup/:id"     $ DB.fetchThemeGroup conn
    getRouteWithoutParameter "/parties"            $ DB.fetchAllParties conn
    getRouteWithParameter    "/party/:id"          $ DB.fetchParty conn
    getRouteWithoutParameter "/partieGroups"       $ DB.fetchPartieGroups conn
    getRouteWithParameter    "/partyGroup/:id"     $ DB.fetchPartyGroup conn
    getRouteWithoutParameter "/series"             $ DB.fetchAllSeries conn
    getRouteWithParameter    "/serie/:id"          $ DB.fetchSeries conn
    getRouteWithoutParameter "/serieGroups"        $ DB.fetchSeriesGroups conn
    getRouteWithParameter    "/serieGroup/:id"     $ DB.fetchSeriesGroup conn
    getRouteWithoutParameter "/leaders"            $ DB.fetchAllLeaders conn
    getRouteWithParameter    "/leader/:id"         $ DB.fetchLeader conn
    getRouteWithoutParameter "/leaderGroups"       $ DB.fetchLeaderGroups conn
    getRouteWithParameter    "/leaderGroup/:id"    $ DB.fetchLeaderGroup conn
    getRouteWithoutParameter "/fromYears"          $ DB.fetchAllFromYears conn
    getRouteWithParameter    "/fromYear/:id"       $ DB.fetchFromYear conn
    getRouteWithoutParameter "/fromYearGroups"     $ DB.fetchFromYearGroups conn
    getRouteWithParameter    "/fromYearGroup/:id"  $ DB.fetchFromYearGroup conn
    getRouteWithoutParameter "/upToYears"          $ DB.fetchAllUpToYears conn
    getRouteWithParameter    "/upToYear/:id"       $ DB.fetchUpToYear conn
    getRouteWithoutParameter "/upToYearGroups"     $ DB.fetchUpToYearGroups conn
    getRouteWithParameter    "/upToYearGroup/:id"  $ DB.fetchUpToYearGroup conn
    getRouteWithoutParameter "/latitudes"          $ DB.fetchAllLatitudes conn
    getRouteWithParameter    "/latitude/:id"       $ DB.fetchLatitude conn
    getRouteWithoutParameter "/latitudeGroups"     $ DB.fetchLatitudeGroups conn
    getRouteWithParameter    "/latitudeGroups/:id" $ DB.fetchLatitudeGroup conn
    getRouteWithoutParameter "/longitudes"         $ DB.fetchAllLongitudes conn
    getRouteWithParameter    "/longitude/:id"      $ DB.fetchLongitude conn
    getRouteWithoutParameter "/longitudeGroups"    $ DB.fetchLongitudeGroups conn
    getRouteWithParameter    "/longitudeGroups/:id"$ DB.fetchLongitudeGroup conn
    getRouteWithoutParameter "/fromRanges"         $ DB.fetchAllFromRanges conn
    getRouteWithParameter    "/fromRange/:id"      $ DB.fetchFromRange conn
    getRouteWithoutParameter "/rangeGroups"        $ DB.fetchRangeGroups conn
    getRouteWithParameter    "/rangeGroup/:id"     $ DB.fetchRangeGroup conn
    getRouteWithoutParameter "/upToRanges"         $ DB.fetchAllUpToRanges conn
    getRouteWithParameter    "/upToRange/:id"      $ DB.fetchUpToRange conn
    getRouteWithoutParameter "/fromTimescales"     $ DB.fetchAllFromTimescales conn
    getRouteWithParameter    "/fromTimescale/:id"  $ DB.fetchFromTimescale conn
    getRouteWithoutParameter "/timescaleGroups"    $ DB.fetchTimescaleGroups conn
    getRouteWithParameter    "/timescaleGroup/:id" $ DB.fetchTimescaleGroup conn
    getRouteWithoutParameter "/upToTimescales"     $ DB.fetchAllUpToTimescales conn
    getRouteWithParameter    "/upToTimescale/:id"  $ DB.fetchUpToTimescale conn
    getRouteWithoutParameter "/scenarios"          $ DB.fetchAllScenarios conn
    getRouteWithParameter    "/scenario/:id"       $ DB.fetchScenario conn
