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

    get "/g" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledGameResult conn joinMap p
      json result
    get "/"                                    $ text "No service at this level."
    getRouteWithoutParameter "/authors"        $ DB.fetchAllAuthors conn
    getRouteWithParameter    "/authors/:id"    $ DB.fetchAuthor conn
    getRouteWithoutParameter "/authorsToc"     $ DB.fetchAuthorsToc conn
    getRouteWithParameter    "/authorsToc/:id" $ DB.fetchAuthorSection conn
    getRouteWithoutParameter "/engines"        $ DB.fetchAllEngines conn
    getRouteWithParameter    "/engines/:id"    $ DB.fetchEngine conn
    getRouteWithoutParameter "/sides"          $ DB.fetchAllSides conn
    getRouteWithParameter    "/sides/:id"      $ DB.fetchSide conn
    getRouteWithoutParameter "/sidesToc"       $ DB.fetchSidesToc conn
    getRouteWithParameter    "/sidesToc/:id"   $ DB.fetchSideSection conn
    getRouteWithoutParameter "/publishers"     $ DB.fetchAllPublishers conn
    getRouteWithParameter    "/publishers/:id" $ DB.fetchPublisher conn
    getRouteWithoutParameter "/mechanics"      $ DB.fetchAllMechanics conn
    getRouteWithParameter    "/mechanics/:id"  $ DB.fetchMechanic conn
    getRouteWithoutParameter "/games"          $ DB.fetchAllGames conn
    getRouteWithParameter    "/games/:id"      $ DB.fetchGame conn
    getRouteWithoutParameter "/gamesToc"       $ DB.fetchGamesToc conn
    getRouteWithParameter    "/gamesToc/:id"   $ DB.fetchGameSection conn
    getRouteWithoutParameter "/scenarios"      $ DB.fetchAllScenarios conn
    getRouteWithParameter    "/scenarios/:id"  $ DB.fetchScenario conn
    getRouteWithoutParameter "/genres"         $ DB.fetchAllGenres conn
    getRouteWithParameter    "/genres/:id"     $ DB.fetchGenre conn
    getRouteWithoutParameter "/genresToc"      $ DB.fetchGenresToc conn
    getRouteWithParameter    "/genresToc/:id"  $ DB.fetchGenreSection conn
    getRouteWithoutParameter "/themes"         $ DB.fetchAllThemes conn
    getRouteWithParameter    "/themes/:id"     $ DB.fetchTheme conn
    getRouteWithoutParameter "/themesToc"      $ DB.fetchThemesToc conn
    getRouteWithParameter    "/themesToc/:id"  $ DB.fetchThemeSection conn
    getRouteWithoutParameter "/parties"        $ DB.fetchAllParties conn
    getRouteWithParameter    "/parties/:id"    $ DB.fetchParty conn
    getRouteWithoutParameter "/partiesToc"     $ DB.fetchPartiesToc conn
    getRouteWithParameter    "/partiesToc/:id" $ DB.fetchPartySection conn
    getRouteWithoutParameter "/series"         $ DB.fetchAllSeries conn
    getRouteWithParameter    "/series/:id"     $ DB.fetchSeries conn
    getRouteWithoutParameter "/seriesToc"      $ DB.fetchSeriesToc conn
    getRouteWithParameter    "/seriesToc/:id"  $ DB.fetchSeriesSection conn
    getRouteWithoutParameter "/leaders"        $ DB.fetchAllLeaders conn
    getRouteWithParameter    "/leaders/:id"    $ DB.fetchLeader conn
    getRouteWithoutParameter "/leadersToc"     $ DB.fetchLeadersToc conn
    getRouteWithParameter    "/leadersToc/:id" $ DB.fetchLeaderSection conn
    getRouteWithoutParameter "/latitudes"      $ DB.fetchAllLatitudes conn
    getRouteWithParameter    "/latitudes/:id"  $ DB.fetchLatitude conn
    getRouteWithoutParameter "/longitudes"     $ DB.fetchAllLongitudes conn
    getRouteWithParameter    "/longitudes/:id" $ DB.fetchLongitude conn
    getRouteWithoutParameter "/fromYears"      $ DB.fetchAllFromYears conn
    getRouteWithParameter    "/fromYears/:id"  $ DB.fetchFromYear conn
    getRouteWithoutParameter "/upToYears"      $ DB.fetchAllUpToYears conn
    getRouteWithParameter    "/upToYears/:id"  $ DB.fetchUpToYear conn
    getRouteWithoutParameter "/fromRanges"     $ DB.fetchAllFromRanges conn
    getRouteWithParameter    "/fromRanges/:id" $ DB.fetchFromRange conn
    getRouteWithoutParameter "/upToRanges"     $ DB.fetchAllUpToRanges conn
    getRouteWithParameter    "/upToRanges/:id" $ DB.fetchUpToRange conn
