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
    get "/"                                   $ text "No service at this level."
    getRouteWithoutParameter "/authors"       $ DB.fetchAllAuthors conn
    getRouteWithParameter    "/author/:id"    $ DB.fetchAuthor conn
    getRouteWithoutParameter "/engines"       $ DB.fetchAllEngines conn
    getRouteWithParameter    "/engine/:id"    $ DB.fetchEngine conn
    getRouteWithoutParameter "/sides"         $ DB.fetchAllSides conn
    getRouteWithParameter    "/side/:id"      $ DB.fetchSide conn
    getRouteWithoutParameter "/publishers"    $ DB.fetchAllPublishers conn
    getRouteWithParameter    "/publisher/:id" $ DB.fetchPublisher conn
    getRouteWithoutParameter "/mechanics"     $ DB.fetchAllMechanics conn
    getRouteWithParameter    "/mechanic/:id"  $ DB.fetchMechanic conn
    getRouteWithoutParameter "/games"         $ DB.fetchAllGames conn
    getRouteWithParameter    "/game/:id"      $ DB.fetchGame conn
    getRouteWithoutParameter "/genres"        $ DB.fetchAllGenres conn
    getRouteWithParameter    "/genre/:id"     $ DB.fetchGenre conn
    getRouteWithoutParameter "/themes"        $ DB.fetchAllThemes conn
    getRouteWithParameter    "/theme/:id"     $ DB.fetchTheme conn
    getRouteWithoutParameter "/parties"       $ DB.fetchAllParties conn
    getRouteWithParameter    "/party/:id"     $ DB.fetchParty conn
    getRouteWithoutParameter "/series"        $ DB.fetchAllSeries conn
    getRouteWithParameter    "/series/:id"    $ DB.fetchSeries conn
    getRouteWithoutParameter "/leaders"       $ DB.fetchAllLeaders conn
    getRouteWithParameter    "/leader/:id"    $ DB.fetchLeader conn
    getRouteWithoutParameter "/latitudes"     $ DB.fetchAllLatitudes conn
    getRouteWithParameter    "/latitude/:id"  $ DB.fetchLatitude conn
    getRouteWithoutParameter "/longitudes"    $ DB.fetchAllLongitudes conn
    getRouteWithParameter    "/longitude/:id" $ DB.fetchLongitude conn
    getRouteWithoutParameter "/fromYears"     $ DB.fetchAllFromYears conn
    getRouteWithParameter    "/fromYear/:id"  $ DB.fetchFromYear conn
    getRouteWithoutParameter "/upToYears"     $ DB.fetchAllUpToYears conn
    getRouteWithParameter    "/upToYear/:id"  $ DB.fetchUpToYear conn
    getRouteWithoutParameter "/fromRanges"    $ DB.fetchAllFromRanges conn
    getRouteWithParameter    "/fromRange/:id" $ DB.fetchFromRange conn
    getRouteWithoutParameter "/upToRanges"    $ DB.fetchAllUpToRanges conn
    getRouteWithParameter    "/upToRange/:id" $ DB.fetchUpToRange conn
