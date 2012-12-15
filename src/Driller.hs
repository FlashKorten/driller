{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Driller.DB as DB
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Database.PostgreSQL.Simple ( connect )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( ToJSON )
-- import System.Remote.Monitoring ( forkServer )
import Web.Scotty
    ( ScottyM,
      RoutePattern,
      Parsable,
      get,
      text,
      params,
      param,
      json,
      scotty,
      middleware )

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

main :: IO ()
main = do
  -- _ <- forkServer "localhost" 8000
  conn <- connect DB.connectionInfo
  let joinMap = DB.initJoinMap
  scotty 3000 $ do
    middleware logStdoutDev

    get "/"                                $ text "No service at this level."
    getRouteWithoutParameter "/authors"    $ DB.fetchAllAuthors conn
    getRouteWithoutParameter "/genres"     $ DB.fetchAllGenres conn
    getRouteWithoutParameter "/engines"    $ DB.fetchAllEngines conn
    getRouteWithoutParameter "/themes"     $ DB.fetchAllThemes conn
    getRouteWithoutParameter "/sides"      $ DB.fetchAllSides conn
    getRouteWithoutParameter "/parties"    $ DB.fetchAllParties conn
    getRouteWithoutParameter "/publishers" $ DB.fetchAllPublishers conn
    getRouteWithoutParameter "/areas"      $ DB.fetchAllAreas conn
    getRouteWithoutParameter "/mechanics"  $ DB.fetchAllMechanics conn
    getRouteWithoutParameter "/games"      $ DB.fetchAllGames conn
    getRouteWithParameter "/author/:id"    $ DB.fetchAuthor conn
    getRouteWithParameter "/genre/:id"     $ DB.fetchGenre conn
    getRouteWithParameter "/engine/:id"    $ DB.fetchEngine conn
    getRouteWithParameter "/theme/:id"     $ DB.fetchTheme conn
    getRouteWithParameter "/side/:id"      $ DB.fetchSide conn
    getRouteWithParameter "/party/:id"     $ DB.fetchParty conn
    getRouteWithParameter "/publisher/:id" $ DB.fetchPublisher conn
    getRouteWithParameter "/area/:id"      $ DB.fetchArea conn
    getRouteWithParameter "/mechanic/:id"  $ DB.fetchMechanic conn
    getRouteWithParameter "/game/:id"      $ DB.fetchGame conn

    get "/g" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledGameResult joinMap conn p
      json result
