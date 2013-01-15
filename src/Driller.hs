{-# LANGUAGE OverloadedStrings #-}
module Main where

import Driller.Data
import qualified Driller.DB as DB
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Driller.JsonP ( jsonp )
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
    , Param()
    , json
    , scotty
    , middleware
    )

main :: IO ()
main = do
  -- _ <- forkServer "localhost" 8000
  conn <- connect DB.connectionInfo
  let config = initConfig conn DB.initQueryMap DB.initJoinMap DB.initGroupMap
  scotty 3003 $ do
    middleware logStdoutDev
    middleware jsonp

    drilledResultRoutes config
    groupsRoutes config
    entriesRoutes config
    entryRoutes config
    get "/" $ text "No service at this level."

drilledResultRoutes :: Config -> ScottyM ()
drilledResultRoutes config = do
    getDrilledResult "/q"                      DB.fetchDrilledGameResult config
    getDrilledResult "/authorGroup/:id"        DB.fetchDrilledAuthorGroup config
    getDrilledResult "/publisherGroup/:id"     DB.fetchDrilledPublisherGroup config
    getDrilledResult "/gameGroup/:id"          DB.fetchDrilledGameGroup config
    getDrilledResult "/seriesGroup/:id"        DB.fetchDrilledSeriesGroup config
    getDrilledResult "/sideGroup/:id"          DB.fetchDrilledSideGroup config
    getDrilledResult "/themeGroup/:id"         DB.fetchDrilledThemeGroup config
    getDrilledResult "/leaderGroup/:id"        DB.fetchDrilledLeaderGroup config
    getDrilledResult "/mechanicGroup/:id"      DB.fetchDrilledMechanicGroup config
    getDrilledResult "/partyGroup/:id"         DB.fetchDrilledPartyGroup config
    getDrilledResult "/engineGroup/:id"        DB.fetchDrilledEngineGroup config
    getDrilledResult "/genreGroup/:id"         DB.fetchDrilledGenreGroup config
    getDrilledResult "/fromYearGroup/:id"      DB.fetchDrilledFromYearGroup config
    getDrilledResult "/latitudeGroup/:id"      DB.fetchDrilledLatitudeGroup config
    getDrilledResult "/longitudeGroup/:id"     DB.fetchDrilledLongitudeGroup config
    getDrilledResult "/fromRangeGroup/:id"     DB.fetchDrilledFromRangeGroup config
    getDrilledResult "/upToRangeGroup/:id"     DB.fetchDrilledUpToRangeGroup config
    getDrilledResult "/fromTimescaleGroup/:id" DB.fetchDrilledFromTimescaleGroup config
    getDrilledResult "/upToTimescaleGroup/:id" DB.fetchDrilledUpToTimescaleGroup config
    getDrilledResult "/upToYearGroup/:id"      DB.fetchDrilledUpToYearGroup config

entriesRoutes :: Config -> ScottyM ()
entriesRoutes config = do
    getRouteWithoutParameter "/authors"         $ DB.fetchAllAuthorEntries config
    getRouteWithoutParameter "/engines"         $ DB.fetchAllEngineEntries config
    getRouteWithoutParameter "/fromRanges"      $ DB.fetchAllFromRangeEntries config
    getRouteWithoutParameter "/fromTimescales"  $ DB.fetchAllFromTimescaleEntries config
    getRouteWithoutParameter "/fromYears"       $ DB.fetchAllFromYearEntries config
    getRouteWithoutParameter "/games"           $ DB.fetchAllGameEntries config
    getRouteWithoutParameter "/genres"          $ DB.fetchAllGenreEntries config
    getRouteWithoutParameter "/latitudes"       $ DB.fetchAllLatitudeEntries config
    getRouteWithoutParameter "/leaders"         $ DB.fetchAllLeaderEntries config
    getRouteWithoutParameter "/longitudes"      $ DB.fetchAllLongitudeEntries config
    getRouteWithoutParameter "/mechanics"       $ DB.fetchAllMechanicEntries config
    getRouteWithoutParameter "/parties"         $ DB.fetchAllPartyEntries config
    getRouteWithoutParameter "/publishers"      $ DB.fetchAllPublisherEntries config
    getRouteWithoutParameter "/seriess"         $ DB.fetchAllSeriesEntries config
    getRouteWithoutParameter "/sides"           $ DB.fetchAllSideEntries config
    getRouteWithoutParameter "/themes"          $ DB.fetchAllThemeEntries config
    getRouteWithoutParameter "/upToRanges"      $ DB.fetchAllUpToRangeEntries config
    getRouteWithoutParameter "/upToTimescales"  $ DB.fetchAllUpToTimescaleEntries config
    getRouteWithoutParameter "/upToYears"       $ DB.fetchAllUpToYearEntries config

groupsRoutes :: Config -> ScottyM ()
groupsRoutes config = do
    getRouteWithoutParameter "/authorGroups"    $ DB.fetchAuthorGroups config
    getRouteWithoutParameter "/engineGroups"    $ DB.fetchEngineGroups config
    getRouteWithoutParameter "/fromYearGroups"  $ DB.fetchFromYearGroups config
    getRouteWithoutParameter "/gameGroups"      $ DB.fetchGameGroups config
    getRouteWithoutParameter "/genreGroups"     $ DB.fetchGenreGroups config
    getRouteWithoutParameter "/latitudeGroups"  $ DB.fetchLatitudeGroups config
    getRouteWithoutParameter "/leaderGroups"    $ DB.fetchLeaderGroups config
    getRouteWithoutParameter "/longitudeGroups" $ DB.fetchLongitudeGroups config
    getRouteWithoutParameter "/mechanicGroups"  $ DB.fetchMechanicGroups config
    getRouteWithoutParameter "/partyGroups"     $ DB.fetchPartyGroups config
    getRouteWithoutParameter "/publisherGroups" $ DB.fetchPublisherGroups config
    getRouteWithoutParameter "/rangeGroups"     $ DB.fetchRangeGroups config
    getRouteWithoutParameter "/seriesGroups"    $ DB.fetchSeriesGroups config
    getRouteWithoutParameter "/sideGroups"      $ DB.fetchSideGroups config
    getRouteWithoutParameter "/themeGroups"     $ DB.fetchThemeGroups config
    getRouteWithoutParameter "/timescaleGroups" $ DB.fetchTimescaleGroups config
    getRouteWithoutParameter "/upToYearGroups"  $ DB.fetchUpToYearGroups config

entryRoutes :: Config -> ScottyM ()
entryRoutes config = do
    getRouteWithParameter    "/author/:id"         $ DB.fetchAuthorEntry config
    getRouteWithParameter    "/engine/:id"         $ DB.fetchEngineEntry config
    getRouteWithParameter    "/fromRange/:id"      $ DB.fetchFromRangeEntry config
    getRouteWithParameter    "/fromTimescale/:id"  $ DB.fetchFromTimescaleEntry config
    getRouteWithParameter    "/fromYear/:id"       $ DB.fetchFromYearEntry config
    getRouteWithParameter    "/game/:id"           $ DB.fetchGameEntry config
    getRouteWithParameter    "/genre/:id"          $ DB.fetchGenreEntry config
    getRouteWithParameter    "/latitude/:id"       $ DB.fetchLatitudeEntry config
    getRouteWithParameter    "/leader/:id"         $ DB.fetchLeaderEntry config
    getRouteWithParameter    "/longitude/:id"      $ DB.fetchLongitudeEntry config
    getRouteWithParameter    "/mechanic/:id"       $ DB.fetchMechanicEntry config
    getRouteWithParameter    "/party/:id"          $ DB.fetchPartyEntry config
    getRouteWithParameter    "/publisher/:id"      $ DB.fetchPublisherEntry config
    getRouteWithParameter    "/series/:id"         $ DB.fetchSeriesEntry config
    getRouteWithParameter    "/side/:id"           $ DB.fetchSideEntry config
    getRouteWithParameter    "/theme/:id"          $ DB.fetchThemeEntry config
    getRouteWithParameter    "/upToRange/:id"      $ DB.fetchUpToRangeEntry config
    getRouteWithParameter    "/upToTimescale/:id"  $ DB.fetchUpToTimescaleEntry config
    getRouteWithParameter    "/upToYear/:id"       $ DB.fetchUpToYearEntry config

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

getDrilledResult :: ToJSON a => RoutePattern -> (Config -> [Param] -> IO a) -> Config -> ScottyM ()
getDrilledResult url getter config = get url $ params >>= liftIO . getter config >>= json

