{-# LANGUAGE OverloadedStrings #-}
module Main where

import Driller.Data
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
  let config = initConfig conn DB.initQueryMap DB.initJoinMap DB.initGroupMap
  print $ getGroupMap config
  print $ getJoinMap config
  scotty 3003 $ do
    middleware logStdoutDev

    get "/q" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledGameResult config p
      json result
    get "/authorGroup" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledAuthorGroup config p
      json result
    get "/"                                  $ text "No service at this level."
    getRouteWithoutParameter "/authors"         (DB.fetchAllEntries AUTHOR config         :: IO [Author])
    getRouteWithoutParameter "/engines"         (DB.fetchAllEntries ENGINE config         :: IO [Engine])
    getRouteWithoutParameter "/fromRanges"      (DB.fetchAllEntries FROM_RANGE config     :: IO [FromRange])
    getRouteWithoutParameter "/fromTimescales"  (DB.fetchAllEntries FROM_TIMESCALE config :: IO [FromTimescale])
    getRouteWithoutParameter "/fromYears"       (DB.fetchAllEntries FROM_YEAR config      :: IO [FromYear])
    getRouteWithoutParameter "/games"           (DB.fetchAllEntries GAME config           :: IO [Game])
    getRouteWithoutParameter "/genres"          (DB.fetchAllEntries GENRE config          :: IO [Genre])
    getRouteWithoutParameter "/latitudes"       (DB.fetchAllEntries LATITUDE config       :: IO [Latitude])
    getRouteWithoutParameter "/leaders"         (DB.fetchAllEntries LEADER config         :: IO [Leader])
    getRouteWithoutParameter "/longitudes"      (DB.fetchAllEntries LONGITUDE config      :: IO [Longitude])
    getRouteWithoutParameter "/mechanics"       (DB.fetchAllEntries MECHANIC config       :: IO [Mechanic])
    getRouteWithoutParameter "/parties"         (DB.fetchAllEntries PARTY config          :: IO [Party])
    getRouteWithoutParameter "/publishers"      (DB.fetchAllEntries PUBLISHER config      :: IO [Publisher])
    getRouteWithoutParameter "/seriess"         (DB.fetchAllEntries SERIES config         :: IO [Series])
    getRouteWithoutParameter "/sides"           (DB.fetchAllEntries SIDE config           :: IO [Side])
    getRouteWithoutParameter "/themes"          (DB.fetchAllEntries THEME config          :: IO [Theme])
    getRouteWithoutParameter "/upToRanges"      (DB.fetchAllEntries UPTO_RANGE config     :: IO [UpToRange])
    getRouteWithoutParameter "/upToTimescales"  (DB.fetchAllEntries UPTO_TIMESCALE config :: IO [UpToTimescale])
    getRouteWithoutParameter "/upToYears"       (DB.fetchAllEntries UPTO_YEAR config      :: IO [UpToYear])
    getRouteWithoutParameter "/authorGroups"    (DB.fetchGroups AUTHOR config             :: IO [GroupLetter])
    getRouteWithoutParameter "/engineGroups"    (DB.fetchGroups ENGINE config             :: IO [GroupLetter])
    getRouteWithoutParameter "/fromYearGroups"  (DB.fetchGroups FROM_YEAR config          :: IO [GroupNumber])
    getRouteWithoutParameter "/gameGroups"      (DB.fetchGroups GAME config               :: IO [GroupLetter])
    getRouteWithoutParameter "/genreGroups"     (DB.fetchGroups GENRE config              :: IO [GroupLetter])
    getRouteWithoutParameter "/latitudeGroups"  (DB.fetchGroups LATITUDE config           :: IO [GroupNumber])
    getRouteWithoutParameter "/leaderGroups"    (DB.fetchGroups LEADER config             :: IO [GroupLetter])
    getRouteWithoutParameter "/longitudeGroups" (DB.fetchGroups LONGITUDE config          :: IO [GroupNumber])
    getRouteWithoutParameter "/mechanicGroups"  (DB.fetchGroups MECHANIC config           :: IO [GroupLetter])
    getRouteWithoutParameter "/partyGroups"     (DB.fetchGroups PARTY config              :: IO [GroupLetter])
    getRouteWithoutParameter "/publisherGroups" (DB.fetchGroups PUBLISHER config          :: IO [GroupLetter])
    getRouteWithoutParameter "/rangeGroups"     (DB.fetchGroups RANGE config              :: IO [GroupNumber])
    getRouteWithoutParameter "/seriesGroups"    (DB.fetchGroups SERIES config             :: IO [GroupLetter])
    getRouteWithoutParameter "/sideGroups"      (DB.fetchGroups SIDE config               :: IO [GroupLetter])
    getRouteWithoutParameter "/themeGroups"     (DB.fetchGroups THEME config              :: IO [GroupLetter])
    getRouteWithoutParameter "/timescaleGroups" (DB.fetchGroups TIMESCALE config          :: IO [GroupNumber])
    getRouteWithoutParameter "/upToYearGroups"  (DB.fetchGroups UPTO_YEAR config          :: IO [GroupNumber])
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

    -- getRouteWithParameter    "/authorGroup/:id"    $ DB.fetchAuthorGroup config
    getRouteWithParameter    "/engineGroup/:id"    $ DB.fetchEngineGroup config
    getRouteWithParameter    "/fromYearGroup/:id"  $ DB.fetchFromYearGroup config
    getRouteWithParameter    "/gameGroup/:id"      $ DB.fetchGameGroup config
    getRouteWithParameter    "/genreGroup/:id"     $ DB.fetchGenreGroup config
    getRouteWithParameter    "/latitudeGroup/:id"  $ DB.fetchLatitudeGroup config
    getRouteWithParameter    "/leaderGroup/:id"    $ DB.fetchLeaderGroup config
    getRouteWithParameter    "/longitudeGroup/:id" $ DB.fetchLongitudeGroup config
    getRouteWithParameter    "/mechanicGroup/:id"  $ DB.fetchMechanicGroup config
    getRouteWithParameter    "/partyGroup/:id"     $ DB.fetchPartyGroup config
    getRouteWithParameter    "/publisherGroup/:id" $ DB.fetchPublisherGroup config
    getRouteWithParameter    "/rangeGroup/:id"     $ DB.fetchRangeGroup config
    getRouteWithParameter    "/seriesGroup/:id"    $ DB.fetchSeriesGroup config
    getRouteWithParameter    "/sideGroup/:id"      $ DB.fetchSideGroup config
    getRouteWithParameter    "/themeGroup/:id"     $ DB.fetchThemeGroup config
    getRouteWithParameter    "/timescaleGroup/:id" $ DB.fetchTimescaleGroup config
    getRouteWithParameter    "/upToYearGroup/:id"  $ DB.fetchUpToYearGroup config
