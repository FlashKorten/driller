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
  let joinMap  = DB.initJoinMap
      queryMap = DB.initQueryMap
  scotty 3003 $ do
    middleware logStdoutDev

    get "/q" $ do
      p <- params
      result <- liftIO $ DB.fetchDrilledGameResult conn joinMap queryMap p
      json result
    get "/"                                  $ text "No service at this level."
    getRouteWithoutParameter "/authorGroups"    (DB.fetchGroups AUTHOR conn queryMap             :: IO [GroupLetter])
    getRouteWithoutParameter "/authors"         (DB.fetchAllEntries AUTHOR conn queryMap         :: IO [Author])
    getRouteWithoutParameter "/engineGroups"    (DB.fetchGroups ENGINE conn queryMap             :: IO [GroupLetter])
    getRouteWithoutParameter "/engines"         (DB.fetchAllEntries ENGINE conn queryMap         :: IO [Engine])
    getRouteWithoutParameter "/fromRanges"      (DB.fetchAllEntries FROM_RANGE conn queryMap     :: IO [FromRange])
    getRouteWithoutParameter "/fromTimescales"  (DB.fetchAllEntries FROM_TIMESCALE conn queryMap :: IO [FromTimescale])
    getRouteWithoutParameter "/fromYearGroups"  (DB.fetchGroups FROM_YEAR conn queryMap          :: IO [GroupNumber])
    getRouteWithoutParameter "/fromYears"       (DB.fetchAllEntries FROM_YEAR conn queryMap      :: IO [FromYear])
    getRouteWithoutParameter "/gameGroups"      (DB.fetchGroups GAME conn queryMap               :: IO [GroupLetter])
    getRouteWithoutParameter "/games"           (DB.fetchAllEntries GAME conn queryMap           :: IO [Game])
    getRouteWithoutParameter "/genreGroups"     (DB.fetchGroups GENRE conn queryMap              :: IO [GroupLetter])
    getRouteWithoutParameter "/genres"          (DB.fetchAllEntries GENRE conn queryMap          :: IO [Genre])
    getRouteWithoutParameter "/latitudeGroups"  (DB.fetchGroups LATITUDE conn queryMap           :: IO [GroupNumber])
    getRouteWithoutParameter "/latitudes"       (DB.fetchAllEntries LATITUDE conn queryMap       :: IO [Latitude])
    getRouteWithoutParameter "/leaderGroups"    (DB.fetchGroups LEADER conn queryMap             :: IO [GroupLetter])
    getRouteWithoutParameter "/leaders"         (DB.fetchAllEntries LEADER conn queryMap         :: IO [Leader])
    getRouteWithoutParameter "/longitudeGroups" (DB.fetchGroups LONGITUDE conn queryMap          :: IO [GroupNumber])
    getRouteWithoutParameter "/longitudes"      (DB.fetchAllEntries LONGITUDE conn queryMap      :: IO [Longitude])
    getRouteWithoutParameter "/mechanicGroups"  (DB.fetchGroups MECHANIC conn queryMap           :: IO [GroupLetter])
    getRouteWithoutParameter "/mechanics"       (DB.fetchAllEntries MECHANIC conn queryMap       :: IO [Mechanic])
    getRouteWithoutParameter "/parties"         (DB.fetchAllEntries PARTY conn queryMap          :: IO [Party])
    getRouteWithoutParameter "/partyGroups"     (DB.fetchGroups PARTY conn queryMap              :: IO [GroupLetter])
    getRouteWithoutParameter "/publisherGroups" (DB.fetchGroups PUBLISHER conn queryMap          :: IO [GroupLetter])
    getRouteWithoutParameter "/publishers"      (DB.fetchAllEntries PUBLISHER conn queryMap      :: IO [Publisher])
    getRouteWithoutParameter "/rangeGroups"     (DB.fetchGroups RANGE conn queryMap              :: IO [GroupNumber])
    getRouteWithoutParameter "/seriesGroups"    (DB.fetchGroups SERIES conn queryMap             :: IO [GroupLetter])
    getRouteWithoutParameter "/seriess"         (DB.fetchAllEntries SERIES conn queryMap         :: IO [Series])
    getRouteWithoutParameter "/sideGroups"      (DB.fetchGroups SIDE conn queryMap               :: IO [GroupLetter])
    getRouteWithoutParameter "/sides"           (DB.fetchAllEntries SIDE conn queryMap           :: IO [Side])
    getRouteWithoutParameter "/themeGroups"     (DB.fetchGroups THEME conn queryMap              :: IO [GroupLetter])
    getRouteWithoutParameter "/themes"          (DB.fetchAllEntries THEME conn queryMap          :: IO [Theme])
    getRouteWithoutParameter "/timescaleGroups" (DB.fetchGroups TIMESCALE conn queryMap          :: IO [GroupNumber])
    getRouteWithoutParameter "/upToRanges"      (DB.fetchAllEntries UPTO_RANGE conn queryMap     :: IO [UpToRange])
    getRouteWithoutParameter "/upToTimescales"  (DB.fetchAllEntries UPTO_TIMESCALE conn queryMap :: IO [UpToTimescale])
    getRouteWithoutParameter "/upToYearGroups"  (DB.fetchGroups UPTO_YEAR conn queryMap          :: IO [GroupNumber])
    getRouteWithoutParameter "/upToYears"       (DB.fetchAllEntries UPTO_YEAR conn queryMap      :: IO [UpToYear])
    getRouteWithParameter    "/author/:id"         $ DB.fetchAuthorEntry conn queryMap
    getRouteWithParameter    "/authorGroup/:id"    $ DB.fetchAuthorGroup conn queryMap
    getRouteWithParameter    "/engine/:id"         $ DB.fetchEngineEntry conn queryMap
    getRouteWithParameter    "/engineGroup/:id"    $ DB.fetchEngineGroup conn queryMap
    getRouteWithParameter    "/fromRange/:id"      $ DB.fetchFromRangeEntry conn queryMap
    getRouteWithParameter    "/fromTimescale/:id"  $ DB.fetchFromTimescaleEntry conn queryMap
    getRouteWithParameter    "/fromYear/:id"       $ DB.fetchFromYearEntry conn queryMap
    getRouteWithParameter    "/fromYearGroup/:id"  $ DB.fetchFromYearGroup conn queryMap
    getRouteWithParameter    "/game/:id"           $ DB.fetchGameEntry conn queryMap
    getRouteWithParameter    "/gameGroup/:id"      $ DB.fetchGameGroup conn queryMap
    getRouteWithParameter    "/genre/:id"          $ DB.fetchGenreEntry conn queryMap
    getRouteWithParameter    "/genreGroup/:id"     $ DB.fetchGenreGroup conn queryMap
    getRouteWithParameter    "/latitude/:id"       $ DB.fetchLatitudeEntry conn queryMap
    getRouteWithParameter    "/latitudeGroup/:id"  $ DB.fetchLatitudeGroup conn queryMap
    getRouteWithParameter    "/leader/:id"         $ DB.fetchLeaderEntry conn queryMap
    getRouteWithParameter    "/leaderGroup/:id"    $ DB.fetchLeaderGroup conn queryMap
    getRouteWithParameter    "/longitude/:id"      $ DB.fetchLongitudeEntry conn queryMap
    getRouteWithParameter    "/longitudeGroup/:id" $ DB.fetchLongitudeGroup conn queryMap
    getRouteWithParameter    "/mechanic/:id"       $ DB.fetchMechanicEntry conn queryMap
    getRouteWithParameter    "/mechanicGroup/:id"  $ DB.fetchMechanicGroup conn queryMap
    getRouteWithParameter    "/party/:id"          $ DB.fetchPartyEntry conn queryMap
    getRouteWithParameter    "/partyGroup/:id"     $ DB.fetchPartyGroup conn queryMap
    getRouteWithParameter    "/publisher/:id"      $ DB.fetchPublisherEntry conn queryMap
    getRouteWithParameter    "/publisherGroup/:id" $ DB.fetchPublisherGroup conn queryMap
    getRouteWithParameter    "/rangeGroup/:id"     $ DB.fetchRangeGroup conn queryMap
    getRouteWithParameter    "/series/:id"         $ DB.fetchSeriesEntry conn queryMap
    getRouteWithParameter    "/seriesGroup/:id"    $ DB.fetchSeriesGroup conn queryMap
    getRouteWithParameter    "/side/:id"           $ DB.fetchSideEntry conn queryMap
    getRouteWithParameter    "/sideGroup/:id"      $ DB.fetchSideGroup conn queryMap
    getRouteWithParameter    "/theme/:id"          $ DB.fetchThemeEntry conn queryMap
    getRouteWithParameter    "/themeGroup/:id"     $ DB.fetchThemeGroup conn queryMap
    getRouteWithParameter    "/timescaleGroup/:id" $ DB.fetchTimescaleGroup conn queryMap
    getRouteWithParameter    "/upToRange/:id"      $ DB.fetchUpToRangeEntry conn queryMap
    getRouteWithParameter    "/upToTimescale/:id"  $ DB.fetchUpToTimescaleEntry conn queryMap
    getRouteWithParameter    "/upToYear/:id"       $ DB.fetchUpToYearEntry conn queryMap
    getRouteWithParameter    "/upToYearGroup/:id"  $ DB.fetchUpToYearGroup conn queryMap
