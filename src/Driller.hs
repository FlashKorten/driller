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
    , Param()
    , json
    , scotty
    , middleware
    )

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

getDrilledResult :: ToJSON a => RoutePattern -> (Config -> [Param] -> IO a) -> Config -> ScottyM ()
getDrilledResult url getter config = get url $ params >>= liftIO . getter config >>= json

main :: IO ()
main = do
  -- _ <- forkServer "localhost" 8000
  conn <- connect DB.connectionInfo
  let config = initConfig conn DB.initQueryMap DB.initJoinMap DB.initGroupMap
  scotty 3003 $ do
    middleware logStdoutDev

    getDrilledResult "/q"                   DB.fetchDrilledGameResult config
    getDrilledResult "/authorGroup/:id"     DB.fetchDrilledAuthorGroup config
    getDrilledResult "/publisherGroup/:id"  DB.fetchDrilledPublisherGroup config
    getDrilledResult "/gameGroup/:id"       DB.fetchDrilledGameGroup config
    getDrilledResult "/seriesGroup/:id"     DB.fetchDrilledSeriesGroup config
    getDrilledResult "/sideGroup/:id"       DB.fetchDrilledSideGroup config
    getDrilledResult "/themeGroup/:id"      DB.fetchDrilledThemeGroup config
    getDrilledResult "/leaderGroup/:id"     DB.fetchDrilledLeaderGroup config
    getDrilledResult "/mechanicGroup/:id"   DB.fetchDrilledMechanicGroup config
    getDrilledResult "/partyGroup/:id"      DB.fetchDrilledPartyGroup config
    getDrilledResult "/engineGroup/:id"     DB.fetchDrilledEngineGroup config
    getDrilledResult "/genreGroup/:id"      DB.fetchDrilledGenreGroup config
    getDrilledResult "/fromYearGroup/:id"   DB.fetchDrilledFromYearGroup config
    getDrilledResult "/latitudeGroup/:id"   DB.fetchDrilledLatitudeGroup config
    getDrilledResult "/longitudeGroup/:id"  DB.fetchDrilledLongitudeGroup config
    -- getDrilledResult "/rangeGroup/:id"      DB.fetchDrilledRangeGroup config
    -- getDrilledResult "/timescaleGroup/:id"  DB.fetchDrilledTimescaleGroup config
    getDrilledResult "/upToYearGroup/:id"   DB.fetchDrilledUpToYearGroup config
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
    get                      "/"                   $ text "No service at this level."
