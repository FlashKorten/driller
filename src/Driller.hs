{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import qualified Data.Text as Text
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH
import Data.Aeson (ToJSON)
import qualified Driller.Queries as Q
-- import System.Remote.Monitoring

data Genre     = Genre     { getGenreId     :: Int, getGenreName     :: Text.Text }
data Engine    = Engine    { getEngineId    :: Int, getEngineName    :: Text.Text }
data Theme     = Theme     { getThemeId     :: Int, getThemeName     :: Text.Text }
data Mechanic  = Mechanic  { getMechanicId  :: Int, getMechanicName  :: Text.Text }
data Side      = Side      { getSideId      :: Int, getSideName      :: Text.Text }
data Party     = Party     { getPartyId     :: Int, getPartyName     :: Text.Text }
data Publisher = Publisher { getPublisherId :: Int, getPublisherName :: Text.Text }
data Area      = Area      { getAreaId      :: Int, getAreaName      :: Text.Text }
data Author    = Author    { getAuthorId    :: Int, getAuthorName    :: Text.Text }
  deriving Show

data GameResult = GameResult { getGames :: [Game]
                             , getGenres :: [Genre]
                             , getThemes :: [Theme]
                             , getMechanics :: [Mechanic]
                             , getSides :: [Side]
                             , getParties :: [Party]
                             , getPublishers :: [Publisher]
                             , getAreas :: [Area]
                             , getAuthors :: [Author]
                             , getEngines :: [Engine]
}

data Game = Game { getGameId        :: Int
                 , getGameTitle     :: Text.Text
                 , getGameSubtitle  :: Text.Text
                 , getNumPlayersMin :: Int
                 , getNumPlayersMax :: Int
                 , getGameTimeStart :: Int
                 , getGameTimeEnd   :: Int
                 , getBggId         :: Text.Text
                 }


$(deriveJSON (drop 9) ''Author)
$(deriveJSON (drop 8) ''Genre)
$(deriveJSON (drop 9) ''Engine)
$(deriveJSON (drop 8) ''Theme)
$(deriveJSON (drop 11) ''Mechanic)
$(deriveJSON (drop 7) ''Side)
$(deriveJSON (drop 8) ''Party)
$(deriveJSON (drop 12) ''Publisher)
$(deriveJSON (drop 7) ''Area)
$(deriveJSON (drop 3) ''Game)
$(deriveJSON (drop 3) ''GameResult)

instance FromRow Author    where fromRow = Author    <$> field <*> field
instance FromRow Genre     where fromRow = Genre     <$> field <*> field
instance FromRow Engine    where fromRow = Engine    <$> field <*> field
instance FromRow Theme     where fromRow = Theme     <$> field <*> field
instance FromRow Mechanic  where fromRow = Mechanic  <$> field <*> field
instance FromRow Side      where fromRow = Side      <$> field <*> field
instance FromRow Party     where fromRow = Party     <$> field <*> field
instance FromRow Publisher where fromRow = Publisher <$> field <*> field
instance FromRow Area      where fromRow = Area      <$> field <*> field
instance FromRow Game      where fromRow = Game      <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance FromRow Int       where fromRow = field
instance ToRow Int         where toRow n = [toField n]

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser = "nemesis"
                                    , connectPassword = "nemesis"
                                    , connectDatabase = "nn"
                                    }

fetchAuthor :: Connection -> Int -> IO [Author]
fetchAuthor c = query c Q.authorQuery

fetchAuthors :: Connection -> [Int] -> IO [Author]
fetchAuthors c ids = query c Q.authorsQuery (Only (In ids))

fetchAllAuthors :: Connection -> IO [Author]
fetchAllAuthors c = query_ c Q.allAuthorsQuery

fetchGenre :: Connection -> Int -> IO [Genre]
fetchGenre c = query c Q.genreQuery

fetchGenres :: Connection -> [Int] -> IO [Genre]
fetchGenres c ids = query c Q.genresQuery (Only (In ids))

fetchAllGenres :: Connection -> IO [Genre]
fetchAllGenres c = query_ c Q.allGenresQuery

fetchEngine :: Connection -> Int -> IO [Engine]
fetchEngine c = query c Q.engineQuery

fetchEngines :: Connection -> [Int] -> IO [Engine]
fetchEngines c ids = query c Q.enginesQuery (Only (In ids))

fetchAllEngines :: Connection -> IO [Engine]
fetchAllEngines c = query_ c Q.allEnginesQuery

fetchTheme :: Connection -> Int -> IO [Theme]
fetchTheme c = query c Q.themeQuery

fetchThemes :: Connection -> [Int] -> IO [Theme]
fetchThemes c ids = query c Q.themesQuery (Only (In ids))

fetchAllThemes :: Connection -> IO [Theme]
fetchAllThemes c = query_ c Q.allThemesQuery

fetchMechanic :: Connection -> Int -> IO [Mechanic]
fetchMechanic c = query c Q.mechanicQuery

fetchMechanics :: Connection -> [Int] -> IO [Mechanic]
fetchMechanics c ids = query c Q.mechanicsQuery (Only (In ids))

fetchAllMechanics :: Connection -> IO [Mechanic]
fetchAllMechanics c = query_ c Q.allMechanicsQuery

fetchSide :: Connection -> Int -> IO [Side]
fetchSide c = query c Q.sideQuery

fetchSides :: Connection -> [Int] -> IO [Side]
fetchSides c ids = query c Q.sidesQuery (Only (In ids))

fetchAllSides :: Connection -> IO [Side]
fetchAllSides c = query_ c Q.allSidesQuery

fetchParty :: Connection -> Int -> IO [Party]
fetchParty c = query c Q.partyQuery

fetchParties :: Connection -> [Int] -> IO [Party]
fetchParties c ids = query c Q.partiesQuery (Only (In ids))

fetchAllParties :: Connection -> IO [Party]
fetchAllParties c = query_ c Q.allPartiesQuery

fetchPublisher :: Connection -> Int -> IO [Publisher]
fetchPublisher c = query c Q.publisherQuery

fetchPublishers :: Connection -> [Int] -> IO [Publisher]
fetchPublishers c ids = query c Q.publishersQuery (Only (In ids))

fetchAllPublishers :: Connection -> IO [Publisher]
fetchAllPublishers c = query_ c Q.allPublishersQuery

fetchArea :: Connection -> Int -> IO [Area]
fetchArea c = query c Q.areaQuery

fetchAreas :: Connection -> [Int] -> IO [Area]
fetchAreas c ids = query c Q.areasQuery (Only (In ids))

fetchAllAreas :: Connection -> IO [Area]
fetchAllAreas c = query_ c Q.allAreasQuery

fetchGame :: Connection -> Int -> IO [Game]
fetchGame c = query c Q.gameQuery

fetchGames :: Connection -> [Int] -> IO [Game]
fetchGames c ids = query c Q.gamesQuery (Only (In ids))

fetchAllGames :: Connection -> IO [Game]
fetchAllGames c = query_ c Q.allGamesQuery

fetchDrilledGameResult :: Q.JoinMap -> Connection -> [Param] -> IO GameResult
fetchDrilledGameResult jm c p = do
    let (keys, values) = unzip p
        que = Q.gameListQuery jm keys
    ids        <- query c que values
    games      <- fetchGames c ids
    genres     <- fetchGenres c ids
    themes     <- fetchThemes c ids
    mechanics  <- fetchMechanics c ids
    sides      <- fetchSides c ids
    parties    <- fetchParties c ids
    publishers <- fetchPublishers c ids
    areas      <- fetchAreas c ids
    authors    <- fetchAuthors c ids
    engines    <- fetchEngines c ids
    return GameResult { getGames      = games
                      , getGenres     = genres
                      , getThemes     = themes
                      , getMechanics  = mechanics
                      , getSides      = sides
                      , getParties    = parties
                      , getPublishers = publishers
                      , getAreas      = areas
                      , getAuthors    = authors
                      , getEngines    = engines
                      }

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

main :: IO ()
main = do
  -- _ <- forkServer "localhost" 8000
  conn <- connect connectionInfo
  scotty 3000 $ do
    middleware logStdoutDev

    let joinMap = Q.initJoinMap
    get "/"                                $ text "foobar"
    getRouteWithoutParameter "/authors"    $ fetchAllAuthors conn
    getRouteWithoutParameter "/genres"     $ fetchAllGenres conn
    getRouteWithoutParameter "/engines"    $ fetchAllEngines conn
    getRouteWithoutParameter "/themes"     $ fetchAllThemes conn
    getRouteWithoutParameter "/sides"      $ fetchAllSides conn
    getRouteWithoutParameter "/parties"    $ fetchAllParties conn
    getRouteWithoutParameter "/publishers" $ fetchAllPublishers conn
    getRouteWithoutParameter "/areas"      $ fetchAllAreas conn
    getRouteWithoutParameter "/mechanics"  $ fetchAllMechanics conn
    getRouteWithoutParameter "/games"      $ fetchAllGames conn
    getRouteWithParameter "/author/:id"    $ fetchAuthor conn
    getRouteWithParameter "/genre/:id"     $ fetchGenre conn
    getRouteWithParameter "/engine/:id"    $ fetchEngine conn
    getRouteWithParameter "/theme/:id"     $ fetchTheme conn
    getRouteWithParameter "/side/:id"      $ fetchSide conn
    getRouteWithParameter "/party/:id"     $ fetchParty conn
    getRouteWithParameter "/publisher/:id" $ fetchPublisher conn
    getRouteWithParameter "/area/:id"      $ fetchArea conn
    getRouteWithParameter "/mechanic/:id"  $ fetchMechanic conn
    getRouteWithParameter "/game/:id"      $ fetchGame conn

    get "/g" $ do
      p <- params
      result <- liftIO $ fetchDrilledGameResult joinMap conn p
      json result
