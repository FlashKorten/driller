{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Driller where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import qualified Data.Text as Text
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Aeson.TH
import Data.Aeson (ToJSON)
import qualified Driller.Queries as Q

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

data Game = Game { getGameId        :: Int
                 , getGameTitle     :: Text.Text
                 , getGameSubtitle  :: Text.Text
                 , getNumPlayersMin :: Int
                 , getNumPlayersMax :: Int
                 , getGameTimeStart :: Int
                 , getGameTimeEnd   :: Int
                 , getBggId         :: Text.Text
                 }


$(deriveJSON (drop 3) ''Author)
$(deriveJSON (drop 3) ''Genre)
$(deriveJSON (drop 3) ''Engine)
$(deriveJSON (drop 3) ''Theme)
$(deriveJSON (drop 3) ''Mechanic)
$(deriveJSON (drop 3) ''Side)
$(deriveJSON (drop 3) ''Party)
$(deriveJSON (drop 3) ''Publisher)
$(deriveJSON (drop 3) ''Area)
$(deriveJSON (drop 3) ''Game)

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
instance ToRow Int         where toRow n = [toField n]

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectUser = "nemesis"
                                    , connectPassword = "nemesis"
                                    , connectDatabase = "nn"
                                    }

getAuthor :: Connection -> Int -> IO [Author]
getAuthor c = query c Q.authorQuery

getAuthors :: Connection -> [Int] -> IO [Author]
getAuthors c ids = query c Q.authorsQuery (Only (In ids))

getAllAuthors :: Connection -> IO [Author]
getAllAuthors c = query_ c Q.allAuthorsQuery

getGenre :: Connection -> Int -> IO [Genre]
getGenre c = query c Q.genreQuery

getGenres :: Connection -> [Int] -> IO [Genre]
getGenres c ids = query c Q.genresQuery (Only (In ids))

getAllGenres :: Connection -> IO [Genre]
getAllGenres c = query_ c Q.allGenresQuery

getEngine :: Connection -> Int -> IO [Engine]
getEngine c = query c Q.engineQuery

getEngines :: Connection -> [Int] -> IO [Engine]
getEngines c ids = query c Q.enginesQuery (Only (In ids))

getAllEngines :: Connection -> IO [Engine]
getAllEngines c = query_ c Q.allEnginesQuery

getTheme :: Connection -> Int -> IO [Theme]
getTheme c = query c Q.themeQuery

getThemes :: Connection -> [Int] -> IO [Theme]
getThemes c ids = query c Q.themesQuery (Only (In ids))

getAllThemes :: Connection -> IO [Theme]
getAllThemes c = query_ c Q.allThemesQuery

getMechanic :: Connection -> Int -> IO [Mechanic]
getMechanic c = query c Q.mechanicQuery

getMechanics :: Connection -> [Int] -> IO [Mechanic]
getMechanics c ids = query c Q.mechanicsQuery (Only (In ids))

getAllMechanics :: Connection -> IO [Mechanic]
getAllMechanics c = query_ c Q.allMechanicsQuery

getSide :: Connection -> Int -> IO [Side]
getSide c = query c Q.sideQuery

getSides :: Connection -> [Int] -> IO [Side]
getSides c ids = query c Q.sidesQuery (Only (In ids))

getAllSides :: Connection -> IO [Side]
getAllSides c = query_ c Q.allSidesQuery

getParty :: Connection -> Int -> IO [Party]
getParty c = query c Q.partyQuery

getParties :: Connection -> [Int] -> IO [Party]
getParties c ids = query c Q.partiesQuery (Only (In ids))

getAllParties :: Connection -> IO [Party]
getAllParties c = query_ c Q.allPartiesQuery

getPublisher :: Connection -> Int -> IO [Publisher]
getPublisher c = query c Q.publisherQuery

getPublishers :: Connection -> [Int] -> IO [Publisher]
getPublishers c ids = query c Q.publishersQuery (Only (In ids))

getAllPublishers :: Connection -> IO [Publisher]
getAllPublishers c = query_ c Q.allPublishersQuery

getArea :: Connection -> Int -> IO [Area]
getArea c = query c Q.areaQuery

getAreas :: Connection -> [Int] -> IO [Area]
getAreas c ids = query c Q.areasQuery (Only (In ids))

getAllAreas :: Connection -> IO [Area]
getAllAreas c = query_ c Q.allAreasQuery

getGame :: Connection -> Int -> IO [Game]
getGame c = query c Q.gameQuery

getGames :: Connection -> [Int] -> IO [Game]
getGames c ids = query c Q.gamesQuery (Only (In ids))

getAllGames :: Connection -> IO [Game]
getAllGames c = query_ c Q.allGamesQuery

getRouteWithoutParameter :: ToJSON a => RoutePattern -> IO a -> ScottyM ()
getRouteWithoutParameter url getter = get url $ liftIO getter >>= json

getRouteWithParameter :: (Parsable a1, ToJSON a) => RoutePattern -> (a1 -> IO a) -> ScottyM ()
getRouteWithParameter url getter = get url $ param "id" >>= liftIO . getter >>= json

main :: IO ()
main = do
  conn <- connect connectionInfo
  authorRow <- getAuthors conn [1,3,6]
  forM_ authorRow print
  scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ text "foobar"
    getRouteWithoutParameter "/authors" $ getAllAuthors conn
    getRouteWithoutParameter "/genres" $ getAllGenres conn
    getRouteWithoutParameter "/engines" $ getAllEngines conn
    getRouteWithoutParameter "/themes" $ getAllThemes conn
    getRouteWithoutParameter "/sides" $ getAllSides conn
    getRouteWithoutParameter "/parties" $ getAllParties conn
    getRouteWithoutParameter "/publishers" $ getAllPublishers conn
    getRouteWithoutParameter "/areas" $ getAllAreas conn
    getRouteWithoutParameter "/mechanics" $ getAllMechanics conn
    getRouteWithoutParameter "/games" $ getAllGames conn
    getRouteWithParameter "/author/:id" $ getAuthor conn
    getRouteWithParameter "/genre/:id" $ getGenre conn
    getRouteWithParameter "/engine/:id" $ getEngine conn
    getRouteWithParameter "/theme/:id" $ getTheme conn
    getRouteWithParameter "/side/:id" $ getSide conn
    getRouteWithParameter "/party/:id" $ getParty conn
    getRouteWithParameter "/publisher/:id" $ getPublisher conn
    getRouteWithParameter "/area/:id" $ getArea conn
    getRouteWithParameter "/mechanic/:id" $ getMechanic conn
    getRouteWithParameter "/game/:id" $ getGame conn

