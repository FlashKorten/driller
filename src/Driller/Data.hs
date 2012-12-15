{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Driller.Data
    ( Theme
    , Side
    , Publisher
    , Party
    , Mechanic
    , Genre
    , GameResult(..)
    , Game
    , Engine
    , Author
    , Area
    ) where

import qualified Data.Text as Text ( Text )
import Data.Aeson.TH ( deriveJSON )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Control.Applicative ( (<$>), (<*>) )

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
