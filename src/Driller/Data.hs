{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings, TemplateHaskell #-}

module Driller.Data
    ( Theme
    , Side
    , Publisher
    , Party
    , Mechanic
    , Genre
    , GameResult(..)
    , emptyGameResult
    , Game
    , Engine
    , Leader
    , Author
    , Series
    , Answer(..)
    , Parameter
    , ParameterMap
    , YearFrom
    , YearUpTo
    , Latitude
    , Longitude
    , Range
    ) where

import Driller.Error
import qualified Data.Text as Text ( Text )
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Database.PostgreSQL.Simple.Time ( Date )
import Control.Applicative ( (<$>), (<*>) )
import qualified Data.HashMap.Strict as HM

data Genre     = Genre     { getGenreId     :: Int, getGenreName     :: Text.Text }
data Engine    = Engine    { getEngineId    :: Int, getEngineName    :: Text.Text }
data Theme     = Theme     { getThemeId     :: Int, getThemeName     :: Text.Text }
data Mechanic  = Mechanic  { getMechanicId  :: Int, getMechanicName  :: Text.Text }
data Side      = Side      { getSideId      :: Int, getSideName      :: Text.Text }
data Party     = Party     { getPartyId     :: Int, getPartyName     :: Text.Text }
data Publisher = Publisher { getPublisherId :: Int, getPublisherName :: Text.Text }
data Series    = Series    { getSeriesId    :: Int, getSeriesName    :: Text.Text }
data Leader    = Leader    { getLeaderId    :: Int, getLeaderName    :: Text.Text }
data Author    = Author    { getAuthorId    :: Int, getAuthorName    :: Text.Text }
  deriving Show

data YearFrom  = YearFrom  { getValueYearFrom  :: Int }
data YearUpTo  = YearUpTo  { getValueYearUpTo  :: Int }
data Latitude  = Latitude  { getValueLatitude  :: Int }
data Longitude = Longitude { getValueLongitude :: Int }
data Range     = Range     { getValueRange     :: Int }

data GameResult = GameResult { getGames      :: [Game]
                             , getGenres     :: [Genre]
                             , getThemes     :: [Theme]
                             , getMechanics  :: [Mechanic]
                             , getSides      :: [Side]
                             , getParties    :: [Party]
                             , getPublishers :: [Publisher]
                             , getSeries     :: [Series]
                             , getAuthors    :: [Author]
                             , getEngines    :: [Engine]
                             , getLeaders    :: [Leader]
                             , getLatitudes  :: [Latitude]
                             , getLongitudes :: [Longitude]
                             , getYearsFrom  :: [YearFrom]
                             , getYearsUpTo  :: [YearUpTo]
                             , getRanges     :: [Range]
}

emptyGameResult :: GameResult
emptyGameResult = GameResult { getGames      = []
                             , getGenres     = []
                             , getThemes     = []
                             , getMechanics  = []
                             , getSides      = []
                             , getParties    = []
                             , getPublishers = []
                             , getSeries     = []
                             , getAuthors    = []
                             , getEngines    = []
                             , getLeaders    = []
                             , getLatitudes  = []
                             , getLongitudes = []
                             , getYearsFrom  = []
                             , getYearsUpTo  = []
                             , getRanges     = []
                             }

data Game = Game { getGameId        :: Int
                 , getGameTitle     :: Text.Text
                 , getGameSubtitle  :: Text.Text
                 , getPlayersMin    :: Int
                 , getPlayersMax    :: Int
                 , getGametimeStart :: Date
                 , getGametimeEnd   :: Date
                 , getBggId         :: Text.Text
                 }

instance FromJSON Game where
  parseJSON (Object o) = Game <$>
    o .: "id" <*>
    o .: "title" <*>
    o .: "subtitle" <*>
    o .: "minPlayers" <*>
    o .: "maxPlayers" <*>
    o .: "gametimeStart" <*>
    o .: "gametimeEnd" <*>
    o .: "bggid"

instance ToJSON Game where
  toJSON g = object [ "title" .= getGameTitle g
                    , "subtitle" .= getGameSubtitle g
                    , "minPlayers" .= getPlayersMin g
                    , "maxPlayers" .= getPlayersMax g
                    , "gametimeStart" .= show (getGametimeStart g)
                    , "gametimeEnd" .= show (getGametimeEnd g)
                    , "bggid" .= getBggId g
                    ]

type Parameter = (Text.Text, Int)
type ParameterMap = HM.HashMap Text.Text Int
type Answer = Either ParameterError GameResult

instance ToJSON Answer where
  toJSON (Left e)  = toJSON e
  toJSON (Right r) = toJSON r

instance FromJSON Date where
  parseJSON (Object _) = undefined

$(deriveJSON (drop 9)  ''Author)
$(deriveJSON (drop 8)  ''Genre)
$(deriveJSON (drop 9)  ''Engine)
$(deriveJSON (drop 8)  ''Theme)
$(deriveJSON (drop 11) ''Mechanic)
$(deriveJSON (drop 7)  ''Side)
$(deriveJSON (drop 8)  ''Party)
$(deriveJSON (drop 12) ''Publisher)
$(deriveJSON (drop 9)  ''Series)
$(deriveJSON (drop 9)  ''Leader)
$(deriveJSON (drop 3)  ''GameResult)

$(deriveJSON (drop 8)  ''YearFrom)
$(deriveJSON (drop 8)  ''YearUpTo)
$(deriveJSON (drop 8)  ''Latitude)
$(deriveJSON (drop 8)  ''Longitude)
$(deriveJSON (drop 8)  ''Range)

instance FromRow YearFrom  where fromRow = YearFrom  <$> field
instance FromRow YearUpTo  where fromRow = YearUpTo  <$> field
instance FromRow Latitude  where fromRow = Latitude  <$> field
instance FromRow Longitude where fromRow = Longitude <$> field
instance FromRow Range     where fromRow = Range     <$> field

instance FromRow Author    where fromRow = Author    <$> field <*> field
instance FromRow Genre     where fromRow = Genre     <$> field <*> field
instance FromRow Engine    where fromRow = Engine    <$> field <*> field
instance FromRow Theme     where fromRow = Theme     <$> field <*> field
instance FromRow Mechanic  where fromRow = Mechanic  <$> field <*> field
instance FromRow Side      where fromRow = Side      <$> field <*> field
instance FromRow Party     where fromRow = Party     <$> field <*> field
instance FromRow Leader    where fromRow = Leader    <$> field <*> field
instance FromRow Publisher where fromRow = Publisher <$> field <*> field
instance FromRow Series    where fromRow = Series    <$> field <*> field
instance FromRow Game      where fromRow = Game      <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance FromRow Int       where fromRow = field
instance ToRow Int         where toRow n = [toField n]

