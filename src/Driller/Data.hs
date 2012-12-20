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
    , FromYear
    , UpToYear
    , Latitude
    , Longitude
    , FromRange
    , UpToRange
    , fromInt
    , FromInt
    ) where


import Driller.Error ( ParameterError )
import qualified Data.Text as Text ( Text )
import qualified Data.HashMap.Strict as HM ( HashMap )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson ( Value(Object), FromJSON(..), ToJSON(..), (.=), (.:), object )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Control.Applicative ( (<$>), (<*>) )

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

newtype FromYear  = FromYear  { getValueFromYear  :: Int }
newtype UpToYear  = UpToYear  { getValueUpToYear  :: Int }
newtype Latitude  = Latitude  { getValueLatitude  :: Int }
newtype Longitude = Longitude { getValueLongitude :: Int }
newtype FromRange = FromRange { getValueFromRange :: Int }
newtype UpToRange = UpToRange { getValueUpToRange :: Int }

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
                             , getFromYears  :: [FromYear]
                             , getUpToYears  :: [UpToYear]
                             , getFromRanges :: [FromRange]
                             , getUpToRanges :: [UpToRange]
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
                             , getFromYears  = []
                             , getUpToYears  = []
                             , getFromRanges = []
                             , getUpToRanges = []
                             }

data Game = Game { getGameId        :: Int
                 , getGameTitle     :: Text.Text
                 , getGameSubtitle  :: Text.Text
                 , getPlayersMin    :: Int
                 , getPlayersMax    :: Int
                 , getBggId         :: Text.Text
                 }

instance FromJSON Game where
  parseJSON (Object o) = Game <$>
    o .: "id" <*>
    o .: "title" <*>
    o .: "subtitle" <*>
    o .: "minPlayers" <*>
    o .: "maxPlayers" <*>
    o .: "bggid"

instance ToJSON Game where
  toJSON g = object [ "title" .= getGameTitle g
                    , "subtitle" .= getGameSubtitle g
                    , "minPlayers" .= getPlayersMin g
                    , "maxPlayers" .= getPlayersMax g
                    , "bggid" .= getBggId g
                    ]

type Parameter = (Text.Text, Int)
type ParameterMap = HM.HashMap Text.Text Int
type Answer = Either ParameterError GameResult

instance ToJSON Answer where
  toJSON (Left e)  = toJSON e
  toJSON (Right r) = toJSON r

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

$(deriveJSON (drop 8)  ''FromYear)
$(deriveJSON (drop 8)  ''UpToYear)
$(deriveJSON (drop 8)  ''Latitude)
$(deriveJSON (drop 8)  ''Longitude)
$(deriveJSON (drop 8)  ''FromRange)
$(deriveJSON (drop 8)  ''UpToRange)

class FromInt a where
  fromInt :: Int -> a

instance FromInt FromYear where
  fromInt i = FromYear {getValueFromYear = i}
instance FromInt UpToYear where
  fromInt i = UpToYear {getValueUpToYear = i}
instance FromInt FromRange where
  fromInt i = FromRange {getValueFromRange = i}
instance FromInt UpToRange where
  fromInt i = UpToRange {getValueUpToRange = i}
instance FromInt Latitude where
  fromInt i = Latitude {getValueLatitude = i}
instance FromInt Longitude where
  fromInt i = Longitude {getValueLongitude = i}

instance FromRow FromYear  where fromRow = FromYear  <$> field
instance FromRow UpToYear  where fromRow = UpToYear  <$> field
instance FromRow Latitude  where fromRow = Latitude  <$> field
instance FromRow Longitude where fromRow = Longitude <$> field
instance FromRow FromRange where fromRow = FromRange <$> field
instance FromRow UpToRange where fromRow = UpToRange <$> field

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
instance FromRow Game      where fromRow = Game      <$> field <*> field <*> field <*> field <*> field <*> field
instance FromRow Int       where fromRow = field
instance ToRow Int         where toRow n = [toField n]

