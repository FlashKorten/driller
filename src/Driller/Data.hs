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
    , Answer
    , Parameter
    , ParameterMap
    , FromYear
    , UpToYear
    , Latitude
    , Longitude
    , FromRange
    , UpToRange
    , FromTimescale
    , UpToTimescale
    , Scenario
    , fromInt
    , FromInt
    , JoinMap
    , JoinComponentMap
    , MarkExclusive
    , markExclusive
    , GroupLetter
    , GroupNumber
    , AuthorList
    ) where

import Driller.Error ( ParameterError )
import qualified Data.Text as Text ( Text )
import Data.HashMap.Strict ( HashMap )
import Database.PostgreSQL.Simple ( Query )
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

data GroupLetter = GroupLetter { getGroupLetterPrefix :: Text.Text, getGroupLetterMatches :: Int }
data GroupNumber = GroupNumber { getGroupNumberNumber :: Int,       getGroupNumberMatches :: Int }

newtype FromYear      = FromYear      { getValueFromYear      :: Int }
newtype UpToYear      = UpToYear      { getValueUpToYear      :: Int }
newtype Latitude      = Latitude      { getValueLatitude      :: Int }
newtype Longitude     = Longitude     { getValueLongitude     :: Int }
newtype FromRange     = FromRange     { getValueFromRange     :: Int }
newtype UpToRange     = UpToRange     { getValueUpToRange     :: Int }
newtype FromTimescale = FromTimescale { getValueFromTimescale :: Int }
newtype UpToTimescale = UpToTimescale { getValueUpToTimescale :: Int }

data GameResult = GameResult { getNoResults     :: Int
                             , getGames         :: [Game]
                             , getGenres        :: [Genre]
                             , getThemes        :: [Theme]
                             , getMechanics     :: [Mechanic]
                             , getSides         :: [Side]
                             , getParties       :: [Party]
                             , getPublishers    :: [Publisher]
                             , getSeries        :: [Series]
                             , getAuthors       :: AuthorList
                             , getEngines       :: [Engine]
                             , getLeaders       :: [Leader]
                             , getScenarios     :: [Scenario]
                             , getLatitudes     :: [Latitude]
                             , getLongitudes    :: [Longitude]
                             , getFromYears     :: [FromYear]
                             , getUpToYears     :: [UpToYear]
                             , getFromRanges    :: [FromRange]
                             , getUpToRanges    :: [UpToRange]
                             , getFromTimescales :: [FromTimescale]
                             , getUpToTimescales :: [UpToTimescale]
                             }

emptyGameResult :: GameResult
emptyGameResult =  GameResult 0 [] [] [] [] [] [] [] [] (Right []) [] [] [] [] [] [] [] [] [] [] []

data Game = Game { getGameId        :: Int
                 , getGameTitle     :: Text.Text
                 , getGameSubtitle  :: Text.Text
                 }

data Scenario = Scenario { getScenarioId       :: Int
                         , getScenarioTitle    :: Text.Text
                         , getScenarioSubtitle :: Text.Text
                         , getScenarioFromYear :: Int
                         , getScenarioUpToYear :: Int
                         }

type Parameter        = (Text.Text, Int)
type ParameterMap     = HashMap Text.Text Int
type Answer           = Either ParameterError GameResult
type JoinMap          = HashMap Text.Text (Query, Query, Query)
type JoinComponentMap = HashMap Text.Text Query
type AuthorList       = Either [GroupLetter] [Author]

instance ToJSON Answer where
  toJSON (Left e)  = toJSON e
  toJSON (Right r) = toJSON r

instance ToJSON AuthorList where
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
$(deriveJSON (drop 8)  ''FromTimescale)
$(deriveJSON (drop 8)  ''UpToTimescale)
$(deriveJSON (drop 11) ''Scenario)
$(deriveJSON (drop 7)  ''Game)
$(deriveJSON (drop 14) ''GroupLetter)
$(deriveJSON (drop 14) ''GroupNumber)

class FromInt a where
  fromInt :: Int -> a

instance FromInt FromYear  where fromInt = FromYear
instance FromInt UpToYear  where fromInt = UpToYear
instance FromInt FromRange where fromInt = FromRange
instance FromInt UpToRange where fromInt = UpToRange
instance FromInt Latitude  where fromInt = Latitude
instance FromInt Longitude where fromInt = Longitude
instance FromInt FromTimescale where fromInt = FromTimescale
instance FromInt UpToTimescale where fromInt = UpToTimescale

instance FromRow FromYear      where fromRow = FromYear      <$> field
instance FromRow UpToYear      where fromRow = UpToYear      <$> field
instance FromRow Latitude      where fromRow = Latitude      <$> field
instance FromRow Longitude     where fromRow = Longitude     <$> field
instance FromRow FromRange     where fromRow = FromRange     <$> field
instance FromRow UpToRange     where fromRow = UpToRange     <$> field
instance FromRow FromTimescale where fromRow = FromTimescale <$> field
instance FromRow UpToTimescale where fromRow = UpToTimescale <$> field

instance FromRow Author      where fromRow = Author      <$> field <*> field
instance FromRow Genre       where fromRow = Genre       <$> field <*> field
instance FromRow Engine      where fromRow = Engine      <$> field <*> field
instance FromRow Theme       where fromRow = Theme       <$> field <*> field
instance FromRow Mechanic    where fromRow = Mechanic    <$> field <*> field
instance FromRow Side        where fromRow = Side        <$> field <*> field
instance FromRow Party       where fromRow = Party       <$> field <*> field
instance FromRow Leader      where fromRow = Leader      <$> field <*> field
instance FromRow Publisher   where fromRow = Publisher   <$> field <*> field
instance FromRow Series      where fromRow = Series      <$> field <*> field
instance FromRow Game        where fromRow = Game        <$> field <*> field <*> field
instance FromRow Scenario    where fromRow = Scenario    <$> field <*> field <*> field <*> field <*> field
instance FromRow GroupLetter where fromRow = GroupLetter <$> field <*> field
instance FromRow GroupNumber where fromRow = GroupNumber <$> field <*> field
instance FromRow Int         where fromRow = field

instance ToRow Int           where toRow n = [toField n]
instance ToRow Text.Text     where toRow n = [toField n]

class MarkExclusive a where
  markExclusive :: a -> a

instance MarkExclusive Author    where markExclusive a = a{ getAuthorId    = negate $ getAuthorId a }
instance MarkExclusive Genre     where markExclusive a = a{ getGenreId     = negate $ getGenreId a }
instance MarkExclusive Engine    where markExclusive a = a{ getEngineId    = negate $ getEngineId a }
instance MarkExclusive Theme     where markExclusive a = a{ getThemeId     = negate $ getThemeId a }
instance MarkExclusive Mechanic  where markExclusive a = a{ getMechanicId  = negate $ getMechanicId a }
instance MarkExclusive Side      where markExclusive a = a{ getSideId      = negate $ getSideId a }
instance MarkExclusive Party     where markExclusive a = a{ getPartyId     = negate $ getPartyId a }
instance MarkExclusive Leader    where markExclusive a = a{ getLeaderId    = negate $ getLeaderId a }
instance MarkExclusive Publisher where markExclusive a = a{ getPublisherId = negate $ getPublisherId a }
instance MarkExclusive Series    where markExclusive a = a{ getSeriesId    = negate $ getSeriesId a }
instance MarkExclusive Game      where markExclusive a = a{ getGameId      = negate $ getGameId a }

instance MarkExclusive a => MarkExclusive [a] where markExclusive = map markExclusive

