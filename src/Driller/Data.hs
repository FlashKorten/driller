{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings, TemplateHaskell #-}

module Driller.Data
    ( Theme
    , Side
    , Publisher
    , Party
    , Mechanic
    , Genre
    , Result(..)
    , emptyResult
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
    , GameList
    , GenreList
    , EngineList
    , ThemeList
    , MechanicList
    , SideList
    , PartyList
    , PublisherList
    , SeriesList
    , LeaderList
    , FromYearList
    , UpToYearList
    , FromRangeList
    , UpToRangeList
    , FromTimescaleList
    , UpToTimescaleList
    , LatitudeList
    , LongitudeList
    , FromRow
    , AnswerList(..)
    , QueryCategory(..)
    , QueryTarget(..)
    , QueryType(..)
    , QueryKey
    , QueryMap
    , categoryToQuery
    ) where

import Driller.Error ( ParameterError )
import qualified Data.Text as Text ( Text )
import Data.Hashable ( Hashable(..) )
import Data.HashMap.Strict ( HashMap )
import Database.PostgreSQL.Simple ( Query )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson ( ToJSON(..) )
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

data Result = Result { getNoResults     :: Int
                     , getScenarios     :: [Scenario]
                     , getGames         :: GameList
                     , getGenres        :: GenreList
                     , getThemes        :: ThemeList
                     , getMechanics     :: MechanicList
                     , getSides         :: SideList
                     , getParties       :: PartyList
                     , getPublishers    :: PublisherList
                     , getSeries        :: SeriesList
                     , getAuthors       :: AuthorList
                     , getEngines       :: EngineList
                     , getLeaders       :: LeaderList
                     , getLatitudes     :: LatitudeList
                     , getLongitudes    :: LongitudeList
                     , getFromYears     :: FromYearList
                     , getUpToYears     :: UpToYearList
                     , getFromRanges    :: FromRangeList
                     , getUpToRanges    :: UpToRangeList
                     , getFromTimescales :: FromTimescaleList
                     , getUpToTimescales :: UpToTimescaleList
                     }

emptyResult :: Result
emptyResult =  Result 0 [] d d d d d d d d d d d d d d d d d d d
                where d = Entries []

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

type Parameter         = (Text.Text, Int)
type ParameterMap      = HashMap Text.Text Int
type Answer            = Either ParameterError Result
type JoinMap           = HashMap Text.Text (Query, Query, Query)
type JoinComponentMap  = HashMap Text.Text Query
type AuthorList        = AnswerList [GroupLetter] [Author]
type GameList          = AnswerList [GroupLetter] [Game]
type GenreList         = AnswerList [GroupLetter] [Genre]
type EngineList        = AnswerList [GroupLetter] [Engine]
type ThemeList         = AnswerList [GroupLetter] [Theme]
type MechanicList      = AnswerList [GroupLetter] [Mechanic]
type SideList          = AnswerList [GroupLetter] [Side]
type PartyList         = AnswerList [GroupLetter] [Party]
type PublisherList     = AnswerList [GroupLetter] [Publisher]
type SeriesList        = AnswerList [GroupLetter] [Series]
type LeaderList        = AnswerList [GroupLetter] [Leader]
type FromYearList      = AnswerList [GroupNumber] [FromYear]
type UpToYearList      = AnswerList [GroupNumber] [UpToYear]
type FromRangeList     = AnswerList [GroupNumber] [FromRange]
type UpToRangeList     = AnswerList [GroupNumber] [UpToRange]
type FromTimescaleList = AnswerList [GroupNumber] [FromTimescale]
type UpToTimescaleList = AnswerList [GroupNumber] [UpToTimescale]
type LatitudeList      = AnswerList [GroupNumber] [Latitude]
type LongitudeList     = AnswerList [GroupNumber] [Longitude]

data AnswerList a b = Groups a | Entries b
  deriving (Eq, Show)

data QueryCategory = AUTHOR | GAME | GENRE | ENGINE | THEME | MECHANIC | SIDE | PARTY | PUBLISHER | SERIES | LEADER | SCENARIO
                   | LATITUDE | LONGITUDE | FROM_YEAR | UPTO_YEAR | RANGE | FROM_RANGE | UPTO_RANGE | TIMESCALE | FROM_TIMESCALE | UPTO_TIMESCALE deriving (Eq, Show, Enum)
data QueryTarget   = ENTRY  | GROUP | COUNT                       deriving (Eq, Show, Enum)
data QueryType     = MONO   | POLY  | OMNI                        deriving (Eq, Show, Enum)
type QueryKey      = (QueryCategory, QueryTarget, QueryType)
type QueryMap      = HashMap QueryKey Query

instance Hashable QueryCategory where
  hashWithSalt salt qc = hashWithSalt (fromEnum qc) salt

instance Hashable QueryTarget where
  hashWithSalt salt qc = hashWithSalt (fromEnum qc) salt

instance Hashable QueryType where
  hashWithSalt salt qc = hashWithSalt (fromEnum qc) salt

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
$(deriveJSON (drop 3)  ''Result)

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
$(deriveJSON id ''AnswerList)

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

categoryToQuery :: QueryCategory -> Query
categoryToQuery AUTHOR         = "author"
categoryToQuery GAME           = "game"
categoryToQuery GENRE          = "genre"
categoryToQuery ENGINE         = "engine"
categoryToQuery THEME          = "theme"
categoryToQuery MECHANIC       = "mechanic"
categoryToQuery SIDE           = "side"
categoryToQuery PARTY          = "party"
categoryToQuery PUBLISHER      = "publisher"
categoryToQuery SERIES         = "series"
categoryToQuery LEADER         = "leader"
categoryToQuery LATITUDE       = "latitude_trunc"
categoryToQuery LONGITUDE      = "longitude_trunc"
categoryToQuery FROM_YEAR      = "year_from"
categoryToQuery UPTO_YEAR      = "year_upto"
categoryToQuery RANGE          = "range"
categoryToQuery FROM_RANGE     = "range"
categoryToQuery UPTO_RANGE     = "range"
categoryToQuery TIMESCALE      = "timescale"
categoryToQuery FROM_TIMESCALE = "timescale"
categoryToQuery UPTO_TIMESCALE = "timescale"
categoryToQuery SCENARIO       = "scenario"

