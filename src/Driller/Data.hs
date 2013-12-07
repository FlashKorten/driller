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
    , Special
    , Series
    , Answer
    , Parameter
    , ParameterValue(..)
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
    , GroupMap
    , GroupMapComponents
    , JoinMap
    , JoinMapComponents
    , ComponentMap
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
    , SpecialList
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
    , Config(..)
    , initConfig
    , ScenarioInfo
    ) where

import Driller.Error ( ParameterError )
import qualified Data.Text as Text ( Text )
import Data.Hashable ( Hashable(..) )
import Data.HashMap.Strict ( HashMap, empty )
import Data.Aeson.TH ( deriveJSON )
import Data.Aeson ( ToJSON(..) )
import Database.PostgreSQL.Simple ( Query, Connection )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Control.Applicative ( (<$>), (<*>) )

-- | Config contains all configuration specifics needed deep down in the db-parts.
-- | Everything in here is constant over the runtime of the program - except for
-- | the @ParameterMap@, which is set for each request that uses parameters.

data Config = Config { getParameterMap :: ParameterMap
                     , getDBConnection :: Connection
                     , getQueryMap     :: QueryMap
                     , getJoinMap      :: JoinMap
                     , getGroupMap     :: GroupMap
                     }

initConfig :: Connection -> QueryMap -> JoinMap -> GroupMap -> Config
initConfig = Config empty

-- | These types contain the data relevant to this drilldown search.

data Genre     = Genre     { getGenreId     :: Int, getGenreName     :: Text.Text }
data Engine    = Engine    { getEngineId    :: Int, getEngineName    :: Text.Text }
data Theme     = Theme     { getThemeId     :: Int, getThemeName     :: Text.Text }
data Mechanic  = Mechanic  { getMechanicId  :: Int, getMechanicName  :: Text.Text }
data Side      = Side      { getSideId      :: Int, getSideName      :: Text.Text }
data Party     = Party     { getPartyId     :: Int, getPartyName     :: Text.Text }
data Publisher = Publisher { getPublisherId :: Int, getPublisherName :: Text.Text }
data Series    = Series    { getSeriesId    :: Int, getSeriesName    :: Text.Text }
data Leader    = Leader    { getLeaderId    :: Int, getLeaderName    :: Text.Text }
data Special   = Special   { getSpecialId   :: Int, getSpecialName   :: Text.Text }
data Author    = Author    { getAuthorId    :: Int, getAuthorName    :: Text.Text }
  deriving Show

newtype FromYear      = FromYear      { getFromYearValue      :: Int }
newtype UpToYear      = UpToYear      { getUpToYearValue      :: Int }
newtype Latitude      = Latitude      { getLatitudeValue      :: Int }
newtype Longitude     = Longitude     { getLongitudeValue     :: Int }
newtype FromRange     = FromRange     { getFromRangeValue     :: Int }
newtype UpToRange     = UpToRange     { getUpToRangeValue     :: Int }
newtype FromTimescale = FromTimescale { getFromTimescaleValue :: Int }
newtype UpToTimescale = UpToTimescale { getUpToTimescaleValue :: Int }

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

data ScenarioInfo = ScenarioInfo { getScInfoTitle       :: Text.Text
                                 , getScInfoSubtitle    :: Text.Text
                                 , getScInfoDescription :: Text.Text
                                 -- , getScInfoStart       :: Text.Text
                                 -- , getScInfoEnd         :: Text.Text
                                 , getScInfoLatitude    :: Double
                                 , getScInfoLongitude   :: Double
                                 , getScInfoRange       :: Int
                                 , getScInfoTimescale   :: Int
                                 , getScInfoPlayersMin  :: Int
                                 , getScInfoPlayersMax  :: Int
                                 }

-- | @Result@ collects all individual informations to be sent as a response to
-- | a drilldown request.

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
                     , getSpecials      :: SpecialList
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
emptyResult =  Result 0 [] d d d d d d d d d d d d d d d d d d d d
                where d = Entries [] []

-- | @Entries@ map to one specific group: text-types map to a @GroupLetter@ with the same first Letter,
-- | number-types map to an implicit range as a @GroupNumber@;
-- | i.e. the year '1976' will map to the group '1950'.
-- | Each group known how many @Entries@ are contained in it.

data GroupLetter = GroupLetter { getGroupLetterGroupId :: Text.Text, getGroupLetterMatches :: Int }
data GroupNumber = GroupNumber { getGroupNumberGroupId :: Int,       getGroupNumberMatches :: Int }

data ParameterValue     = Number Int | GroupID Text.Text deriving (Show, Eq, Ord)
type Parameter          = (Text.Text, ParameterValue)
type ParameterMap       = HashMap Text.Text ParameterValue
type Answer             = Either ParameterError Result
type ComponentMap       = HashMap Text.Text Query
type GroupMap           = HashMap Text.Text GroupMapComponents
type GroupMapComponents = (Query, Query, Query)
type JoinMap            = HashMap Text.Text JoinMapComponents
type JoinMapComponents  = (Query, Query, Query)
type AuthorList         = AnswerList [GroupLetter] [Author]
type GameList           = AnswerList [GroupLetter] [Game]
type GenreList          = AnswerList [GroupLetter] [Genre]
type EngineList         = AnswerList [GroupLetter] [Engine]
type ThemeList          = AnswerList [GroupLetter] [Theme]
type MechanicList       = AnswerList [GroupLetter] [Mechanic]
type SideList           = AnswerList [GroupLetter] [Side]
type PartyList          = AnswerList [GroupLetter] [Party]
type PublisherList      = AnswerList [GroupLetter] [Publisher]
type SeriesList         = AnswerList [GroupLetter] [Series]
type LeaderList         = AnswerList [GroupLetter] [Leader]
type SpecialList        = AnswerList [GroupLetter] [Special]
type FromYearList       = AnswerList [GroupNumber] [FromYear]
type UpToYearList       = AnswerList [GroupNumber] [UpToYear]
type FromRangeList      = AnswerList [GroupNumber] [FromRange]
type UpToRangeList      = AnswerList [GroupNumber] [UpToRange]
type FromTimescaleList  = AnswerList [GroupNumber] [FromTimescale]
type UpToTimescaleList  = AnswerList [GroupNumber] [UpToTimescale]
type LatitudeList       = AnswerList [GroupNumber] [Latitude]
type LongitudeList      = AnswerList [GroupNumber] [Longitude]

-- | An answer is either a list of concrete @Entries@
-- | or a list of @Groups@ if there were too many @Entries@.
-- | In both cases there is a second list of concrete @Entries@ to
-- | show which attributes are no longer useful for filtering, since
-- | they apply to all results.
data AnswerList a b = Groups a b | Entries b b
  deriving (Eq, Show)

data QueryCategory = AUTHOR | LEADER | SIDE | PARTY | SPECIAL
                   | GAME | GENRE | ENGINE | THEME | MECHANIC  | PUBLISHER | SERIES | SCENARIO
                   | LATITUDE | LONGITUDE | FROM_YEAR | UPTO_YEAR
                   | RANGE | FROM_RANGE | UPTO_RANGE
                   | TIMESCALE | FROM_TIMESCALE | UPTO_TIMESCALE deriving (Eq, Show, Enum)
data QueryTarget   = ENTRY  | GROUP | COUNT                      deriving (Eq, Show, Enum)
data QueryType     = MONO   | POLY  | OMNI | POLYA | POLYB       deriving (Eq, Show, Enum)
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
$(deriveJSON (drop 10)  ''Special)
$(deriveJSON (drop 3)  ''Result)

$(deriveJSON (drop 11)  ''FromYear)
$(deriveJSON (drop 11)  ''UpToYear)
$(deriveJSON (drop 11)  ''Latitude)
$(deriveJSON (drop 12)  ''Longitude)
$(deriveJSON (drop 12)  ''FromRange)
$(deriveJSON (drop 12)  ''UpToRange)
$(deriveJSON (drop 16)  ''FromTimescale)
$(deriveJSON (drop 16)  ''UpToTimescale)
$(deriveJSON (drop 11) ''Scenario)
$(deriveJSON (drop 7)  ''Game)
$(deriveJSON (drop 14) ''GroupLetter)
$(deriveJSON (drop 14) ''GroupNumber)
$(deriveJSON id ''AnswerList)
$(deriveJSON (drop 9) ''ScenarioInfo)

-- | A class to promote simple @Int@ values to instances of types represented by numbers.
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

-- | Instances needed for moving data to and from the database.
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
instance FromRow Special     where fromRow = Special     <$> field <*> field
instance FromRow Series      where fromRow = Series      <$> field <*> field
instance FromRow Game        where fromRow = Game        <$> field <*> field <*> field
instance FromRow Scenario    where fromRow = Scenario    <$> field <*> field <*> field <*> field <*> field
instance FromRow GroupLetter where fromRow = GroupLetter <$> field <*> field
instance FromRow GroupNumber where fromRow = GroupNumber <$> field <*> field
instance FromRow Int         where fromRow = field
instance FromRow ScenarioInfo where fromRow = ScenarioInfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field -- <*> field <*> field

instance ToRow Int           where toRow n = [toField n]
instance ToRow Text.Text     where toRow n = [toField n]
instance ToRow ParameterValue where
  toRow (Number n)  = [toField n]
  toRow (GroupID x) = [toField x]

instance ToField ParameterValue where
  toField (Number n)  = toField n
  toField (GroupID x) = toField x


-- | Those categories using mapping tables only use positive integers for reference.
-- | If a query contains a negative value for such a category
-- | this means "every value but this one".

class MarkExclusive a where
  markExclusive :: a -> a

instance MarkExclusive Author    where markExclusive a = a{ getAuthorId    = negate $ getAuthorId a }
instance MarkExclusive Genre     where markExclusive a = a{ getGenreId     = negate $ getGenreId a }
instance MarkExclusive Engine    where markExclusive a = a{ getEngineId    = negate $ getEngineId a }
instance MarkExclusive Theme     where markExclusive a = a{ getThemeId     = negate $ getThemeId a }
instance MarkExclusive Mechanic  where markExclusive a = a{ getMechanicId  = negate $ getMechanicId a }
instance MarkExclusive Side      where markExclusive a = a{ getSideId      = negate $ getSideId a }
instance MarkExclusive Special   where markExclusive a = a{ getSpecialId   = negate $ getSpecialId a }
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
categoryToQuery SPECIAL        = "special"
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

