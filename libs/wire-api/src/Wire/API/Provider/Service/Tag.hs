{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Provider.Service.Tag
  ( -- * ServiceTag
    ServiceTag (..),
    ServiceTagList (..),

    -- * Bounded ServiceTag Queries
    QueryAnyTags (..),
    queryAnyTags,
    QueryAllTags (..),
    queryAllTags,

    -- * ServiceTag Matchers
    MatchAny (..),
    MatchAll (..),
    (.||.),
    (.&&.),
    matchAll,
    match1,
    match,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Range (LTE, Range, fromRange)
import qualified Data.Range as Range
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import GHC.TypeLits (KnownNat, Nat)
import Imports
import Wire.API.Arbitrary (Arbitrary (..), GenericUniform (..))

--------------------------------------------------------------------------------
-- ServiceTag

newtype ServiceTagList = ServiceTagList [ServiceTag]
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Arbitrary)

-- | A fixed enumeration of tags for services.
data ServiceTag
  = AudioTag
  | BooksTag
  | BusinessTag
  | DesignTag
  | EducationTag
  | EntertainmentTag
  | FinanceTag
  | FitnessTag
  | FoodDrinkTag
  | GamesTag
  | GraphicsTag
  | HealthTag
  | IntegrationTag
  | LifestyleTag
  | MediaTag
  | MedicalTag
  | MoviesTag
  | MusicTag
  | NewsTag
  | PhotographyTag
  | PollTag
  | ProductivityTag
  | QuizTag
  | RatingTag
  | ShoppingTag
  | SocialTag
  | SportsTag
  | TravelTag
  | TutorialTag
  | VideoTag
  | WeatherTag
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform ServiceTag)

instance FromByteString ServiceTag where
  parser =
    parser >>= \t -> case (t :: ByteString) of
      "audio" -> pure AudioTag
      "books" -> pure BooksTag
      "business" -> pure BusinessTag
      "design" -> pure DesignTag
      "education" -> pure EducationTag
      "entertainment" -> pure EntertainmentTag
      "finance" -> pure FinanceTag
      "fitness" -> pure FitnessTag
      "food-drink" -> pure FoodDrinkTag
      "games" -> pure GamesTag
      "graphics" -> pure GraphicsTag
      "health" -> pure HealthTag
      "integration" -> pure IntegrationTag
      "lifestyle" -> pure LifestyleTag
      "media" -> pure MediaTag
      "medical" -> pure MedicalTag
      "movies" -> pure MoviesTag
      "music" -> pure MusicTag
      "news" -> pure NewsTag
      "photography" -> pure PhotographyTag
      "poll" -> pure PollTag
      "productivity" -> pure ProductivityTag
      "quiz" -> pure QuizTag
      "rating" -> pure RatingTag
      "shopping" -> pure ShoppingTag
      "social" -> pure SocialTag
      "sports" -> pure SportsTag
      "travel" -> pure TravelTag
      "tutorial" -> pure TutorialTag
      "video" -> pure VideoTag
      "weather" -> pure WeatherTag
      _ -> fail $ "Invalid tag: " ++ show t

instance ToByteString ServiceTag where
  builder AudioTag = "audio"
  builder BooksTag = "books"
  builder BusinessTag = "business"
  builder DesignTag = "design"
  builder EducationTag = "education"
  builder EntertainmentTag = "entertainment"
  builder FinanceTag = "finance"
  builder FitnessTag = "fitness"
  builder FoodDrinkTag = "food-drink"
  builder GamesTag = "games"
  builder GraphicsTag = "graphics"
  builder HealthTag = "health"
  builder IntegrationTag = "integration"
  builder LifestyleTag = "lifestyle"
  builder MediaTag = "media"
  builder MedicalTag = "medical"
  builder MoviesTag = "movies"
  builder MusicTag = "music"
  builder NewsTag = "news"
  builder PhotographyTag = "photography"
  builder PollTag = "poll"
  builder ProductivityTag = "productivity"
  builder QuizTag = "quiz"
  builder RatingTag = "rating"
  builder ShoppingTag = "shopping"
  builder SocialTag = "social"
  builder SportsTag = "sports"
  builder TravelTag = "travel"
  builder TutorialTag = "tutorial"
  builder VideoTag = "video"
  builder WeatherTag = "weather"

instance ToJSON ServiceTag where
  toJSON = JSON.String . Text.decodeUtf8 . toByteString'

instance FromJSON ServiceTag where
  parseJSON =
    JSON.withText "ServiceTag" $
      either fail pure . runParser parser . Text.encodeUtf8

--------------------------------------------------------------------------------
-- Bounded ServiceTag Queries

-- | Bounded logical disjunction of 'm' to 'n' 'QueryAllTags'.
newtype QueryAnyTags (m :: Nat) (n :: Nat) = QueryAnyTags
  {queryAnyTagsRange :: Range m n (Set (QueryAllTags m n))}
  deriving stock (Eq, Show, Ord)

instance (KnownNat m, KnownNat n, LTE m n) => Arbitrary (QueryAnyTags m n) where
  arbitrary = QueryAnyTags <$> arbitrary

queryAnyTags :: LTE m n => MatchAny -> Maybe (QueryAnyTags m n)
queryAnyTags t = do
  x <- mapM queryAllTags (Set.toList (matchAnySet t))
  QueryAnyTags <$> Range.checked (Set.fromList x)

-- | QueryAny ::= QueryAll { "," QueryAll }
instance ToByteString (QueryAnyTags m n) where
  builder =
    BB.byteString
      . C8.intercalate ","
      . map toByteString'
      . Set.toList
      . fromRange
      . queryAnyTagsRange

-- | QueryAny ::= QueryAll { "," QueryAll }
instance LTE m n => FromByteString (QueryAnyTags m n) where
  parser = do
    bs <- C8.split ',' <$> parser
    ts <- mapM (either fail pure . runParser parser) bs
    rs <- either fail pure (Range.checkedEither (Set.fromList ts))
    return $! QueryAnyTags rs

-- | Bounded logical conjunction of 'm' to 'n' 'ServiceTag's to match.
newtype QueryAllTags (m :: Nat) (n :: Nat) = QueryAllTags
  {queryAllTagsRange :: Range m n (Set ServiceTag)}
  deriving stock (Eq, Show, Ord)

instance (KnownNat m, KnownNat n, LTE m n) => Arbitrary (QueryAllTags m n) where
  arbitrary = QueryAllTags <$> arbitrary

queryAllTags :: LTE m n => MatchAll -> Maybe (QueryAllTags m n)
queryAllTags = fmap QueryAllTags . Range.checked . matchAllSet

-- | QueryAll ::= tag { "." tag }
instance ToByteString (QueryAllTags m n) where
  builder =
    BB.byteString
      . C8.intercalate "."
      . map toByteString'
      . Set.toList
      . fromRange
      . queryAllTagsRange

-- | QueryAll ::= tag { "." tag }
instance LTE m n => FromByteString (QueryAllTags m n) where
  parser = do
    bs <- C8.split '.' <$> parser
    ts <- mapM (either fail pure . runParser parser) bs
    rs <- either fail pure (Range.checkedEither (Set.fromList ts))
    return $! QueryAllTags rs

--------------------------------------------------------------------------------
-- ServiceTag Matchers

-- | Logical disjunction of 'MatchAllTags' to match.
newtype MatchAny = MatchAny
  {matchAnySet :: Set MatchAll}
  deriving stock (Eq, Show, Ord)

-- | Logical conjunction of 'ServiceTag's to match.
newtype MatchAll = MatchAll
  {matchAllSet :: Set ServiceTag}
  deriving stock (Eq, Show, Ord)

(.||.) :: MatchAny -> MatchAny -> MatchAny
(.||.) (MatchAny a) (MatchAny b) = MatchAny (Set.union a b)

(.&&.) :: MatchAll -> MatchAll -> MatchAll
(.&&.) (MatchAll a) (MatchAll b) = MatchAll (Set.union a b)

matchAll :: MatchAll -> MatchAny
matchAll = MatchAny . Set.singleton

match1 :: ServiceTag -> MatchAny
match1 = matchAll . match

match :: ServiceTag -> MatchAll
match = MatchAll . Set.singleton
