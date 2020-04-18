{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Brig.Types.Provider.Tag where

import Cassandra.CQL (Cql)
import Data.Aeson
import Data.Bits
import Data.ByteString.Conversion
import Data.List (foldl')
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Imports

--------------------------------------------------------------------------------
-- ServiceTag

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
  deriving (Eq, Show, Ord, Enum, Bounded)

instance FromByteString ServiceTag where
  parser = parser >>= \t -> case (t :: ByteString) of
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

instance FromJSON ServiceTag where
  parseJSON =
    withText "ServiceTag" $
      either fail pure . runParser parser . Text.encodeUtf8

instance ToJSON ServiceTag where
  toJSON = String . Text.decodeUtf8 . toByteString'

--------------------------------------------------------------------------------
-- ServiceTag Matchers

-- | Logical disjunction of 'MatchAllTags' to match.
newtype MatchAny
  = MatchAny
      {matchAnySet :: Set MatchAll}
  deriving (Eq, Show, Ord)

-- | Logical conjunction of 'ServiceTag's to match.
newtype MatchAll
  = MatchAll
      {matchAllSet :: Set ServiceTag}
  deriving (Eq, Show, Ord)

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

newtype Bucket = Bucket Int32
  deriving newtype (Cql, Show)

-- | Bucketing allows us to distribute individual tag bitmasks
-- across multiple wide rows, if it should become necessary.
-- If a tag bitmask it spread across buckets, lookups and deletes
-- on that bitmask will require O(n) queries, where /n/ is the number
-- of buckets for a specific tag, whereas writes can stay at O(1)
-- by always writing to the "newest" bucket.
defBucket :: Bucket
defBucket = Bucket 201608

foldTags :: Range 1 3 (Set ServiceTag) -> Int64
foldTags = foldl' (.|.) 0 . map tagToInt . Set.toList . fromRange

unfoldTags :: Range 0 3 (Set ServiceTag) -> [Int64]
unfoldTags s = case map tagToInt (Set.toList (fromRange s)) of
  [] -> []
  [t] -> [t]
  ts@[t, u] -> (t .|. u) : ts
  ts@[t, u, v] -> (t .|. u) : (t .|. v) : (u .|. v) : (t .|. u .|. v) : ts
  _ -> error "Brig.Provider.DB.Tag: unfoldTags: Too many tags."

unfoldTagsInto :: Range 1 3 (Set ServiceTag) -> [Int64] -> [Int64]
unfoldTagsInto xs ys =
  let xs' = unfoldTags (rcast xs)
   in xs' ++ concatMap (\x -> map (.|. x) ys) xs'

diffTags ::
  Range 0 3 (Set ServiceTag) ->
  Range 0 3 (Set ServiceTag) ->
  Range 0 3 (Set ServiceTag)
diffTags a b = unsafeRange $ Set.difference (fromRange a) (fromRange b)

nonEmptyTags :: Range m 3 (Set ServiceTag) -> Maybe (Range 1 3 (Set ServiceTag))
nonEmptyTags r
  | Set.null (fromRange r) = Nothing
  | otherwise = Just (unsafeRange (fromRange r))

tagToInt :: ServiceTag -> Int64
tagToInt AudioTag = 0b1
tagToInt BooksTag = 0b10
tagToInt BusinessTag = 0b100
tagToInt DesignTag = 0b1000
tagToInt EducationTag = 0b10000
tagToInt EntertainmentTag = 0b100000
tagToInt FinanceTag = 0b1000000
tagToInt FitnessTag = 0b10000000
tagToInt FoodDrinkTag = 0b100000000
tagToInt GamesTag = 0b1000000000
tagToInt GraphicsTag = 0b10000000000
tagToInt HealthTag = 0b100000000000
tagToInt IntegrationTag = 0b1000000000000
tagToInt LifestyleTag = 0b10000000000000
tagToInt MediaTag = 0b100000000000000
tagToInt MedicalTag = 0b1000000000000000
tagToInt MoviesTag = 0b10000000000000000
tagToInt MusicTag = 0b100000000000000000
tagToInt NewsTag = 0b1000000000000000000
tagToInt PhotographyTag = 0b10000000000000000000
tagToInt PollTag = 0b100000000000000000000
tagToInt ProductivityTag = 0b1000000000000000000000
tagToInt QuizTag = 0b10000000000000000000000
tagToInt RatingTag = 0b100000000000000000000000
tagToInt ShoppingTag = 0b1000000000000000000000000
tagToInt SocialTag = 0b10000000000000000000000000
tagToInt SportsTag = 0b100000000000000000000000000
tagToInt TravelTag = 0b1000000000000000000000000000
tagToInt TutorialTag = 0b10000000000000000000000000000
tagToInt VideoTag = 0b100000000000000000000000000000
tagToInt WeatherTag = 0b1000000000000000000000000000000

intToTag :: Int64 -> Maybe ServiceTag
intToTag 0b1 = pure AudioTag
intToTag 0b10 = pure BooksTag
intToTag 0b100 = pure BusinessTag
intToTag 0b1000 = pure DesignTag
intToTag 0b10000 = pure EducationTag
intToTag 0b100000 = pure EntertainmentTag
intToTag 0b1000000 = pure FinanceTag
intToTag 0b10000000 = pure FitnessTag
intToTag 0b100000000 = pure FoodDrinkTag
intToTag 0b1000000000 = pure GamesTag
intToTag 0b10000000000 = pure GraphicsTag
intToTag 0b100000000000 = pure HealthTag
intToTag 0b1000000000000 = pure IntegrationTag
intToTag 0b10000000000000 = pure LifestyleTag
intToTag 0b100000000000000 = pure MediaTag
intToTag 0b1000000000000000 = pure MedicalTag
intToTag 0b10000000000000000 = pure MoviesTag
intToTag 0b100000000000000000 = pure MusicTag
intToTag 0b1000000000000000000 = pure NewsTag
intToTag 0b10000000000000000000 = pure PhotographyTag
intToTag 0b100000000000000000000 = pure PollTag
intToTag 0b1000000000000000000000 = pure ProductivityTag
intToTag 0b10000000000000000000000 = pure QuizTag
intToTag 0b100000000000000000000000 = pure RatingTag
intToTag 0b1000000000000000000000000 = pure ShoppingTag
intToTag 0b10000000000000000000000000 = pure SocialTag
intToTag 0b100000000000000000000000000 = pure SportsTag
intToTag 0b1000000000000000000000000000 = pure TravelTag
intToTag 0b10000000000000000000000000000 = pure TutorialTag
intToTag 0b100000000000000000000000000000 = pure VideoTag
intToTag 0b1000000000000000000000000000000 = pure WeatherTag
intToTag _ = Nothing
