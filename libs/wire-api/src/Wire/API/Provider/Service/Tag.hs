{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    matchAll,
    match1,
    match,
  )
where

import Control.Lens (Prism', prism)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Attoparsec.ByteString (IResult (..), parse)
import Data.ByteString (toStrict)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Conversion
import Data.OpenApi qualified as S
import Data.Range (Range, fromRange, rangedSchema)
import Data.Range qualified as Range
import Data.Schema
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Type.Ord
import GHC.TypeLits (KnownNat, Nat)
import Imports
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

--------------------------------------------------------------------------------
-- ServiceTag

newtype ServiceTagList = ServiceTagList [ServiceTag]
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Arbitrary)
  deriving (S.ToSchema) via (Schema ServiceTagList)

_ServiceTagList :: Prism' ServiceTagList [ServiceTag]
_ServiceTagList = prism ServiceTagList (\(ServiceTagList l) -> pure l)

instance ToSchema ServiceTagList where
  schema = named "ServiceTagList" $ tag _ServiceTagList $ array schema

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
  deriving (S.ToSchema, ToJSON, FromJSON) via (Schema ServiceTag)

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

instance ToSchema ServiceTag where
  schema = enum @Text "" . mconcat $ (\a -> element (decodeUtf8With lenientDecode $ toStrict $ toByteString a) a) <$> [minBound ..]

instance S.ToParamSchema ServiceTag where
  toParamSchema _ =
    mempty
      { S._schemaType = Just S.OpenApiString,
        S._schemaEnum = Just (toJSON <$> [(minBound :: ServiceTag) ..])
      }

--------------------------------------------------------------------------------
-- Bounded ServiceTag Queries

-- | Bounded logical disjunction of 'm' to 'n' 'QueryAllTags'.
newtype QueryAnyTags (m :: Nat) (n :: Nat) = QueryAnyTags
  {queryAnyTagsRange :: Range m n (Set (QueryAllTags m n))}
  deriving stock (Eq, Show, Ord)

instance (m <= n) => S.ToParamSchema (QueryAnyTags m n) where
  toParamSchema _ =
    mempty
      { S._schemaType = Just S.OpenApiString,
        S._schemaEnum = Just (toJSON <$> [(minBound :: ServiceTag) ..])
      }

instance (KnownNat n, KnownNat m, m <= n) => ToSchema (QueryAnyTags m n) where
  schema =
    let sch :: ValueSchema NamedSwaggerDoc (Range m n (Set (QueryAllTags m n)))
        sch = fromRange .= rangedSchema (named "QueryAnyTags" $ set schema)
     in queryAnyTagsRange .= (QueryAnyTags <$> sch)

instance (KnownNat m, KnownNat n, m <= n) => Arbitrary (QueryAnyTags m n) where
  arbitrary = QueryAnyTags <$> arbitrary

queryAnyTags :: (KnownNat m, KnownNat n, m <= n) => MatchAny -> Maybe (QueryAnyTags m n)
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
instance (KnownNat n, KnownNat m, m <= n) => FromByteString (QueryAnyTags m n) where
  parser = do
    bs <- C8.split ',' <$> parser
    ts <- mapM (either fail pure . runParser parser) bs
    rs <- either fail pure (Range.checkedEither (Set.fromList ts))
    pure $! QueryAnyTags rs

runPartial :: (IsString i) => Bool -> IResult i b -> Either Text b
runPartial alreadyRun result = case result of
  Fail _ _ e -> Left $ Text.pack e
  Partial f ->
    if alreadyRun
      then Left "A partial parse returned another partial parse."
      else runPartial True $ f ""
  Done _ r -> pure r

instance (KnownNat n, KnownNat m, m <= n) => FromHttpApiData (QueryAnyTags m n) where
  parseUrlPiece t = do
    txt <- parseUrlPiece t
    runPartial False $ parse parser $ Text.encodeUtf8 txt

-- | Bounded logical conjunction of 'm' to 'n' 'ServiceTag's to match.
newtype QueryAllTags (m :: Nat) (n :: Nat) = QueryAllTags
  {queryAllTagsRange :: Range m n (Set ServiceTag)}
  deriving stock (Eq, Show, Ord)

instance (KnownNat n, KnownNat m, m <= n) => ToSchema (QueryAllTags m n) where
  schema =
    let sch :: ValueSchema NamedSwaggerDoc (Range m n (Set ServiceTag))
        sch = fromRange .= rangedSchema (named "QueryAllTags" $ set schema)
     in queryAllTagsRange .= (QueryAllTags <$> sch)

instance (KnownNat m, KnownNat n, m <= n) => Arbitrary (QueryAllTags m n) where
  arbitrary = QueryAllTags <$> arbitrary

queryAllTags :: (KnownNat m, KnownNat n, m <= n) => MatchAll -> Maybe (QueryAllTags m n)
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
instance (KnownNat m, KnownNat n, m <= n) => FromByteString (QueryAllTags m n) where
  parser = do
    bs <- C8.split '.' <$> parser
    ts <- mapM (either fail pure . runParser parser) bs
    rs <- either fail pure (Range.checkedEither (Set.fromList ts))
    pure $! QueryAllTags rs

instance (KnownNat n, KnownNat m, m <= n) => FromHttpApiData (QueryAllTags m n) where
  parseUrlPiece t = do
    txt <- parseUrlPiece t
    runPartial False $ parse parser $ Text.encodeUtf8 txt

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

matchAll :: MatchAll -> MatchAny
matchAll = MatchAny . Set.singleton

match1 :: ServiceTag -> MatchAny
match1 = matchAll . match

match :: ServiceTag -> MatchAll
match = MatchAll . Set.singleton
