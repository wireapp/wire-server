{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Provider.Tag where

import Imports
import Data.Aeson
import Data.ByteString.Conversion

import qualified Data.Set           as Set
import qualified Data.Text.Encoding as Text

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
        "audio"         -> pure AudioTag
        "books"         -> pure BooksTag
        "business"      -> pure BusinessTag
        "design"        -> pure DesignTag
        "education"     -> pure EducationTag
        "entertainment" -> pure EntertainmentTag
        "finance"       -> pure FinanceTag
        "fitness"       -> pure FitnessTag
        "food-drink"    -> pure FoodDrinkTag
        "games"         -> pure GamesTag
        "graphics"      -> pure GraphicsTag
        "health"        -> pure HealthTag
        "integration"   -> pure IntegrationTag
        "lifestyle"     -> pure LifestyleTag
        "media"         -> pure MediaTag
        "medical"       -> pure MedicalTag
        "movies"        -> pure MoviesTag
        "music"         -> pure MusicTag
        "news"          -> pure NewsTag
        "photography"   -> pure PhotographyTag
        "poll"          -> pure PollTag
        "productivity"  -> pure ProductivityTag
        "quiz"          -> pure QuizTag
        "rating"        -> pure RatingTag
        "shopping"      -> pure ShoppingTag
        "social"        -> pure SocialTag
        "sports"        -> pure SportsTag
        "travel"        -> pure TravelTag
        "tutorial"      -> pure TutorialTag
        "video"         -> pure VideoTag
        "weather"       -> pure WeatherTag
        _               -> fail $ "Invalid tag: " ++ show t

instance ToByteString ServiceTag where
    builder AudioTag         = "audio"
    builder BooksTag         = "books"
    builder BusinessTag      = "business"
    builder DesignTag        = "design"
    builder EducationTag     = "education"
    builder EntertainmentTag = "entertainment"
    builder FinanceTag       = "finance"
    builder FitnessTag       = "fitness"
    builder FoodDrinkTag     = "food-drink"
    builder GamesTag         = "games"
    builder GraphicsTag      = "graphics"
    builder HealthTag        = "health"
    builder IntegrationTag   = "integration"
    builder LifestyleTag     = "lifestyle"
    builder MediaTag         = "media"
    builder MedicalTag       = "medical"
    builder MoviesTag        = "movies"
    builder MusicTag         = "music"
    builder NewsTag          = "news"
    builder PhotographyTag   = "photography"
    builder PollTag          = "poll"
    builder ProductivityTag  = "productivity"
    builder QuizTag          = "quiz"
    builder RatingTag        = "rating"
    builder ShoppingTag      = "shopping"
    builder SocialTag        = "social"
    builder SportsTag        = "sports"
    builder TravelTag        = "travel"
    builder TutorialTag      = "tutorial"
    builder VideoTag         = "video"
    builder WeatherTag       = "weather"

instance FromJSON ServiceTag where
    parseJSON = withText "ServiceTag" $
        either fail pure . runParser parser . Text.encodeUtf8

instance ToJSON ServiceTag where
    toJSON = String . Text.decodeUtf8 . toByteString'

--------------------------------------------------------------------------------
-- ServiceTag Matchers

-- | Logical disjunction of 'MatchAllTags' to match.
newtype MatchAny = MatchAny
    { matchAnySet :: Set MatchAll }
    deriving (Eq, Show, Ord)

-- | Logical conjunction of 'ServiceTag's to match.
newtype MatchAll = MatchAll
    { matchAllSet :: Set ServiceTag }
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
