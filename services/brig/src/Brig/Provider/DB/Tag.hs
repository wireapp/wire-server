{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Service tags are intepreted as bitmasks. Any combination of
-- tags yields a unique bitmask (i.e. number) that can be used as
-- a key to efficiently find all services having multiple tags set
-- (i.e. by logical conjunction of multiple tags).
--
-- In practice, the number of tags that can be combined per service
-- (and thus per query) is, and should be, small (currently 3), since
-- the possible combinations are obtained by taking the power set of
-- the bitmasks (without the empty set), which grows exponentially.
module Brig.Provider.DB.Tag
    ( Bucket
    , defBucket
    , tagToInt
    , intToTag
    , foldTags
    , unfoldTags
    , unfoldTagsInto
    , diffTags
    , nonEmptyTags
    ) where

import Imports
import Brig.Types.Provider.Tag
import Cassandra (Cql)
import Data.Bits
import Data.List (foldl')
import Data.Range

import qualified Data.Set as Set

newtype Bucket = Bucket Int32
    deriving (Cql, Show)

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
    []         -> []
    [t]        -> [t]
    ts@[t,u]   -> (t .|. u) : ts
    ts@[t,u,v] -> (t .|. u) : (t .|. v) : (u .|. v) : (t .|. u .|. v) : ts
    _          -> error "Brig.Provider.DB.Tag: unfoldTags: Too many tags."

unfoldTagsInto :: Range 1 3 (Set ServiceTag) -> [Int64] -> [Int64]
unfoldTagsInto xs ys =
    let xs' = unfoldTags (rcast xs)
    in xs' ++ concatMap (\x -> map (.|. x) ys) xs'

diffTags :: Range 0 3 (Set ServiceTag)
         -> Range 0 3 (Set ServiceTag)
         -> Range 0 3 (Set ServiceTag)
diffTags a b = unsafeRange $ Set.difference (fromRange a) (fromRange b)

nonEmptyTags :: Range m 3 (Set ServiceTag) -> Maybe (Range 1 3 (Set ServiceTag))
nonEmptyTags r
    | Set.null (fromRange r) = Nothing
    | otherwise              = Just (unsafeRange (fromRange r))

tagToInt :: ServiceTag -> Int64
tagToInt AudioTag         = 0b1
tagToInt BooksTag         = 0b10
tagToInt BusinessTag      = 0b100
tagToInt DesignTag        = 0b1000
tagToInt EducationTag     = 0b10000
tagToInt EntertainmentTag = 0b100000
tagToInt FinanceTag       = 0b1000000
tagToInt FitnessTag       = 0b10000000
tagToInt FoodDrinkTag     = 0b100000000
tagToInt GamesTag         = 0b1000000000
tagToInt GraphicsTag      = 0b10000000000
tagToInt HealthTag        = 0b100000000000
tagToInt IntegrationTag   = 0b1000000000000
tagToInt LifestyleTag     = 0b10000000000000
tagToInt MediaTag         = 0b100000000000000
tagToInt MedicalTag       = 0b1000000000000000
tagToInt MoviesTag        = 0b10000000000000000
tagToInt MusicTag         = 0b100000000000000000
tagToInt NewsTag          = 0b1000000000000000000
tagToInt PhotographyTag   = 0b10000000000000000000
tagToInt PollTag          = 0b100000000000000000000
tagToInt ProductivityTag  = 0b1000000000000000000000
tagToInt QuizTag          = 0b10000000000000000000000
tagToInt RatingTag        = 0b100000000000000000000000
tagToInt ShoppingTag      = 0b1000000000000000000000000
tagToInt SocialTag        = 0b10000000000000000000000000
tagToInt SportsTag        = 0b100000000000000000000000000
tagToInt TravelTag        = 0b1000000000000000000000000000
tagToInt TutorialTag      = 0b10000000000000000000000000000
tagToInt VideoTag         = 0b100000000000000000000000000000
tagToInt WeatherTag       = 0b1000000000000000000000000000000

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
