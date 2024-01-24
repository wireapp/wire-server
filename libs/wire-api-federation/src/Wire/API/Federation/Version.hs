{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Wire.API.Federation.Version
  ( Version (..),
    V0Sym0,
    V1Sym0,
    versionInt,
    supportedVersions,
    VersionInfo (..),
    versionInfo,
    VersionRange (..),
    latestCommonVersion,
  )
where

import Control.Lens (makePrisms, (?~))
import Control.Lens.Tuple (_1)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Imports
import Wire.API.VersionInfo

data Version = V0 | V1
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)

versionInt :: Version -> Int
versionInt V0 = 0
versionInt V1 = 1

intToVersion :: Int -> Maybe Version
intToVersion intV = find (\v -> versionInt v == intV) [minBound .. maxBound]

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1
      ]

supportedVersions :: Set Version
supportedVersions = Set.fromList [minBound .. maxBound]

data VersionInfo = VersionInfo
  { vinfoSupported :: [Version]
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        <$> vinfoSupported .= vinfoObjectSchema schema
    where
      example :: VersionInfo
      example =
        VersionInfo
          { vinfoSupported = toList supportedVersions
          }

versionInfo :: VersionInfo
versionInfo = VersionInfo (toList supportedVersions)

----------------------------------------------------------------------

data VersionRangeTag
  = VersionRangeTagAll
  | VersionRangeTagFrom
  | VersionRangeTagUntil
  | VersionRangeTagFromUntil
  deriving (Eq, Enum, Bounded)

versionRangeTagSchema :: ValueSchema NamedSwaggerDoc VersionRangeTag
versionRangeTagSchema =
  enum @Text "VersionRange Tag" $
    mconcat
      [ element "all-versions" VersionRangeTagAll,
        element "from" VersionRangeTagFrom,
        element "until" VersionRangeTagUntil,
        element "from-until" VersionRangeTagFromUntil
      ]

versionPairSchema :: ValueSchema NamedSwaggerDoc (Version, Version)
versionPairSchema =
  object "VersionPair" $
    (,)
      <$> fst .= field "from" schema
      <*> snd .= field "until" schema

data VersionRange
  = AllVersions
  | -- | The version in the argument represent an inclusive bound.
    FromVersion Version
  | -- | The version in the argument represent an exclusive bound.
    UntilVersion Version
  | -- | The second argument represents an exclusive upper bound.
    FromUntilVersion Version Version

deriving instance Eq VersionRange

deriving instance Show VersionRange

deriving instance Ord VersionRange

makePrisms ''VersionRange

instance ToSchema VersionRange where
  schema =
    object "VersionRange" $
      fromTagged
        <$> toTagged
          .= bind
            (fst .= field "tag" versionRangeTagSchema)
            (snd .= fieldOver _1 "value" untaggedSchema)
    where
      toTagged :: VersionRange -> (VersionRangeTag, VersionRange)
      toTagged d@AllVersions = (VersionRangeTagAll, d)
      toTagged d@(FromVersion _) = (VersionRangeTagFrom, d)
      toTagged d@(UntilVersion _) = (VersionRangeTagUntil, d)
      toTagged d@(FromUntilVersion _ _) = (VersionRangeTagFromUntil, d)

      fromTagged :: (VersionRangeTag, VersionRange) -> VersionRange
      fromTagged = snd

      untaggedSchema = dispatch $ \case
        VersionRangeTagAll -> tag _AllVersions null_
        VersionRangeTagFrom -> tag _FromVersion (unnamed schema)
        VersionRangeTagUntil -> tag _UntilVersion (unnamed schema)
        VersionRangeTagFromUntil -> tag _FromUntilVersion $ unnamed versionPairSchema

deriving via Schema VersionRange instance ToJSON VersionRange

deriving via Schema VersionRange instance FromJSON VersionRange

-- | Compute the lower and upper boundary of a version range. The first
-- component of the pair is the lower boundary, while the second component is
-- the upper boundary. The upper boundary is inclusive.
versionRangeToBoundaries :: VersionRange -> (Version, Version)
versionRangeToBoundaries AllVersions = (minBound @Version, maxBound @Version)
versionRangeToBoundaries (FromVersion fv) = (fv, maxBound @Version)
versionRangeToBoundaries (UntilVersion uv) = (minBound @Version, pred uv)
versionRangeToBoundaries (FromUntilVersion fv uv) = (fv, pred uv)

-- | Checks if a version is within a given version range.
inVersionRange :: Version -> VersionRange -> Bool
inVersionRange v vr =
  let (lo, hi) = versionRangeToBoundaries vr
   in lo <= v && v < hi

-- | For a version range of a local backend and for a set of versions that a
-- remote backend supports, compute the newest version supported by both. The
-- remote versions are given as integers as the range of versions supported by
-- the remote backend can include a version unknown to the local backend. If
-- there is no version in common, the return value is 'Nothing'.
latestCommonVersion :: VersionRange -> Set Int -> Maybe Version
latestCommonVersion localVersions remoteVersions =
  foldl' f Nothing (Set.map inRange remoteVersions)
  where
    inRange :: Int -> Maybe Version
    inRange i = do
      v <- intToVersion i
      guard (v `inVersionRange` localVersions) $> v

    f :: Maybe Version -> Maybe Version -> Maybe Version
    f Nothing mv = mv
    f (Just m) (Just v) = Just $ m `max` v
    f v Nothing = v

$(genSingletons [''Version])

$(promoteOrdInstances [''Version])
