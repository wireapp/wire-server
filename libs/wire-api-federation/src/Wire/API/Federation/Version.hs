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

$(genSingletons [''Version])

$(promoteOrdInstances [''Version])
