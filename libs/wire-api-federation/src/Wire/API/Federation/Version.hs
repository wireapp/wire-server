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
  ( -- * Version, VersionInfo
    Version (..),
    V0Sym0,
    V1Sym0,
    intToVersion,
    versionInt,
    versionText,
    supportedVersions,
    VersionInfo (..),
    versionInfo,

    -- * VersionRange
    VersionUpperBound (..),
    VersionRange (..),
    fromVersion,
    toVersionExcl,
    allVersions,
    latestCommonVersion,
    rangeFromVersion,
    rangeUntilVersion,
    enumVersionRange,
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Data.Text qualified as Text
import Imports

data Version = V0 | V1
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)

versionInt :: Version -> Int
versionInt V0 = 0
versionInt V1 = 1

versionText :: Version -> Text
versionText = ("v" <>) . Text.pack . show . versionInt

intToVersion :: Int -> Maybe Version
intToVersion intV = find (\v -> versionInt v == intV) [minBound ..]

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1
      ]

supportedVersions :: Set Version
supportedVersions = Set.fromList [minBound .. maxBound]

data VersionInfo = VersionInfo
  { vinfoSupported :: [Int]
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        -- if the supported_versions field does not exist, assume an old backend
        -- that only supports V0
        <$> vinfoSupported
          .= fmap
            (fromMaybe [0])
            (optField "supported_versions" (array schema))
        -- legacy field to support older versions of the backend with broken
        -- version negotiation
        <* const [0 :: Int, 1] .= field "supported" (array schema)
    where
      example :: VersionInfo
      example =
        VersionInfo
          { vinfoSupported = map versionInt (toList supportedVersions)
          }

versionInfo :: VersionInfo
versionInfo = VersionInfo (map versionInt (toList supportedVersions))

----------------------------------------------------------------------

-- | The upper bound of a version range.
--
-- The order of constructors here makes the 'Unbounded' value maximum in the
-- generated lexicographic ordering.
data VersionUpperBound = VersionUpperBound Version | Unbounded
  deriving (Eq, Ord, Show)

versionFromUpperBound :: VersionUpperBound -> Maybe Version
versionFromUpperBound (VersionUpperBound v) = Just v
versionFromUpperBound Unbounded = Nothing

versionToUpperBound :: Maybe Version -> VersionUpperBound
versionToUpperBound (Just v) = VersionUpperBound v
versionToUpperBound Nothing = Unbounded

data VersionRange = VersionRange
  { _fromVersion :: Version,
    _toVersionExcl :: VersionUpperBound
  }

deriving instance Eq VersionRange

deriving instance Show VersionRange

deriving instance Ord VersionRange

makeLenses ''VersionRange

instance ToSchema VersionRange where
  schema =
    object "VersionRange" $
      VersionRange
        <$> _fromVersion .= field "from" schema
        <*> (versionFromUpperBound . _toVersionExcl)
          .= maybe_ (versionToUpperBound <$> optFieldWithDocModifier "until_excl" desc schema)
    where
      desc = description ?~ "exlusive upper version bound"

deriving via Schema VersionRange instance ToJSON VersionRange

deriving via Schema VersionRange instance FromJSON VersionRange

allVersions :: VersionRange
allVersions = VersionRange minBound Unbounded

-- | The semigroup instance of VersionRange is intersection.
instance Semigroup VersionRange where
  VersionRange from1 to1 <> VersionRange from2 to2 =
    VersionRange (max from1 from2) (min to1 to2)

inVersionRange :: VersionRange -> Version -> Bool
inVersionRange (VersionRange a b) v =
  v >= a && VersionUpperBound v < b

rangeFromVersion :: Version -> VersionRange
rangeFromVersion v = VersionRange v Unbounded

rangeUntilVersion :: Version -> VersionRange
rangeUntilVersion v = VersionRange minBound (VersionUpperBound v)

enumVersionRange :: VersionRange -> Set Version
enumVersionRange =
  Set.fromList . \case
    VersionRange l Unbounded -> [l ..]
    VersionRange l (VersionUpperBound u) -> init [l .. u]

-- | For a version range of a local backend and for a set of versions that a
-- remote backend supports, compute the newest version supported by both. The
-- remote versions are given as integers as the range of versions supported by
-- the remote backend can include a version unknown to the local backend. If
-- there is no version in common, the return value is 'Nothing'.
latestCommonVersion :: (Foldable f) => VersionRange -> f Int -> Maybe Version
latestCommonVersion localVersions =
  safeMaximum
    . filter (inVersionRange localVersions)
    . mapMaybe intToVersion
    . toList

safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum as = Just (maximum as)

$(genSingletons [''Version])

$(promoteOrdInstances [''Version])
