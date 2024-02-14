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
    V2Sym0,
    versionInt,
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
    mostRecentTuple,
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List.NonEmpty qualified as NE
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Imports
import Wire.API.VersionInfo

data Version = V0 | V1 | V2
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)

versionInt :: Version -> Int
versionInt V0 = 0
versionInt V1 = 1
versionInt V2 = 2

intToVersion :: Int -> Maybe Version
intToVersion intV = find (\v -> versionInt v == intV) [minBound ..]

instance ToSchema Version where
  schema =
    enum @Integer "Version" . mconcat $
      [ element 0 V0,
        element 1 V1,
        element 2 V2
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
latestCommonVersion :: VersionRange -> Set Int -> Maybe Version
latestCommonVersion (Set.map versionInt . enumVersionRange -> localVersions) remoteVersions =
  intToVersion =<< Set.lookupMax (Set.intersection localVersions remoteVersions)

mostRecentTuple :: forall a. (a -> Maybe VersionRange) -> NE.NonEmpty a -> Set Int -> Maybe (a, Version)
mostRecentTuple pr (NE.toList -> as) remoteVersions = foldl' combine Nothing as
  where
    combine :: Maybe (a, Version) -> a -> Maybe (a, Version)
    combine greatest a =
      let notifGreatest = pr a >>= flip latestCommonVersion remoteVersions
       in case (greatest, notifGreatest) of
            (Nothing, Nothing) -> Nothing
            (Nothing, Just v) -> Just (a, v)
            (Just (gn, gv), Nothing) -> Just (gn, gv)
            (Just (gn, gv), Just v) ->
              if v > gv
                then Just (a, v)
                else Just (gn, gv)

$(genSingletons [''Version])

$(promoteOrdInstances [''Version])
