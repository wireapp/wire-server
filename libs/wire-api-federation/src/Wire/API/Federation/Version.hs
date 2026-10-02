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
    V3Sym0,
    V4Sym0,
    V5Sym0,
    intToVersion,
    versionInt,
    versionText,
    FederationVersionExp (..),
    developmentVersions,
    expandVersionExp,
    enabledFederationVersions,
    supportsVersionRange,
    supportedVersions,
    VersionInfo (..),
    versionInfoFor,
    federationVersionMiddleware,
    groupIdFedVersion,

    -- * VersionRange
    VersionUpperBound (..),
    VersionRange (..),
    allVersions,
    latestCommonVersion,
    rangeFromVersion,
    rangeUntilVersion,
  )
where

import Control.Lens (makePrisms, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai
import Network.Wai.Utilities.Error
import Network.Wai.Utilities.Response
import Servant.API (ToHttpApiData (..))
import Wire.API.MLS.Group.Serialisation
import Wire.API.VersionInfo qualified as API

data Version = V0 | V1 | V2 | V3 | V4 | V5
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)

instance ToHttpApiData Version where
  toHeader = versionByteString
  toUrlPiece = versionText

versionInt :: Version -> Int
versionInt V0 = 0
versionInt V1 = 1
versionInt V2 = 2
versionInt V3 = 3
versionInt V4 = 4
versionInt V5 = 5

versionText :: Version -> Text
versionText = ("v" <>) . Text.pack . show . versionInt

versionByteString :: Version -> ByteString
versionByteString = ("v" <>) . BS.pack . show . versionInt

intToVersion :: Int -> Maybe Version
intToVersion intV = find (\v -> versionInt v == intV) [minBound .. maxBound]

instance ToSchema Version where
  schema =
    enum @Integer . mconcat $
      [ element 0 V0,
        element 1 V1,
        element 2 V2,
        element 3 V3,
        element 4 V4,
        element 5 V5
      ]

supportedVersions :: Set Version
supportedVersions = Set.fromList [minBound .. maxBound]

developmentVersions :: Set Version
developmentVersions = Set.singleton V5

data FederationVersionExp
  = FederationVersionExpConst Version
  | FederationVersionExpDevelopment
  deriving (Show, Eq, Ord, Generic)

$(makePrisms ''FederationVersionExp)

instance ToSchema FederationVersionExp where
  schema =
    named "FederationVersionExp" $
      tag _FederationVersionExpConst (unnamed schema)
        <> tag
          _FederationVersionExpDevelopment
          (unnamed (enum @Text (element "development" ())))

deriving via Schema FederationVersionExp instance FromJSON FederationVersionExp

deriving via Schema FederationVersionExp instance ToJSON FederationVersionExp

expandVersionExp :: FederationVersionExp -> Set Version
expandVersionExp (FederationVersionExpConst v) = Set.singleton v
expandVersionExp FederationVersionExpDevelopment = developmentVersions

enabledFederationVersions :: Set FederationVersionExp -> Set Version
enabledFederationVersions disabled =
  supportedVersions Set.\\ foldMap expandVersionExp disabled

data VersionInfo = VersionInfo
  { vinfoSupported :: [Int]
  }
  deriving (FromJSON, S.ToSchema) via (Schema VersionInfo)

instance ToJSON VersionInfo where
  toJSON VersionInfo {vinfoSupported} =
    Aeson.object
      [ "supported_versions" Aeson..= vinfoSupported,
        "supported" Aeson..= filter (`elem` [0, 1]) vinfoSupported
      ]

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier (S.schema . S.example ?~ toJSON example) $
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

versionInfoFor :: Set Version -> VersionInfo
versionInfoFor versions = VersionInfo (map versionInt (toList versions))

federationVersionMiddleware :: Set Version -> Middleware
federationVersionMiddleware disabled app req k
  | ["federation", "api-version"] <- pathInfo req,
    Nothing <- lookup API.versionHeader (requestHeaders req) =
      app req k
  | "federation" : _ <- pathInfo req =
      case lookup API.versionHeader (requestHeaders req) of
        Nothing -> allow V0
        Just value -> case parseVersionHeader value of
          Nothing -> unsupported "invalid federation API version"
          Just version -> allow version
  | otherwise = app req k
  where
    allow version
      | version `elem` disabled = unsupported ("federation API version " <> versionText version)
      | otherwise = app req k
    parseVersionHeader :: ByteString -> Maybe Version
    parseVersionHeader value = readMaybe (BS.unpack value) >>= intToVersion

    unsupported :: Text -> IO ResponseReceived
    unsupported message =
      k . errorRs . mkError HTTP.status404 "unsupported-version" $ LText.fromStrict message

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

instance ToSchema VersionRange where
  schema =
    object $
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

supportsVersionRange :: VersionRange -> VersionInfo -> Bool
supportsVersionRange range = any (maybe False (inVersionRange range) . intToVersion) . (.vinfoSupported)

rangeFromVersion :: Version -> VersionRange
rangeFromVersion v = VersionRange v Unbounded

rangeUntilVersion :: Version -> VersionRange
rangeUntilVersion v = VersionRange minBound (VersionUpperBound v)

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

-- | First federation version supporting the given group ID version.
groupIdFedVersion :: GroupIdVersion -> Version
groupIdFedVersion GroupIdVersion1 = V0
groupIdFedVersion GroupIdVersion2 = V3

$(genSingletons [''Version])

$(promoteOrdInstances [''Version])
