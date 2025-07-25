{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Routes.Version
  ( -- * API version endpoint
    VersionAPI,
    VersionAPITag,
    VersionInfo (..),
    versionHeader,
    VersionHeader,

    -- * Version
    Version (..),
    versionInt,
    versionText,
    versionedName,
    VersionNumber (..),
    VersionExp (..),
    supportedVersions,
    isDevelopmentVersion,
    developmentVersions,
    expandVersionExp,
    maxAvailableVersion,

    -- * Servant combinators
    Until,
    From,

    -- * Swagger
    module Wire.API.Routes.SpecialiseToVersion,
  )
where

import Control.Error (note)
import Control.Lens (makePrisms, (?~))
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Conversion (ToByteString (builder), toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.Domain
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Data.Text qualified as Text
import Data.Text.Encoding as Text
import Imports hiding ((\\))
import Servant
import Wire.API.Routes.Named hiding (unnamed)
import Wire.API.Routes.SpecialiseToVersion
import Wire.API.VersionInfo
import Wire.Arbitrary (Arbitrary, GenericUniform (GenericUniform))

-- | Version of the public API.  Serializes to `"v<n>"`.  See 'VersionNumber' below for one
-- that serializes to `<n>`.  See `/libs/wire-api/test/unit/Test/Wire/API/Routes/Version.hs`
-- for serialization rules.
--
-- If you add or remove versions from this type, make sure 'versionInt', 'supportedVersions',
-- and 'developmentVersions' stay in sync; everything else here should keep working without
-- change.  See also documentation in the *docs* directory.
-- https://docs.wire.com/developer/developer/api-versioning.html#version-bump-checklist
data Version = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | V11
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)
  deriving (Arbitrary) via (GenericUniform Version)

instance RenderableSymbol V1 where renderSymbol = "V1"

instance RenderableSymbol V2 where renderSymbol = "V2"

instance RenderableSymbol V3 where renderSymbol = "V3"

instance RenderableSymbol V4 where renderSymbol = "V4"

instance RenderableSymbol V5 where renderSymbol = "V5"

instance RenderableSymbol V6 where renderSymbol = "V6"

instance RenderableSymbol V7 where renderSymbol = "V7"

instance RenderableSymbol V8 where renderSymbol = "V8"

instance RenderableSymbol V9 where renderSymbol = "V9"

instance RenderableSymbol V10 where renderSymbol = "V10"

instance RenderableSymbol V11 where renderSymbol = "V11"

-- | Manual enumeration of version integrals (the `<n>` in the constructor `V<n>`).
--
-- This is not the same as 'fromEnum': we will remove unsupported versions in the future,
-- which will cause `<n>` and `fromEnum V<n>` to diverge.  `Enum` should not be understood as
-- a bijection between meaningful integers and versions, but merely as a convenient way to say
-- `allVersions = [minBound..]`.
versionInt :: (Integral i) => Version -> i
versionInt V0 = 0
versionInt V1 = 1
versionInt V2 = 2
versionInt V3 = 3
versionInt V4 = 4
versionInt V5 = 5
versionInt V6 = 6
versionInt V7 = 7
versionInt V8 = 8
versionInt V9 = 9
versionInt V10 = 10
versionInt V11 = 11

supportedVersions :: [Version]
supportedVersions = [minBound .. maxBound]

maxAvailableVersion :: Set Version -> Maybe Version
maxAvailableVersion disabled = Set.lookupMax $ Set.fromList supportedVersions \\ disabled

versionedName :: Maybe Version -> Text -> Text
versionedName Nothing unversionedName = unversionedName
versionedName (Just v) unversionedName = unversionedName <> Text.pack (show v)

----------------------------------------------------------------------

versionText :: Version -> Text
versionText = ("v" <>) . toUrlPiece . versionInt @Int

versionByteString :: Version -> ByteString
versionByteString = ("v" <>) . toByteString' . versionInt @Int

instance ToSchema Version where
  schema = enum @Text "Version" . mconcat $ (\v -> element (versionText v) v) <$> [minBound ..]

instance FromHttpApiData Version where
  parseQueryParam v = note ("Unknown version: " <> v) $
    getAlt $
      flip foldMap [minBound ..] $
        \s ->
          guard (versionText s == v) $> s

instance ToHttpApiData Version where
  toHeader = versionByteString
  toUrlPiece = versionText

instance ToByteString Version where
  builder = Builder.fromByteString . versionByteString

-- | Wrapper around 'Version' that serializes to integers `<n>`, as needed in
-- eg. `VersionInfo`.  See `/libs/wire-api/test/unit/Test/Wire/API/Routes/Version.hs` for
-- serialization rules.
newtype VersionNumber = VersionNumber {fromVersionNumber :: Version}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Bounded, Enum)
  deriving (FromJSON, ToJSON) via (Schema VersionNumber)
  deriving (Arbitrary) via (GenericUniform Version)

instance ToSchema VersionNumber where
  schema =
    enum @Integer "VersionNumber" . mconcat $ (\v -> element (versionInt v) (VersionNumber v)) <$> [minBound ..]

instance FromHttpApiData VersionNumber where
  parseHeader = first Text.pack . Aeson.eitherDecode . LBS.fromStrict
  parseUrlPiece = parseHeader . Text.encodeUtf8

instance ToHttpApiData VersionNumber where
  toHeader = LBS.toStrict . Aeson.encode
  toUrlPiece = Text.decodeUtf8 . toHeader

instance ToByteString VersionNumber where
  builder = toEncodedUrlPiece

-- | Information related to the public API version.
--
-- This record also contains whether federation is enabled and the federation
-- domain. Clients should fetch this information early when connecting to a
-- backend, in order to decide how to form request paths, and how to deal with
-- federated backends and qualified user IDs.
data VersionInfo = VersionInfo
  { vinfoSupported :: [VersionNumber],
    vinfoDevelopment :: [VersionNumber],
    vinfoFederation :: Bool,
    vinfoDomain :: Domain
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VersionInfo)

instance ToSchema VersionInfo where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        <$> vinfoSupported
          .= vinfoObjectSchema schema
        <*> vinfoDevelopment .= field "development" (array schema)
        <*> vinfoFederation .= field "federation" schema
        <*> vinfoDomain .= field "domain" schema
    where
      example :: VersionInfo
      example =
        VersionInfo
          { vinfoSupported = VersionNumber <$> supportedVersions,
            vinfoDevelopment = [maxBound],
            vinfoFederation = False,
            vinfoDomain = Domain "example.com"
          }

type VersionAPI =
  Named
    "get-version"
    ( "api-version"
        :> Get '[JSON] VersionInfo
    )

data VersionAPITag

-- Development versions

$(genSingletons [''Version])

isDevelopmentVersion :: Version -> Bool
isDevelopmentVersion V0 = False
isDevelopmentVersion V1 = False
isDevelopmentVersion V2 = False
isDevelopmentVersion V3 = False
isDevelopmentVersion V4 = False
isDevelopmentVersion V5 = False
isDevelopmentVersion V6 = False
isDevelopmentVersion V7 = False
isDevelopmentVersion V8 = False
isDevelopmentVersion V9 = False
isDevelopmentVersion V10 = False
isDevelopmentVersion V11 = True

developmentVersions :: [Version]
developmentVersions = filter isDevelopmentVersion supportedVersions

-- Version keywords

-- | A version "expression" which can be used when disabling versions in a
-- configuration file.
data VersionExp
  = -- | A fixed version.
    VersionExpConst Version
  | -- | All development versions.
    VersionExpDevelopment
  deriving (Show, Eq, Ord, Generic)

$(makePrisms ''VersionExp)

instance ToSchema VersionExp where
  schema =
    named "VersionExp" $
      tag _VersionExpConst (unnamed schema)
        <> tag
          _VersionExpDevelopment
          ( unnamed
              ( enum @Text "VersionExpDevelopment" (element "development" ())
              )
          )

deriving via Schema VersionExp instance (FromJSON VersionExp)

deriving via Schema VersionExp instance (ToJSON VersionExp)

-- | Expand a version expression into a set of versions.
expandVersionExp :: VersionExp -> Set Version
expandVersionExp (VersionExpConst v) = Set.singleton v
expandVersionExp VersionExpDevelopment = Set.fromList developmentVersions

$(promoteOrdInstances [''Version])
