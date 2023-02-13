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
    VersionInfo (..),
    versionSwagger,
    versionHeader,
    VersionHeader,

    -- * Version
    Version (..),
    VersionNumber (..),
    supportedVersions,
    developmentVersions,

    -- * Servant combinators
    Until,
    From,
  )
where

import Control.Error (note)
import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Data.ByteString.Conversion (ToByteString (builder))
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import Data.Text as Text
import Data.Text.Encoding as Text
import Imports
import Servant
import Servant.Swagger
import Wire.API.Routes.Named
import Wire.API.VersionInfo
import Wire.Arbitrary (Arbitrary, GenericUniform (GenericUniform))

-- | Version of the public API.  Serializes to `"v<n>"`.  See 'VersionNumber' below for one
-- that serializes to `<n>`.  See `/libs/wire-api/test/unit/Test/Wire/API/Routes/Version.hs`
-- for serialization rules.
data Version = V0 | V1 | V2 | V3
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)
  deriving (Arbitrary) via (GenericUniform Version)

-- | Manual enumeration of version strings.
--
-- If you want to implement this using `{to,from}Enum`, continue reading the haddocs for
-- 'versionInt' below.  :-)
versionString :: IsString a => Version -> a
versionString V0 = "v0"
versionString V1 = "v1"
versionString V2 = "v2"
versionString V3 = "v3"

-- | Manual enumeration of version integrals.
--
-- We don't do anything fancy with `{to,from}Enum`
-- because we'll eventually break the invariant that there is a `V<n>` for every `<n>` once we
-- start to deprecate old versions (we may even find a reason to discontinue `V13` but keep
-- supporting `V12`).
versionInt :: Integral i => Version -> i
versionInt V0 = 0
versionInt V1 = 1
versionInt V2 = 2
versionInt V3 = 3

instance ToSchema Version where
  schema = enum @Text "Version" . mconcat $ (\v -> element (versionString v) v) <$> [minBound ..]

instance FromHttpApiData Version where
  parseQueryParam v = note ("Unknown version: " <> v) $
    getAlt $
      flip foldMap [minBound ..] $ \s ->
        guard (versionString s == v) $> s

instance ToHttpApiData Version where
  toHeader = versionString
  toUrlPiece = versionString

instance ToByteString Version where
  builder = versionString

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
    enum @Integer "Version" . mconcat $ (\v -> element (versionInt v) (VersionNumber v)) <$> [minBound ..]

instance FromHttpApiData VersionNumber where
  parseHeader = first Text.pack . Aeson.eitherDecode . LBS.fromStrict
  parseUrlPiece = parseHeader . Text.encodeUtf8

instance ToHttpApiData VersionNumber where
  toHeader = LBS.toStrict . Aeson.encode
  toUrlPiece = Text.decodeUtf8 . toHeader

instance ToByteString VersionNumber where
  builder = toEncodedUrlPiece

supportedVersions :: [Version]
supportedVersions = [minBound .. maxBound]

developmentVersions :: [Version]
developmentVersions = [V3]

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
        <$> vinfoSupported .= vinfoObjectSchema schema
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

versionSwagger :: S.Swagger
versionSwagger = toSwagger (Proxy @VersionAPI)

$(genSingletons [''Version])
