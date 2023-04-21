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
import qualified Data.Binary.Builder as Builder
import Data.ByteString.Conversion (ToByteString (builder), toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import Data.Text as Text
import Data.Text.Encoding as Text
import GHC.Generics
import Imports
import Servant
import Servant.Swagger
import Wire.API.Routes.Named
import Wire.API.VersionInfo
import Wire.Arbitrary (Arbitrary, GenericUniform (GenericUniform))

class
  ( Eq v,
    Ord v,
    Enum v,
    Bounded v,
    Show v,
    Generic v,
    ToSchema v,
    ToJSON v,
    FromJSON v,
    ToHttpApiData v,
    FromHttpApiData v,
    ToByteString v,
    Arbitrary v
  ) =>
  IsVersion v
  where
  -- | Manual enumeration of version integrals (the `<n>` in the constructor `V<n>`).
  --
  -- This is not the same as 'fromEnum': we will remove unsupported versions in the future,
  -- which will cause `<n>` and `fromEnum V<n>` to diverge.  `Enum` should not be understood as
  -- a bijection between meaningful integers and versions, but merely as a convenient way to say
  -- `allVersions = [minBound..]`.
  versionInt :: Integral i => v -> i

  supportedVersions :: [v]
  developmentVersions :: [v]

  versionText :: v -> Text
  versionByteString :: v -> ByteString

-- | Version of the public API.  Serializes to `"v<n>"`.  See 'VersionNumber' below for one
-- that serializes to `<n>`.  See `/libs/wire-api/test/unit/Test/Wire/API/Routes/Version.hs`
-- for serialization rules.
--
-- If you add or remove versions from this type, make sure 'versionInt', 'supportedVersions',
-- and 'developmentVersions' stay in sync; everything else here should keep working without
-- change.  See also documentation in the *docs* directory.
-- https://docs.wire.com/developer/developer/api-versioning.html#version-bump-checklist
data Version = V0 | V1 | V2 | V3 | V4
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema Version)
  deriving (Arbitrary) via (GenericUniform Version)

instance IsVersion Version where
  versionInt V0 = 0
  versionInt V1 = 1
  versionInt V2 = 2
  versionInt V3 = 3
  versionInt V4 = 4

  supportedVersions = [minBound .. V4]
  developmentVersions = [V4]

  versionText = ("v" <>) . toUrlPiece . versionInt @Version @Int
  versionByteString = ("v" <>) . toByteString' . versionInt @Version @Int

instance ToSchema Version where
  schema = enum @Text "Version" . mconcat $ (\v -> element (versionText v) v) <$> [minBound ..]

instance FromHttpApiData Version where
  parseQueryParam v = note ("Unknown version: " <> v) $
    getAlt $
      flip foldMap [minBound ..] $ \s ->
        guard (versionText s == v) $> s

instance ToHttpApiData Version where
  toHeader = versionByteString
  toUrlPiece = versionText

instance ToByteString Version where
  builder = Builder.fromByteString . versionByteString

-- | Wrapper around 'Version' that serializes to integers `<n>`, as needed in
-- eg. `VersionInfo`.  See `/libs/wire-api/test/unit/Test/Wire/API/Routes/Version.hs` for
-- serialization rules.
newtype VersionNumber v = VersionNumber {fromVersionNumber :: v}
  deriving (Eq, Ord, Show)
  deriving newtype (Bounded, Enum)
  deriving (FromJSON, ToJSON) via (Schema (VersionNumber v))

--  deriving (Arbitrary) via (GenericUniform v)

-- Can't derive this because of v's class context.
instance (Generic v, IsVersion v) => Generic (VersionNumber v) where
  type Rep (VersionNumber v) = Rep v
  from (VersionNumber v) = from v -- (T x) = from (x, undefined)
  to r = VersionNumber (to r) -- rep = T (fst (to rep))

{-
  instance GHC.Generics.Generic Wire.API.Routes.Version.Version where
    GHC.Generics.from x_aGix
      = GHC.Generics.M1
          (case x_aGix of
             Wire.API.Routes.Version.V0
               -> GHC.Generics.L1
                    (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1))
             Wire.API.Routes.Version.V1
               -> GHC.Generics.L1
                    (GHC.Generics.R1 (GHC.Generics.M1 GHC.Generics.U1))
             Wire.API.Routes.Version.V2
               -> GHC.Generics.R1
                    (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1))
             Wire.API.Routes.Version.V3
               -> GHC.Generics.R1
                    (GHC.Generics.R1
                       (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1)))
             Wire.API.Routes.Version.V4
               -> GHC.Generics.R1
                    (GHC.Generics.R1
                       (GHC.Generics.R1 (GHC.Generics.M1 GHC.Generics.U1))))
    GHC.Generics.to (GHC.Generics.M1 x_aGiy)
      = case x_aGiy of
          (GHC.Generics.L1 (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1)))
            -> Wire.API.Routes.Version.V0
          (GHC.Generics.L1 (GHC.Generics.R1 (GHC.Generics.M1 GHC.Generics.U1)))
            -> Wire.API.Routes.Version.V1
          (GHC.Generics.R1 (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1)))
            -> Wire.API.Routes.Version.V2
          (GHC.Generics.R1 (GHC.Generics.R1 (GHC.Generics.L1 (GHC.Generics.M1 GHC.Generics.U1))))
            -> Wire.API.Routes.Version.V3
          (GHC.Generics.R1 (GHC.Generics.R1 (GHC.Generics.R1 (GHC.Generics.M1 GHC.Generics.U1))))
            -> Wire.API.Routes.Version.V4
-}

instance IsVersion v => ToSchema (VersionNumber v) where
  schema =
    enum @Integer "VersionNumber" . mconcat $ (\v -> element (versionInt v) (VersionNumber v)) <$> [minBound ..]

instance IsVersion v => FromHttpApiData (VersionNumber v) where
  parseHeader = first Text.pack . Aeson.eitherDecode . LBS.fromStrict
  parseUrlPiece = parseHeader . Text.encodeUtf8

instance IsVersion v => ToHttpApiData (VersionNumber v) where
  toHeader = LBS.toStrict . Aeson.encode
  toUrlPiece = Text.decodeUtf8 . toHeader

instance IsVersion v => ToByteString (VersionNumber v) where
  builder = toEncodedUrlPiece

-- | Information related to the public API version.
--
-- This record also contains whether federation is enabled and the federation
-- domain. Clients should fetch this information early when connecting to a
-- backend, in order to decide how to form request paths, and how to deal with
-- federated backends and qualified user IDs.
data VersionInfo v = VersionInfo
  { vinfoSupported :: [VersionNumber v],
    vinfoDevelopment :: [VersionNumber v],
    vinfoFederation :: Bool,
    vinfoDomain :: Domain
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema (VersionInfo v))

instance IsVersion v => ToSchema (VersionInfo v) where
  schema =
    objectWithDocModifier "VersionInfo" (S.schema . S.example ?~ toJSON example) $
      VersionInfo
        <$> vinfoSupported .= vinfoObjectSchema schema
        <*> vinfoDevelopment .= field "development" (array schema)
        <*> vinfoFederation .= field "federation" schema
        <*> vinfoDomain .= field "domain" schema
    where
      example :: VersionInfo v
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
        :> Get '[JSON] (VersionInfo Version)
    )

{-
  :<|> Named
    "get-backend-version"
    ( "api-version-backend"
        :> Get '[JSON] VersionInfo
    )
-}

versionSwagger :: S.Swagger
versionSwagger = toSwagger (Proxy @VersionAPI)

$(genSingletons [''Version])
