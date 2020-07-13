{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Schema.AuthenticationScheme
  ( AuthenticationScheme (..),
    AuthenticationSchemeEncoding,
    authHttpBasicEncoding,
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.URI.Static
import Web.Scim.Schema.Common

----------------------------------------------------------------------------
-- Types

-- | Possible authentication schemes. The specification defines the values
-- "oauth", "oauth2", "oauthbearertoken", "httpbasic", and "httpdigest".
data AuthenticationScheme
  = AuthOAuth
  | AuthOAuth2
  | AuthOAuthBearerToken
  | AuthHttpBasic
  | AuthHttpDigest
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | The way authentication schemes are expected to be represented in the
-- configuration. Each 'AuthenticationScheme' corresponds to one of such
-- encodings.
data AuthenticationSchemeEncoding = AuthenticationSchemeEncoding
  { -- | The authentication scheme
    typ :: Text,
    -- | The common authentication scheme name, e.g. HTTP Basic
    name :: Text,
    -- | A description of the authentication scheme
    description :: Text,
    -- | An HTTP-addressable URL pointing to the authentication scheme's
    -- specification
    specUri :: Maybe URI,
    -- | An HTTP-addressable URL pointing to the authentication scheme's usage
    -- documentation
    documentationUri :: Maybe URI
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthenticationSchemeEncoding where
  toJSON = genericToJSON serializeOptions

-- NB: "typ" will be converted to "type" thanks to 'serializeOptions'

----------------------------------------------------------------------------
-- Scheme encodings

-- | The description of the 'AuthHttpBasic' scheme.
authHttpBasicEncoding :: AuthenticationSchemeEncoding
authHttpBasicEncoding =
  AuthenticationSchemeEncoding
    { typ = "httpbasic",
      name = "HTTP Basic",
      description = "Authentication via the HTTP Basic standard",
      specUri = Just $ URI [uri|https://tools.ietf.org/html/rfc7617|],
      documentationUri = Just $ URI [uri|https://en.wikipedia.org/wiki/Basic_access_authentication|]
    }
