{-# LANGUAGE QuasiQuotes #-}

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
data AuthenticationSchemeEncoding
  = AuthenticationSchemeEncoding
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
