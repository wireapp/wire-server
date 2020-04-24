{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | A "default" module for types used in Spar, unless there's a better / more specific place
-- for them.
module Spar.Types where

import Control.Lens (makeLenses)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Conversion
import Data.Id (ScimTokenId, TeamId, UserId)
import Data.Json.Util
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions
import Data.String.Conversions (ST)
import qualified Data.Text as ST
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types (Symbol)
import Imports
import Network.HTTP.Media ((//))
import SAML2.Util (parseURI', renderURI)
import SAML2.WebSSO (Assertion, AuthnRequest, ID, IdPConfig, IdPId, SimpleSetCookie)
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Servant.API as Servant hiding (MkLink, URI (..))
import System.Logger.Extended (LogFormat)
import URI.ByteString
import Util.Options
import Web.Cookie
import Web.HttpApiData

type SetBindCookie = SimpleSetCookie "zbind"

newtype BindCookie = BindCookie {fromBindCookie :: ST}

-- | Extract @zbind@ cookie from HTTP header contents if it exists.
bindCookieFromHeader :: ST -> Maybe BindCookie
bindCookieFromHeader = fmap BindCookie . lookup "zbind" . parseCookiesText . cs

-- (we could rewrite this as @SAML.cookieName SetBindCookie@ if 'cookieName'
-- accepted any @proxy :: Symbol -> *@ rather than just 'Proxy'.)

setBindCookieValue :: HasCallStack => SetBindCookie -> BindCookie
setBindCookieValue = BindCookie . cs . setCookieValue . SAML.fromSimpleSetCookie

----------------------------------------------------------------------------
-- Identity provider

-- | The identity provider type used in Spar.
type IdP = IdPConfig WireIdP

data WireIdP = WireIdP
  { _wiTeam :: TeamId,
    -- | list of issuer names that this idp has replaced, most recent first.  this is used
    -- for finding users that are still stored under the old issuer, see
    -- 'findUserWithOldIssuer', 'moveUserToNewIssuer'.
    _wiOldIssuers :: [SAML.Issuer],
    -- | the issuer that has replaced this one.  this is set iff a new issuer is created
    -- with the @"replaces"@ query parameter, and it is used to decide whether users not
    -- existing on this IdP can be auto-provisioned (if 'isJust', they can't).
    _wiReplacedBy :: Maybe SAML.IdPId
  }
  deriving (Eq, Show, Generic)

makeLenses ''WireIdP

deriveJSON deriveJSONOptions ''WireIdP

-- | A list of 'IdP's, returned by some endpoints. Wrapped into an object to
-- allow extensibility later on.
data IdPList = IdPList
  { _idplProviders :: [IdP]
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdPList

deriveJSON deriveJSONOptions ''IdPList

-- | JSON-encoded information about metadata: @{"value": <xml>}@.  (Here we could also
-- implement @{"uri": <url>, "cert": <pinned_pubkey>}@.  check both the certificate we get
-- from the server against the pinned one and the metadata url in the metadata against the one
-- we fetched the xml from, but it's unclear what the benefit would be.)
data IdPMetadataInfo = IdPMetadataValue Text SAML.IdPMetadata
  deriving (Eq, Show, Generic)

-- | We want to store the raw xml text from the registration request in the database for
-- trouble shooting, but @SAML.XML@ only gives us access to the xml tree, not the raw text.
-- 'RawXML' helps with that.
data RawXML

instance Accept RawXML where
  contentType Proxy = "application" // "xml"

instance MimeUnrender RawXML IdPMetadataInfo where
  mimeUnrender Proxy raw = IdPMetadataValue (cs raw) <$> mimeUnrender (Proxy @SAML.XML) raw

instance MimeRender RawXML RawIdPMetadata where
  mimeRender Proxy (RawIdPMetadata raw) = cs raw

newtype RawIdPMetadata = RawIdPMetadata Text
  deriving (Eq, Show, Generic)

instance FromJSON IdPMetadataInfo where
  parseJSON = withObject "IdPMetadataInfo" $ \obj -> do
    raw <- obj .: "value"
    either fail (pure . IdPMetadataValue raw) (SAML.decode (cs raw))

instance ToJSON IdPMetadataInfo where
  toJSON (IdPMetadataValue _ xml) =
    object ["value" .= SAML.encode xml]

----------------------------------------------------------------------------
-- SCIM

-- | > docs/reference/provisioning/scim-token.md {#RefScimToken}
--
-- A bearer token that authorizes a provisioning tool to perform actions with a team. Each
-- token corresponds to one team.
--
-- For SCIM authentication and token handling logic, see "Spar.Scim.Auth".
newtype ScimToken = ScimToken {fromScimToken :: Text}
  deriving (Eq, Show, FromJSON, ToJSON, FromByteString, ToByteString)

-- | Metadata that we store about each token.
data ScimTokenInfo = ScimTokenInfo
  { -- | Which team can be managed with the token
    stiTeam :: !TeamId,
    -- | Token ID, can be used to eg. delete the token
    stiId :: !ScimTokenId,
    -- | Time of token creation
    stiCreatedAt :: !UTCTime,
    -- | IdP that created users will "belong" to
    stiIdP :: !(Maybe IdPId),
    -- | Free-form token description, can be set
    --   by the token creator as a mental aid
    stiDescr :: !Text
  }
  deriving (Eq, Show)

instance FromHttpApiData ScimToken where
  parseHeader h = ScimToken <$> parseHeaderWithPrefix "Bearer " h
  parseQueryParam p = ScimToken <$> parseQueryParam p

instance ToHttpApiData ScimToken where
  toHeader (ScimToken s) = "Bearer " <> encodeUtf8 s
  toQueryParam (ScimToken s) = toQueryParam s

instance FromJSON ScimTokenInfo where
  parseJSON = withObject "ScimTokenInfo" $ \o -> do
    stiTeam <- o .: "team"
    stiId <- o .: "id"
    stiCreatedAt <- o .: "created_at"
    stiIdP <- o .: "idp"
    stiDescr <- o .: "description"
    pure ScimTokenInfo {..}

instance ToJSON ScimTokenInfo where
  toJSON s =
    object $
      "team" .= stiTeam s
        # "id" .= stiId s
        # "created_at" .= stiCreatedAt s
        # "idp" .= stiIdP s
        # "description" .= stiDescr s
        # []

----------------------------------------------------------------------------
-- Requests and verdicts

type AReqId = ID AuthnRequest

type AssId = ID Assertion

-- | Clients can request different ways of receiving the final 'AccessVerdict' when fetching their
-- 'AuthnRequest'.  Web-based clients want an html page, mobile clients want to set two URIs for the
-- two resp. 'AccessVerdict' constructors.  This format is stored in cassandra under the request id
-- so that the verdict handler can act on it.
data VerdictFormat
  = VerdictFormatWeb
  | VerdictFormatMobile {_verdictFormatGrantedURI :: URI, _verdictFormatDeniedURI :: URI}
  deriving (Eq, Show, Generic)

makeLenses ''VerdictFormat

deriveJSON deriveJSONOptions ''VerdictFormat

mkVerdictGrantedFormatMobile :: MonadError String m => URI -> SetCookie -> UserId -> m URI
mkVerdictGrantedFormatMobile before cky uid =
  parseURI'
    . substituteVar "cookie" (cs . Builder.toLazyByteString . renderSetCookie $ cky)
    . substituteVar "userid" (cs . show $ uid)
    $ renderURI before

mkVerdictDeniedFormatMobile :: MonadError String m => URI -> ST -> m URI
mkVerdictDeniedFormatMobile before lbl =
  parseURI'
    . substituteVar "label" lbl
    $ renderURI before

substituteVar :: ST -> ST -> ST -> ST
substituteVar var val = substituteVar' ("$" <> var) val . substituteVar' ("%24" <> var) val

substituteVar' :: ST -> ST -> ST -> ST
substituteVar' var val = ST.intercalate val . ST.splitOn var

type Opts = Opts' DerivedOpts

data Opts' a = Opts
  { saml :: !SAML.Config,
    brig :: !Endpoint,
    galley :: !Endpoint,
    cassandra :: !CassandraOpts,
    maxttlAuthreq :: !(TTL "authreq"),
    maxttlAuthresp :: !(TTL "authresp"),
    -- | The maximum number of SCIM tokens that we will allow teams to have.
    maxScimTokens :: !Int,
    -- | The maximum size of rich info. Should be in sync with 'Brig.Types.richInfoLimit'.
    richInfoLimit :: !Int,
    -- | Wire/AWS specific; optional; used to discover Cassandra instance
    -- IPs using describe-instances.
    discoUrl :: !(Maybe Text),
    logNetStrings :: !(Maybe (Last Bool)),
    logFormat :: !(Maybe (Last LogFormat)),
    -- , optSettings   :: !Settings  -- (nothing yet; see other services for what belongs in here.)
    derivedOpts :: !a
  }
  deriving (Functor, Show, Generic)

instance FromJSON (Opts' (Maybe ()))

data DerivedOpts = DerivedOpts
  { derivedOptsBindCookiePath :: !SBS,
    derivedOptsScimBaseURI :: !URI
  }
  deriving (Show, Generic)

-- | (seconds)
newtype TTL (tablename :: Symbol) = TTL {fromTTL :: Int32}
  deriving (Eq, Ord, Show, Num)

showTTL :: KnownSymbol a => TTL a -> String
showTTL (TTL i :: TTL a) = "TTL:" <> (symbolVal (Proxy @a)) <> ":" <> show i

instance FromJSON (TTL a) where
  parseJSON = withScientific "TTL value (seconds)" (pure . TTL . round)

data TTLError = TTLTooLong String String | TTLNegative String
  deriving (Eq, Show)

ttlToNominalDiffTime :: TTL a -> NominalDiffTime
ttlToNominalDiffTime (TTL i32) = fromIntegral i32

maxttlAuthreqDiffTime :: Opts -> NominalDiffTime
maxttlAuthreqDiffTime = ttlToNominalDiffTime . maxttlAuthreq

data SsoSettings = SsoSettings
  { defaultSsoCode :: !(Maybe IdPId)
  }
  deriving (Generic, Show)

instance FromJSON SsoSettings where
  parseJSON = withObject "SsoSettings" $ \obj -> do
    -- key needs to be present, but can be null
    SsoSettings <$> obj .: "default_sso_code"

instance ToJSON SsoSettings where
  toJSON SsoSettings {defaultSsoCode} =
    object ["default_sso_code" .= defaultSsoCode]
