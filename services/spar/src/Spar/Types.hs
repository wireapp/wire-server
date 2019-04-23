{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | A "default" module for types used in Spar, unless there's a better / more specific place
-- for them.
module Spar.Types where

import Imports
import Control.Lens (makeLenses)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Id (TeamId, UserId, ScimTokenId)
import Data.ByteString.Conversion
import Data.Json.Util
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions
import Data.String.Conversions (ST)
import Data.Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types (Symbol)
import SAML2.Util (renderURI, parseURI')
import SAML2.WebSSO (IdPConfig, IdPId, ID, AuthnRequest, Assertion, SimpleSetCookie)
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import URI.ByteString as URI
import Util.Options
import Web.Cookie
import Web.HttpApiData

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as ST
import qualified SAML2.WebSSO as SAML


type SetBindCookie = SimpleSetCookie "zbind"

newtype BindCookie = BindCookie { fromBindCookie :: ST }

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
type IdP = IdPConfig TeamId

-- | A list of 'IdP's, returned by some endpoints. Wrapped into an object to
-- allow extensibility later on.
data IdPList = IdPList
  { _idplProviders :: [IdP]
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdPList
deriveJSON deriveJSONOptions ''IdPList

-- | JSON-encoded information about metadata: either @{"value": <xml>}@ or @{"uri": <url>}@.
data IdPMetadataInfo =
    IdPMetadataValue SAML.IdPMetadata
  | IdPMetadataURI URI
  deriving (Eq, Show, Generic)

instance FromJSON IdPMetadataInfo where
  parseJSON = withObject "IdPMetadataInfo" $ \obj -> do
    look <- (,) <$> (obj .:? "value") <*> (obj .:? "uri")
    case look of
      (Just xml, Nothing) -> either fail (pure . IdPMetadataValue)
                           $ SAML.decode xml
      (Nothing, Just url) -> either (fail . show) (pure . IdPMetadataURI)
                           $ URI.parseURI URI.laxURIParserOptions (cs @Text @_ url)
      (Nothing, Nothing)  -> fail "empty object"
      (Just _, Just _)    -> fail "only one of value, uri can be given"

-- NB: this is not used anywhere except for in the roundtrip tests.
instance ToJSON IdPMetadataInfo where
  toJSON (IdPMetadataValue xml) =
    object [ "value" .= SAML.encode xml ]
  toJSON (IdPMetadataURI uri) =
    object [ "uri" .= (cs @_ @Text . Builder.toLazyByteString . URI.serializeURIRef $ uri) ]


----------------------------------------------------------------------------
-- SCIM

-- | > docs/reference/provisioning/scim-token.md {#RefScimToken}
--
-- A bearer token that authorizes a provisioning tool to perform actions with a team. Each
-- token corresponds to one team.
--
-- For SCIM authentication and token handling logic, see "Spar.Scim.Auth".
newtype ScimToken = ScimToken { fromScimToken :: Text }
  deriving (Eq, Show, FromJSON, ToJSON, FromByteString, ToByteString)

-- | Metadata that we store about each token.
data ScimTokenInfo = ScimTokenInfo
  { stiTeam      :: !TeamId        -- ^ Which team can be managed with the token
  , stiId        :: !ScimTokenId   -- ^ Token ID, can be used to eg. delete the token
  , stiCreatedAt :: !UTCTime       -- ^ Time of token creation
  , stiIdP       :: !(Maybe IdPId) -- ^ IdP that created users will "belong" to
  , stiDescr     :: !Text          -- ^ Free-form token description, can be set
                                   --   by the token creator as a mental aid
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
    stiTeam      <- o .: "team"
    stiId        <- o .: "id"
    stiCreatedAt <- o .: "created_at"
    stiIdP       <- o .: "idp"
    stiDescr     <- o .: "description"
    pure ScimTokenInfo{..}

instance ToJSON ScimTokenInfo where
  toJSON s = object
      $ "team"        .= stiTeam s
      # "id"          .= stiId s
      # "created_at"  .= stiCreatedAt s
      # "idp"         .= stiIdP s
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
  | VerdictFormatMobile { _verdictFormatGrantedURI :: URI, _verdictFormatDeniedURI :: URI }
  deriving (Eq, Show, Generic)

makeLenses ''VerdictFormat
deriveJSON deriveJSONOptions ''VerdictFormat

mkVerdictGrantedFormatMobile :: MonadError String m => URI -> SetCookie -> UserId -> m URI
mkVerdictGrantedFormatMobile before cky uid
  = parseURI'
  . substituteVar "cookie" (cs . Builder.toLazyByteString . renderSetCookie $ cky)
  . substituteVar "userid" (cs . show $ uid)
  $ renderURI before

mkVerdictDeniedFormatMobile :: MonadError String m => URI -> ST -> m URI
mkVerdictDeniedFormatMobile before lbl
  = parseURI'
  . substituteVar "label" lbl
  $ renderURI before

substituteVar :: ST -> ST -> ST -> ST
substituteVar var val = substituteVar' ("$" <> var) val . substituteVar' ("%24" <> var) val

substituteVar' :: ST -> ST -> ST -> ST
substituteVar' var val = ST.intercalate val . ST.splitOn var


type Opts = Opts' DerivedOpts
data Opts' a = Opts
    { saml           :: !SAML.Config
    , brig           :: !Endpoint
    , galley         :: !Endpoint
    , cassandra      :: !CassandraOpts
    , maxttlAuthreq  :: !(TTL "authreq")
    , maxttlAuthresp :: !(TTL "authresp")
    -- | The maximum number of SCIM tokens that we will allow teams to have.
    , maxScimTokens  :: !Int
    -- | The maximum size of rich info. Should be in sync with 'Brig.Types.richInfoLimit'.
    , richInfoLimit  :: !Int
    -- | Wire/AWS specific; optional; used to discover Cassandra instance
    -- IPs using describe-instances.
    , discoUrl       :: !(Maybe Text)
    , logNetStrings  :: !Bool
    -- , optSettings   :: !Settings  -- (nothing yet; see other services for what belongs in here.)
    , derivedOpts    :: !a
    }
  deriving (Functor, Show, Generic)

instance FromJSON (Opts' (Maybe ()))

data DerivedOpts = DerivedOpts
    { derivedOptsBindCookiePath   :: !SBS
    , derivedOptsBindCookieDomain :: !SBS
    , derivedOptsScimBaseURI      :: !URI
    }
  deriving (Show, Generic)

-- | (seconds)
newtype TTL (tablename :: Symbol) = TTL { fromTTL :: Int32 }
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
