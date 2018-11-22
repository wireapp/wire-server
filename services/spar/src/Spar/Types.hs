{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Spar.Types where

import Imports
import Control.Lens (makeLenses)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Id (TeamId, UserId)
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions
import Data.String.Conversions (ST)
import Data.Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types (Symbol)
import SAML2.Util (renderURI, parseURI')
import SAML2.WebSSO (IdPConfig, ID, AuthnRequest, Assertion, SimpleSetCookie)
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import URI.ByteString
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

----------------------------------------------------------------------------
-- SCIM

-- | A bearer token that authorizes a provisioning tool to perform actions
-- with a team. Each token corresponds to one team.
newtype ScimToken = ScimToken { fromScimToken :: Text }
  deriving (Eq, Show, FromJSON, ToJSON)

instance FromHttpApiData ScimToken where
  parseHeader h = ScimToken <$> parseHeaderWithPrefix "Bearer " h
  parseQueryParam p = ScimToken <$> parseQueryParam p

instance ToHttpApiData ScimToken where
  toHeader (ScimToken s) = "Bearer " <> encodeUtf8 s
  toQueryParam (ScimToken s) = toQueryParam s

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
