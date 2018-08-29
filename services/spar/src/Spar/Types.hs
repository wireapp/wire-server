{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Control.Monad.Except
import Data.Aeson.TH
import Data.Id (TeamId, UserId)
import Data.String.Conversions
import GHC.Generics
import Lens.Micro.TH (makeLenses)
import SAML2.WebSSO (IdPConfig, ID, AuthnRequest)
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Text.XML.Util (renderURI, parseURI')
import URI.ByteString
import Web.Cookie

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as ST


-- | Info about the service provider that can be given to the identity
-- provider to configure the service provider.
data SPInfo = SPInfo
  { _spiMetaURI  :: URI  -- ^ corresponds to 'APIMeta' (unique for Wire)
  , _spiLoginURI :: URI  -- ^ corresponds to 'APIAuthReq' (the prefix without the identity provider id)
  }
  deriving (Eq, Show, Generic)

makeLenses ''SPInfo
deriveJSON deriveJSONOptions ''SPInfo

-- | The identity provider type used in Spar.
type IdP = IdPConfig IdPExtra

-- | Extra information stored for each 'IdP'. The SAML library handles it
-- but never inspects it (see the '_idpExtraInfo' field).
data IdPExtra = IdPExtra
  { _idpeTeam   :: TeamId
  , _idpeSPInfo :: SPInfo
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdPExtra
deriveJSON deriveJSONOptions ''IdPExtra

-- | A list of 'IdP's, returned by some endpoints. Wrapped into an object to
-- allow extensibility later on.
data IdPList = IdPList
  { _idplProviders :: [IdP]
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdPList
deriveJSON deriveJSONOptions ''IdPList

type AReqId = ID AuthnRequest

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
