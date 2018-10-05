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
import SAML2.Util (renderURI, parseURI')
import SAML2.WebSSO (IdPConfig, ID, AuthnRequest)
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import URI.ByteString
import Web.Cookie

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as ST


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
-- Requests and verdicts

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
