{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Spar.Types where

import Data.Aeson.TH
import Data.Id (TeamId)
import GHC.Generics
import Lens.Micro.TH (makeLenses)
import SAML2.WebSSO.Config.TH (deriveJSONOptions)
import SAML2.WebSSO (IdPConfig, Issuer)
import URI.ByteString

import qualified Data.X509 as X509


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

-- | 'IdPConfig' contains some info that will be filled in by the server when processing the
-- creation request.  'NewIdP' is the type of the data provided by the client in this request.
--
-- FUTUREWORK: move this to saml2-web-sso.
data NewIdP = NewIdP
  { _nidpMetadata        :: URI
  , _nidpIssuer          :: Issuer  -- TODO: remove this field, it's redundant.  (this will also shorten the list of possible errors in the UI.)
  , _nidpRequestUri      :: URI     -- TODO: dito.
  , _nidpPublicKey       :: X509.SignedCertificate
  }
  deriving (Eq, Show, Generic)

makeLenses ''NewIdP
deriveJSON deriveJSONOptions ''NewIdP
