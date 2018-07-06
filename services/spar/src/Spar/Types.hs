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
import Data.Misc (HttpsUrl)
import GHC.Generics
import Lens.Micro.TH (makeLenses)
import SAML2.WebSSO.Config.TH (deriveJSONOptions)
import SAML2.WebSSO (IdPConfig, Issuer)

import qualified Data.X509 as X509


type IdP = IdPConfig TeamId

-- | 'IdPConfig' contains some info that will be filled in by the server when processing the
-- creation request.  'NewIdP' is the type of the data provided by the client in this request.
--
-- FUTUREWORK: move this to saml2-web-sso.  (note that this will make saml2-web-sso depend on
-- types-common.)
data NewIdP = NewIdP
  { _nidpMetadata        :: HttpsUrl
  , _nidpIssuer          :: Issuer    -- TODO: remove this field, it's redundant.  (this will also shorten the list of possible errors in the UI.)
  , _nidpRequestUri      :: HttpsUrl  -- TODO: dito.
  , _nidpPublicKey       :: X509.SignedCertificate
  }
  deriving (Eq, Show, Generic)

makeLenses ''NewIdP
deriveJSON deriveJSONOptions ''NewIdP
