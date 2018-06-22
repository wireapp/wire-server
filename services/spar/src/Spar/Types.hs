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

import Control.Lens (makeLenses)
import Data.Aeson.TH
import Data.Id (TeamId)
import GHC.Generics
import SAML2.WebSSO.Config.TH (deriveJSONOptions)
import SAML2.WebSSO (IdPConfig, Issuer)
import URI.ByteString

import qualified Data.X509 as X509


type IdP = IdPConfig TeamId

-- | 'IdPConfig' contains some info that will be filled in by the server when processing the
-- creation request.  'NewIdP' is the type of the data provided by the client in this request.
data NewIdP = NewIdP
  { _nidpMetadata        :: URI
  , _nidpIssuer          :: Issuer
  , _nidpRequestUri      :: URI
  , _nidpPublicKey       :: X509.SignedCertificate
  }
  deriving (Eq, Show, Generic)

makeLenses ''NewIdP
deriveJSON deriveJSONOptions ''NewIdP
