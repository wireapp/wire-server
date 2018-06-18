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

-- import Data.Aeson
-- import Data.Aeson.TH
-- import SAML2.WebSSO.Config.TH (deriveJSONOptions)
import Control.Lens (makeLenses)
import GHC.Generics

import URI.ByteString
-- import URI.ByteString.QQ

import qualified Data.X509 as X509
import qualified Data.Text as ST

-- Very similar to SAML2.WebSSO.Config's IdPConfig
-- TODO: Type reuse?
data IdP = IdP
  { _idpPath            :: ST.Text
  , _idpMetadata        :: URI
  , _idpIssuer          :: ST.Text -- Issuer
  , _idpRequestUri      :: URI
  , _idpPublicKey       :: X509.SignedCertificate
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdP
-- custom or derived JSON instances?
--deriveJSON deriveJSONOptions ''IdP

type NewIdP = IdP -- do we generate an IdP identifier? If so, NewIdP will become a subset of IdP.
