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
import SAML2.WebSSO.Config
import Control.Lens (makeLenses)
import GHC.Generics
import Data.Id (TeamId)

import URI.ByteString
-- import URI.ByteString.QQ

-- import qualified SAML2.WebSSO as SAML
import qualified Data.X509 as X509
import qualified Data.Text as ST
-- import qualified Data.ByteString.Base64.Lazy as B64

type IDP = IdPConfig TeamId

-- data NewIdP = NewIdP
--   { _idpMetadata        :: URI
--   , _idpIssuer          :: Issuer
--   , _idpRequestUri      :: URI
--   , _idpPublicKey       :: X509.SignedCertificate
--   }
--   deriving (Eq, Show, Generic)

-- makeLenses ''NewIdP
-- custom or derived JSON instances?

type NewIdP = IDP -- do we generate an IdP identifier? If so, NewIdP will become a subset of IdP.
