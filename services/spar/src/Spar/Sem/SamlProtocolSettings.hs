module Spar.Sem.SamlProtocolSettings where

import Data.Id (TeamId)
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import qualified URI.ByteString as URI

data SamlProtocolSettings m a where
  SpIssuer :: Maybe TeamId -> SamlProtocolSettings m SAML.Issuer
  ResponseURI :: Maybe TeamId -> SamlProtocolSettings m URI.URI

makeSem ''SamlProtocolSettings
