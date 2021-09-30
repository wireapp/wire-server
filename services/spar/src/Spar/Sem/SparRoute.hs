module Spar.Sem.SparRoute where

import Data.Id (TeamId)
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import qualified URI.ByteString as URI

data SparRoute m a where
  SpIssuer :: Maybe TeamId -> SparRoute m SAML.Issuer
  ResponseURI :: Maybe TeamId -> SparRoute m URI.URI

makeSem ''SparRoute
