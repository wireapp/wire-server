{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.Sem.SamlProtocolSettings.Servant where

import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Spar.Sem.SamlProtocolSettings
import Wire.API.Routes.Public.Spar

-- TODO(sandy): Why is this instance not provided by SAML? Very rude!
instance SAML.HasConfig ((->) SAML.Config) where
  getConfig = id

sparRouteToServant :: SAML.Config -> Sem (SamlProtocolSettings ': r) a -> Sem r a
sparRouteToServant cfg = interpret $ \x -> case x of
  SpIssuer mitlt -> pure $ sparSPIssuer mitlt cfg
  ResponseURI mitlt -> pure $ sparResponseURI mitlt cfg
