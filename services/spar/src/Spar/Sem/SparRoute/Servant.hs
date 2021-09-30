{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.Sem.SparRoute.Servant where

import Spar.Sem.SparRoute
import Polysemy
import Imports
import Wire.API.Routes.Public.Spar
import qualified SAML2.WebSSO as SAML

-- TODO(sandy): Why is this instance not provided by SAML? Very rude!
instance SAML.HasConfig ((->) SAML.Config) where
  getConfig = id

sparRouteToServant :: SAML.Config -> Sem (SparRoute ': r) a -> Sem r a
sparRouteToServant cfg = interpret $ \x -> case x of
  SpIssuer mitlt -> pure $ sparSPIssuer mitlt cfg
  ResponseURI mitlt -> pure $ sparResponseURI mitlt cfg

