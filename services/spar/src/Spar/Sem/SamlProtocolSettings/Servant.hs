{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
