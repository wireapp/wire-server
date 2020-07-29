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

module Wire.Network.DNS.Effect where

import Imports
import Network.DNS (Domain)
import qualified Network.DNS as DNS
import Polysemy
import Wire.Network.DNS.SRV

data DNSLookup m a where
  LookupSRV :: Domain -> DNSLookup m SrvResponse

makeSem ''DNSLookup

runDNSLookupDefault :: Member (Embed IO) r => Sem (DNSLookup ': r) a -> Sem r a
runDNSLookupDefault =
  interpret $ \l -> do
    rs <- embed $ DNS.makeResolvSeed DNS.defaultResolvConf
    embed $ DNS.withResolver rs $ \resolver ->
      case l of
        LookupSRV domain -> interpretResponse <$> DNS.lookupSRV resolver domain

-- TODO: remove comment?
-- class Monad m => MonadDNSLookup m where
--   monadLookupSRV :: Domain -> m SrvResponse
-- I don't know how to make this choice at runtime
