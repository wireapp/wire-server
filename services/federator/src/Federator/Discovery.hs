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

module Federator.Discovery where

import Data.Domain (Domain, domainText)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Wire.Network.DNS.Effect (DNSLookup)
import qualified Wire.Network.DNS.Effect as Lookup
import Wire.Network.DNS.SRV (SrvEntry (srvTarget), SrvResponse (..), SrvTarget)

data LookupError
  = LookupErrorSrvNotAvailable ByteString
  | LookupErrorDNSError ByteString
  deriving (Show, Eq)

data DiscoverFederator m a where
  DiscoverFederator :: Domain -> DiscoverFederator m (Either LookupError SrvTarget)

makeSem ''DiscoverFederator

runFederatorDiscovery :: Members '[DNSLookup] r => Sem (DiscoverFederator ': r) a -> Sem r a
runFederatorDiscovery = interpret $ \(DiscoverFederator d) ->
  -- FUTUREWORK(federation): This string conversation is probably wrong, we should encode this
  -- using IDNA encoding or expect domain to be bytestring everywhere
  let domainSrv = cs $ "_wire-server-federator._tcp." <> domainText d
   in lookupDomainByDNS domainSrv

lookupDomainByDNS :: Member DNSLookup r => ByteString -> Sem r (Either LookupError SrvTarget)
lookupDomainByDNS domainSrv = do
  res <- Lookup.lookupSRV domainSrv
  case res of
    SrvAvailable entries -> do
      -- FUTUREWORK(federation): orderSrvResult and try the list in order this will make it
      -- not federator specific and then we can move this whole function to
      -- dns-util
      pure $ Right $ srvTarget $ NonEmpty.head entries
    SrvNotAvailable -> pure $ Left $ LookupErrorSrvNotAvailable domainSrv
    SrvResponseError _ -> pure $ Left $ LookupErrorDNSError domainSrv
