{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Data.IP qualified as IP
import Imports
import Network.DNS (Domain, Resolver)
import Network.DNS qualified as DNS
import Polysemy
import Wire.Network.DNS.SRV qualified as SRV

data DNSLookup m a where
  LookupSRV :: Domain -> DNSLookup m SRV.SrvResponse
  LookupA :: Domain -> DNSLookup m (Either DNS.DNSError [IP.IPv4])

makeSem ''DNSLookup

runDNSLookupDefault :: (Member (Embed IO) r) => Sem (DNSLookup ': r) a -> Sem r a
runDNSLookupDefault =
  interpret $ \action -> embed $ do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver rs $ flip runLookupIO action

runDNSLookupWithResolver :: (Member (Embed IO) r) => Resolver -> Sem (DNSLookup ': r) a -> Sem r a
runDNSLookupWithResolver resolver = interpret $ embed . runLookupIO resolver

runLookupIO :: Resolver -> DNSLookup m a -> IO a
runLookupIO resolver action =
  case action of
    LookupSRV domain -> do
      SRV.interpretResponse <$> DNS.lookupSRV resolver domain
    LookupA domain ->
      DNS.lookupA resolver domain
