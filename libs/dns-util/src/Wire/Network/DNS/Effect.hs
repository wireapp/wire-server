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
{-# LANGUAGE TemplateHaskell #-}

module Wire.Network.DNS.Effect where

import Imports
import Network.DNS (Domain, Resolver)
import qualified Network.DNS as DNS
import Polysemy
import qualified Wire.Network.DNS.SRV as SRV

data DNSLookup m a where
  LookupSRV :: Domain -> DNSLookup m SRV.SrvResponse

makeSem ''DNSLookup

runDNSLookupDefault :: Member (Embed IO) r => Sem (DNSLookup ': r) a -> Sem r a
runDNSLookupDefault =
  interpret $ \(LookupSRV domain) -> embed $ do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver rs $ \resolver ->
      SRV.interpretResponse <$> DNS.lookupSRV resolver domain

runDNSLookupWithResolver :: Member (Embed IO) r => Resolver -> Sem (DNSLookup ': r) a -> Sem r a
runDNSLookupWithResolver resolver = interpret $ \(LookupSRV domain) ->
  embed (SRV.interpretResponse <$> DNS.lookupSRV resolver domain)
