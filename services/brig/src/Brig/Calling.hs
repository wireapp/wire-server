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

module Brig.Calling where

import Brig.Options (SFTOptions (..))
import Data.List.NonEmpty
import Imports
import qualified Network.DNS as DNS
import Polysemy
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV

data SFTEnv = SFTEnv
  { sftServers :: IORef (Maybe (NonEmpty SrvEntry)),
    sftDomain :: DNS.Domain
  }

-- TODO: Log stuff here (and test it?)
discoverSFTServers :: Member DNSLookup r => DNS.Domain -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSFTServers domain =
  lookupSRV domain >>= \case
    SrvAvailable es -> pure $ Just es
    SrvNotAvailable -> pure Nothing
    SrvResponseError _ -> pure Nothing

mkSFTDomain :: SFTOptions -> DNS.Domain
mkSFTDomain (SFTOptions base maybeSrv) = DNS.normalize $ maybe "_sft" ("_" <>) maybeSrv <> "._tcp." <> base

-- TODO: How can I remove the Embed IO? Even if I cannot, this is better than
-- just IO () as I can still mock DNSLookup
-- TODO: Test that `Nothing` is never explicitly written to the IORef
sftDiscoveryLoop :: Members [DNSLookup, Embed IO] r => SFTEnv -> Sem r ()
sftDiscoveryLoop (SFTEnv serversRef domain) = forever $ do
  servers <- discoverSFTServers domain
  case servers of
    Nothing -> pure ()
    es -> atomicWriteIORef serversRef es
  -- TODO: What should this number be? Use Control.Retry?
  threadDelay 1000000

mkSFTEnv :: SFTOptions -> IO SFTEnv
mkSFTEnv opts =
  SFTEnv
    <$> newIORef Nothing
    <*> pure (mkSFTDomain opts)

startSFTServiceDiscovery :: SFTEnv -> IO ()
startSFTServiceDiscovery =
  runM . runDNSLookupDefault . sftDiscoveryLoop
