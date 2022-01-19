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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import Federator.Error
import Imports
import qualified Network.DNS as DNS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as TinyLog
import qualified System.Logger.Class as Log
import Wire.Network.DNS.Effect (DNSLookup)
import qualified Wire.Network.DNS.Effect as Lookup
import Wire.Network.DNS.SRV (SrvEntry (srvTarget), SrvResponse (..), SrvTarget)

data DiscoveryFailure
  = DiscoveryFailureSrvNotAvailable ByteString
  | DiscoveryFailureDNSError ByteString
  deriving (Show, Eq, Typeable)

instance Exception DiscoveryFailure

instance AsWai DiscoveryFailure where
  toWai e = Wai.mkError status label (LText.fromStrict (waiErrorDescription e))
    where
      (status, label) = case e of
        DiscoveryFailureSrvNotAvailable _ -> (HTTP.status422, "invalid-domain")
        DiscoveryFailureDNSError _ -> (HTTP.status400, "discovery-failure")
  waiErrorDescription :: DiscoveryFailure -> Text
  waiErrorDescription (DiscoveryFailureSrvNotAvailable msg) =
    "srv record not found: " <> Text.decodeUtf8 msg
  waiErrorDescription (DiscoveryFailureDNSError msg) =
    "DNS error: " <> Text.decodeUtf8 msg

data DiscoverFederator m a where
  DiscoverFederator :: Domain -> DiscoverFederator m (Either DiscoveryFailure SrvTarget)
  DiscoverAllFederators :: Domain -> DiscoverFederator m (Either DiscoveryFailure (NonEmpty SrvTarget))

makeSem ''DiscoverFederator

discoverFederatorWithError ::
  Members '[DiscoverFederator, Polysemy.Error DiscoveryFailure] r =>
  Domain ->
  Sem r SrvTarget
discoverFederatorWithError = Polysemy.fromEither <=< discoverFederator

discoverAllFederatorsWithError ::
  Members '[DiscoverFederator, Polysemy.Error DiscoveryFailure] r =>
  Domain ->
  Sem r (NonEmpty SrvTarget)
discoverAllFederatorsWithError = Polysemy.fromEither <=< discoverAllFederators

runFederatorDiscovery :: Members '[DNSLookup, TinyLog] r => Sem (DiscoverFederator ': r) a -> Sem r a
runFederatorDiscovery = interpret $ \case
  DiscoverFederator d ->
    -- FUTUREWORK(federation): orderSrvResult and try the list in order this
    -- will make it not federator specific and then we can move this whole
    -- function to dns-util
    NonEmpty.head <$$> lookupDomainByDNS (domainSrv d)
  DiscoverAllFederators d -> lookupDomainByDNS (domainSrv d)
  where
    -- FUTUREWORK(federation): This string conversion is wrong, we should encode
    -- this using IDNA encoding or expect domain to be bytestring everywhere
    -- (https://wearezeta.atlassian.net/browse/SQCORE-912)
    domainSrv d = cs $ "_wire-server-federator._tcp." <> domainText d

lookupDomainByDNS :: Members '[DNSLookup, TinyLog] r => ByteString -> Sem r (Either DiscoveryFailure (NonEmpty SrvTarget))
lookupDomainByDNS domainSrv = do
  res <- Lookup.lookupSRV domainSrv
  case res of
    SrvAvailable entries ->
      pure $ Right $ srvTarget <$> entries
    SrvNotAvailable ->
      pure $ Left $ DiscoveryFailureSrvNotAvailable domainSrv
    SrvResponseError DNS.NameError ->
      -- Name error also means that the record is not available
      pure $ Left $ DiscoveryFailureSrvNotAvailable domainSrv
    SrvResponseError err -> do
      TinyLog.err $ Log.msg ("DNS Lookup failed" :: ByteString) . Log.field "error" (show err)
      pure $ Left $ DiscoveryFailureDNSError domainSrv
