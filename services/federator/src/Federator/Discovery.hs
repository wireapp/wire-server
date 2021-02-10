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
  -- FUTUREWORK: This string conversation is probably wrong, we should encode this
  -- using IDNA encoding or expect domain to be bytestring everywhere
  let domainSrv = cs $ "_wire-server-federator._tcp." <> domainText d
   in lookupDomainByDNS domainSrv

-- Can most of this function live in DNS-Util?
lookupDomainByDNS :: Member DNSLookup r => ByteString -> Sem r (Either LookupError SrvTarget)
lookupDomainByDNS domainSrv = do
  res <- Lookup.lookupSRV domainSrv
  case res of
    SrvAvailable entries -> do
      -- FUTUREWORK: orderSrvResult and try the list in order
      pure $ Right $ srvTarget $ NonEmpty.head entries
    SrvNotAvailable -> pure $ Left $ LookupErrorSrvNotAvailable domainSrv
    SrvResponseError _ -> pure $ Left $ LookupErrorDNSError domainSrv
