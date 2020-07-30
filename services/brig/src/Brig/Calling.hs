{-# LANGUAGE RecordWildCards #-}

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

import Brig.Options (SFTOptions (..), defSftDiscoveryIntervalSeconds, defSftServiceName)
import qualified Brig.Options as Opts
import Brig.PolyLog
import Brig.Types (TurnURI)
import Control.Lens
import Data.List.NonEmpty
import Data.List1
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Imports
import qualified Network.DNS as DNS
import OpenSSL.EVP.Digest (Digest)
import Polysemy
import qualified System.Logger as Log
import System.Random.MWC (GenIO, createSystemRandom)
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV

data SFTEnv = SFTEnv
  { -- | Starts off as `NotDiscoveredYet`, once it has servers, it should never
    -- go back to `NotDiscoveredYet` and continue having stale values if
    -- subsequent discovries fail
    sftServers :: IORef (Discovery (NonEmpty SrvEntry)),
    sftDomain :: DNS.Domain,
    -- | Microseconds, as expected by 'threadDelay'
    sftDiscoveryInterval :: Int
  }

data Discovery a
  = NotDiscoveredYet
  | Discovered a
  deriving (Show, Eq)

discoveryToMaybe :: Discovery a -> Maybe a
discoveryToMaybe = \case
  NotDiscoveredYet -> Nothing
  Discovered x -> Just x

discoverSFTServers :: Members [DNSLookup, PolyLog] r => DNS.Domain -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSFTServers domain =
  lookupSRV domain >>= \case
    SrvAvailable es -> pure $ Just es
    SrvNotAvailable -> do
      polyLog Log.Warn (Log.msg ("No SFT servers available" :: ByteString))
      pure Nothing
    SrvResponseError e -> do
      polyLog Log.Error (Log.msg ("DNS Lookup failed for SFT Discovery" :: ByteString) . Log.field "Error" (show e))
      pure Nothing

mkSFTDomain :: SFTOptions -> DNS.Domain
mkSFTDomain SFTOptions {..} = DNS.normalize $ maybe defSftServiceName ("_" <>) sftSRVServiceName <> "._tcp." <> sftBaseDomain

-- FUTUREWORK: Remove Embed IO from here and put threadDelay into another
-- effect. This will also make tests for this faster and deterministic
sftDiscoveryLoop :: Members [DNSLookup, PolyLog, Embed IO] r => SFTEnv -> Sem r ()
sftDiscoveryLoop SFTEnv {..} = forever $ do
  servers <- discoverSFTServers sftDomain
  case servers of
    Nothing -> pure ()
    Just es -> atomicWriteIORef sftServers (Discovered es)
  threadDelay sftDiscoveryInterval

mkSFTEnv :: SFTOptions -> IO SFTEnv
mkSFTEnv opts =
  SFTEnv
    <$> newIORef NotDiscoveredYet
    <*> pure (mkSFTDomain opts)
    <*> pure (diffTimeToMicroseconds (fromMaybe defSftDiscoveryIntervalSeconds (Opts.sftDiscoveryIntervalSeconds opts)))

startSFTServiceDiscovery :: Log.Logger -> SFTEnv -> IO ()
startSFTServiceDiscovery logger =
  runM . runPolyLog logger . runDNSLookupDefault . sftDiscoveryLoop

-- | >>> diffTimeToMicroseconds 1
-- 1000000
diffTimeToMicroseconds :: DiffTime -> Int
diffTimeToMicroseconds = fromIntegral . (`quot` 1000000) . diffTimeToPicoseconds

-- TURN specific

data Env = Env
  { _turnServers :: List1 TurnURI,
    _turnTokenTTL :: Word32,
    _turnConfigTTL :: Word32,
    _turnSecret :: ByteString,
    _turnSHA512 :: Digest,
    _turnPrng :: GenIO
  }

makeLenses ''Env

newEnv :: Digest -> List1 TurnURI -> Word32 -> Word32 -> ByteString -> IO Env
newEnv sha512 srvs tTTL cTTL secret = Env srvs tTTL cTTL secret sha512 <$> createSystemRandom
