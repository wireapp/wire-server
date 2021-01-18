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

module Brig.Calling
  ( getRandomSFTServers,
    mkSFTDomain,
    SFTServers, -- See NOTE SFTServers
    mkSFTServers,
    SFTEnv (..),
    Discovery (..),
    Env (..),
    mkSFTEnv,
    newEnv,
    sftDiscoveryLoop,
    discoverSFTServers,
    discoveryToMaybe,
    randomize,
    startSFTServiceDiscovery,
    turnServers,
    turnTokenTTL,
    turnConfigTTL,
    turnSecret,
    turnSHA512,
    turnPrng,
  )
where

import Brig.Options (SFTOptions (..), defSftDiscoveryIntervalSeconds, defSftListLength, defSftServiceName)
import qualified Brig.Options as Opts
import Brig.Types (TurnURI)
import Control.Lens
import Control.Monad.Random.Class (MonadRandom)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List1
import Data.Range
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Imports
import qualified Network.DNS as DNS
import OpenSSL.EVP.Digest (Digest)
import Polysemy
import Polysemy.TinyLog
import qualified System.Logger as Log
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.Shuffle
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV

-- | NOTE SFTServers:
-- Retrieving SFTServers should give a 1) randomized and 2) limited list of servers.
-- Random as a (poor) way of "load balancing" for clients
-- And limited since client currently try contacting all servers returned
-- (and we don't want them to open 100 parallel connections unnecessarily)
-- Therefore, we hide the constructor from the module export.
newtype SFTServers = SFTServers (NonEmpty SrvEntry)
  deriving (Eq, Show)

mkSFTServers :: NonEmpty SrvEntry -> SFTServers
mkSFTServers = SFTServers

type MaximumSFTServers = 100

-- | According to RFC2782, the SRV Entries are supposed to be tried in order of
-- priority and weight, but we internally agreed to randomize the list of
-- available servers for poor man's "load balancing" purposes.
-- FUTUREWORK: be smarter about list orderding depending on how much capacity SFT servers have.
-- randomizedSftEntries <- liftIO $ mapM randomize sftSrvEntries
--
-- Currently (Sept 2020) the client initiating an SFT call will try all
-- servers in this list. Limit this list to a smaller subset in case many
-- SFT servers are advertised in a given environment.
getRandomSFTServers :: MonadRandom m => Range 1 MaximumSFTServers Int -> SFTServers -> m (NonEmpty SrvEntry)
getRandomSFTServers limit (SFTServers list) = subsetSft limit <$> randomize list

subsetSft :: Range 1 100 Int -> NonEmpty a -> NonEmpty a
subsetSft l entries = do
  let entry1 = NonEmpty.head entries
  let entryTail = take (fromRange l - 1) (NonEmpty.tail entries)
  entry1 :| entryTail

-- | Note: Even though 'shuffleM' works only for [a], input is NonEmpty so it's
-- safe to NonEmpty.fromList; ideally, we'd have 'shuffleM' for 'NonEmpty'
randomize :: (MonadRandom m) => NonEmpty a -> m (NonEmpty a)
randomize xs = NonEmpty.fromList <$> shuffleM (NonEmpty.toList xs)

data SFTEnv = SFTEnv
  { -- | Starts off as `NotDiscoveredYet`, once it has servers, it should never
    -- go back to `NotDiscoveredYet` and continue having stale values if
    -- subsequent discoveries fail
    sftServers :: IORef (Discovery SFTServers),
    sftDomain :: DNS.Domain,
    -- | Microseconds, as expected by 'threadDelay'
    sftDiscoveryInterval :: Int,
    -- | maximum amount of servers to give out,
    -- even if more are in the SRV record
    sftListLength :: Range 1 100 Int
  }

data Discovery a
  = NotDiscoveredYet
  | Discovered a
  deriving (Show, Eq)

discoveryToMaybe :: Discovery a -> Maybe a
discoveryToMaybe = \case
  NotDiscoveredYet -> Nothing
  Discovered x -> Just x

discoverSFTServers :: Members [DNSLookup, TinyLog] r => DNS.Domain -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSFTServers domain =
  lookupSRV domain >>= \case
    SrvAvailable es -> pure $ Just es
    SrvNotAvailable -> do
      warn (Log.msg ("No SFT servers available" :: ByteString))
      pure Nothing
    SrvResponseError e -> do
      err (Log.msg ("DNS Lookup failed for SFT Discovery" :: ByteString) . Log.field "Error" (show e))
      pure Nothing

mkSFTDomain :: SFTOptions -> DNS.Domain
mkSFTDomain SFTOptions {..} = DNS.normalize $ maybe defSftServiceName ("_" <>) sftSRVServiceName <> "._tcp." <> sftBaseDomain

-- FUTUREWORK: Remove Embed IO from here and put threadDelay into another
-- effect. This will also make tests for this faster and deterministic
sftDiscoveryLoop :: Members [DNSLookup, TinyLog, Embed IO] r => SFTEnv -> Sem r ()
sftDiscoveryLoop SFTEnv {..} = forever $ do
  servers <- discoverSFTServers sftDomain
  case servers of
    Nothing -> pure ()
    Just es -> atomicWriteIORef sftServers (Discovered (SFTServers es))
  threadDelay sftDiscoveryInterval

mkSFTEnv :: SFTOptions -> IO SFTEnv
mkSFTEnv opts =
  SFTEnv
    <$> newIORef NotDiscoveredYet
    <*> pure (mkSFTDomain opts)
    <*> pure (diffTimeToMicroseconds (fromMaybe defSftDiscoveryIntervalSeconds (Opts.sftDiscoveryIntervalSeconds opts)))
    <*> pure (fromMaybe defSftListLength (Opts.sftListLength opts))

startSFTServiceDiscovery :: Log.Logger -> SFTEnv -> IO ()
startSFTServiceDiscovery logger =
  runM . runTinyLog logger . runDNSLookupDefault . sftDiscoveryLoop

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
