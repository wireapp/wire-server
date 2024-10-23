{-# LANGUAGE RecordWildCards #-}
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

module Brig.Calling
  ( getRandomElements,
    mkSFTDomain,
    SFTServers, -- See NOTE SFTServers
    unSFTServers,
    mkSFTServers,
    SFTEnv (..),
    SFTTokenEnv (..),
    Discovery (..),
    TurnEnv,
    TurnServers (..),
    mkSFTEnv,
    mkTurnEnv,
    srvDiscoveryLoop,
    sftDiscoveryLoop,
    discoverSRVRecords,
    discoveryToMaybe,
    randomize,
    startSFTServiceDiscovery,
    startTurnDiscovery,
    turnServers,
    turnServersV1,
    turnServersV2,
    turnTokenTTL,
    turnConfigTTL,
    turnSecret,
    turnSHA512,
  )
where

import Brig.Options (SFTOptions (..), defSftListLength, defSftServiceName, defSrvDiscoveryIntervalSeconds)
import Brig.Options qualified as Opts
import Control.Exception.Enclosed (handleAny)
import Control.Lens
import Control.Monad.Random.Class (MonadRandom)
import Data.ByteString qualified as BS
import Data.ByteString.Conversion (fromByteString)
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc
import Data.Range
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error
import Data.Text.IO qualified as Text
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Imports
import Network.DNS qualified as DNS
import OpenSSL.EVP.Digest (Digest)
import Polysemy
import Polysemy.TinyLog
import System.FSNotify qualified as FS
import System.FilePath qualified as Path
import System.Logger qualified as Log
import System.Random.Shuffle
import UnliftIO (Async)
import UnliftIO.Async qualified as Async
import Wire.API.Call.Config
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV
import Wire.Sem.Delay
import Wire.Sem.Logger.TinyLog

-- | NOTE SFTServers:
-- Retrieving SFTServers should give a 1) randomized and 2) limited list of servers.
-- Random as a (poor) way of "load balancing" for clients
-- And limited since client currently try contacting all servers returned
-- (and we don't want them to open 100 parallel connections unnecessarily)
-- Therefore, we hide the constructor from the module export.
newtype SFTServers = SFTServers {unSFTServers :: NonEmpty SrvEntry}
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
getRandomElements ::
  (MonadRandom f) =>
  Range 1 MaximumSFTServers Int ->
  NonEmpty a ->
  f (NonEmpty a)
getRandomElements limit list = subsetSft limit <$> randomize list

subsetSft :: Range 1 MaximumSFTServers Int -> NonEmpty a -> NonEmpty a
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
    sftListLength :: Range 1 100 Int,
    -- | token parameters
    sftToken :: Maybe SFTTokenEnv
  }

data Discovery a
  = NotDiscoveredYet
  | Discovered a
  deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Discovery a) where
  NotDiscoveredYet <> other = other
  other <> NotDiscoveredYet = other
  Discovered x <> Discovered y = Discovered (x <> y)

instance (Semigroup a) => Monoid (Discovery a) where
  mempty = NotDiscoveredYet

discoveryToMaybe :: Discovery a -> Maybe a
discoveryToMaybe = \case
  NotDiscoveredYet -> Nothing
  Discovered x -> Just x

discoverSRVRecords :: (Members [DNSLookup, TinyLog] r) => DNS.Domain -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSRVRecords domain =
  lookupSRV domain >>= \case
    SrvAvailable es -> pure $ Just es
    SrvNotAvailable -> do
      warn $
        Log.msg (Log.val "SRV Records not available")
          . Log.field "domain" domain
      pure Nothing
    -- It is not an error if the record doesn't exist
    SrvResponseError DNS.NameError -> do
      warn $
        Log.msg (Log.val "SRV Records not available")
          . Log.field "domain" domain
      pure Nothing
    SrvResponseError e -> do
      err $
        Log.msg (Log.val "SRV Lookup failed")
          . Log.field "Error" (show e)
          . Log.field "domain" domain
      pure Nothing

srvDiscoveryLoop :: (Members [DNSLookup, TinyLog, Delay] r) => DNS.Domain -> Int -> (NonEmpty SrvEntry -> Sem r ()) -> Sem r ()
srvDiscoveryLoop domain discoveryInterval saveAction = forever $ do
  servers <- discoverSRVRecords domain
  forM_ servers saveAction
  delay discoveryInterval

data SFTTokenEnv = SFTTokenEnv
  { sftTokenTTL :: Word32,
    sftTokenSecret :: ByteString,
    sftTokenSHA :: Digest
  }

mkSFTDomain :: SFTOptions -> DNS.Domain
mkSFTDomain SFTOptions {..} = DNS.normalize $ maybe defSftServiceName ("_" <>) sftSRVServiceName <> "._tcp." <> sftBaseDomain

sftDiscoveryLoop :: (Members [DNSLookup, TinyLog, Delay, Embed IO] r) => SFTEnv -> Sem r ()
sftDiscoveryLoop SFTEnv {..} =
  srvDiscoveryLoop sftDomain sftDiscoveryInterval $
    atomicWriteIORef sftServers . Discovered . SFTServers

mkSFTEnv :: Digest -> SFTOptions -> IO SFTEnv
mkSFTEnv digest opts =
  SFTEnv
    <$> newIORef NotDiscoveredYet
    <*> pure (mkSFTDomain opts)
    <*> pure (diffTimeToMicroseconds (fromMaybe defSrvDiscoveryIntervalSeconds (Opts.sftDiscoveryIntervalSeconds opts)))
    <*> pure (fromMaybe defSftListLength (Opts.sftListLength opts))
    <*> forM (Opts.sftTokenOptions opts) (mkSFTTokenEnv digest)

mkSFTTokenEnv :: Digest -> Opts.SFTTokenOptions -> IO SFTTokenEnv
mkSFTTokenEnv digest opts =
  SFTTokenEnv (Opts.sttTTL opts)
    <$> BS.readFile (Opts.sttSecret opts)
    <*> pure digest

-- | Start SFT service discovery synchronously
startSFTServiceDiscovery :: Log.Logger -> SFTEnv -> IO ()
startSFTServiceDiscovery logger =
  runM . loggerToTinyLog logger . runDNSLookupDefault . runDelay . sftDiscoveryLoop

-- | >>> diffTimeToMicroseconds 1
-- 1000000
diffTimeToMicroseconds :: DiffTime -> Int
diffTimeToMicroseconds = fromIntegral . (`quot` 1000000) . diffTimeToPicoseconds

-- TURN specific

type TurnServersRef = IORef (Discovery (NonEmpty TurnURI))

data TurnServers
  = TurnServersFromFiles Opts.TurnServersFiles TurnServersRef TurnServersRef
  | TurnServersFromDNS Opts.TurnDnsOpts TurnServersRef TurnServersRef TurnServersRef TurnServersRef

data TurnEnv = TurnEnv
  { _turnServers :: TurnServers,
    _turnTokenTTL :: Word32,
    _turnConfigTTL :: Word32,
    _turnSecret :: ByteString,
    _turnSHA512 :: Digest
  }

makeLenses ''TurnEnv

mkTurnEnv :: Opts.TurnServersSource -> Word32 -> Word32 -> ByteString -> Digest -> IO TurnEnv
mkTurnEnv serversSource _turnTokenTTL _turnConfigTTL _turnSecret _turnSHA512 = do
  _turnServers <- case serversSource of
    Opts.TurnSourceDNS opts ->
      do
        TurnServersFromDNS opts
        <$> newIORef NotDiscoveredYet
        <*> newIORef NotDiscoveredYet
        <*> newIORef NotDiscoveredYet
        <*> newIORef NotDiscoveredYet
    Opts.TurnSourceFiles files -> do
      TurnServersFromFiles files
        <$> newIORef NotDiscoveredYet
        <*> newIORef NotDiscoveredYet
  pure $ TurnEnv {..}

turnServersV1 :: (MonadIO m) => TurnServers -> m (Discovery (NonEmpty TurnURI))
turnServersV1 =
  readIORef . \case
    TurnServersFromFiles _opts v1Ref _v2Ref ->
      v1Ref
    TurnServersFromDNS _opts v1UdpRef _v2UdpRef _tcpRef _tlsRef ->
      v1UdpRef

turnServersV2 :: (MonadIO m) => TurnServers -> m (Discovery (NonEmpty TurnURI))
turnServersV2 = \case
  TurnServersFromFiles _opts _v1Udref v2Ref ->
    readIORef v2Ref
  TurnServersFromDNS _opts _v1UdpRef v2UdpRef tcpRef tlsRef ->
    mconcat <$> mapM readIORef [v2UdpRef, tcpRef, tlsRef]

-- | Start TURN service discovery asychronously.
startTurnDiscovery :: Log.Logger -> FS.WatchManager -> TurnEnv -> IO [Async ()]
startTurnDiscovery l w env = do
  case env ^. turnServers of
    TurnServersFromFiles files v1Ref v2Ref -> do
      startFileBasedTurnDiscovery l w files v1Ref v2Ref
      -- File based discovery runs in background using fsnotify, so we don't
      -- need to watch any async processes.
      pure []
    TurnServersFromDNS dnsOpts deprecatedUdpRef udpRef tcpRef tlsRef ->
      startDNSBasedTurnDiscovery l dnsOpts deprecatedUdpRef udpRef tcpRef tlsRef

startDNSBasedTurnDiscovery :: Log.Logger -> Opts.TurnDnsOpts -> TurnServersRef -> TurnServersRef -> TurnServersRef -> TurnServersRef -> IO [Async ()]
startDNSBasedTurnDiscovery logger opts deprecatedUdpRef udpRef tcpRef tlsRef = do
  let udpDomain = DNS.normalize $ "_turn._udp." <> Opts.tdoBaseDomain opts
      tcpDomain = DNS.normalize $ "_turn._tcp." <> Opts.tdoBaseDomain opts
      tlsDomain = DNS.normalize $ "_turns._tcp." <> Opts.tdoBaseDomain opts
      interval = diffTimeToMicroseconds (fromMaybe defSrvDiscoveryIntervalSeconds (Opts.tdoDiscoveryIntervalSeconds opts))
      runLoopAsync domain =
        Async.async
          . runM
          . loggerToTinyLog logger
          . runDNSLookupDefault
          . runDelay
          . srvDiscoveryLoop domain interval
          . withNonZeroWeightRecords
  udpLoop <- runLoopAsync udpDomain $
    \records -> do
      ipsAndPorts <- fetchIpsAndPorts records
      case ipsAndPorts of
        [] -> pure ()
        (x : xs) ->
          atomicWriteIORef deprecatedUdpRef . Discovered $ uncurry turnURIFromIpAndPort <$> (x :| xs)
      atomicWriteIORef udpRef . Discovered $ turnURIFromSRV SchemeTurn (Just TransportUDP) <$> records

  tcpLoop <-
    runLoopAsync tcpDomain $
      atomicWriteIORef tcpRef . Discovered . fmap (turnURIFromSRV SchemeTurn (Just TransportTCP))

  tlsLoop <-
    runLoopAsync tlsDomain $
      atomicWriteIORef tlsRef . Discovered . fmap (turnURIFromSRV SchemeTurns (Just TransportTCP))
  pure [udpLoop, tcpLoop, tlsLoop]
  where
    withNonZeroWeightRecords :: (Monad m) => (NonEmpty SrvEntry -> m ()) -> NonEmpty SrvEntry -> m ()
    withNonZeroWeightRecords action records =
      case NonEmpty.filter (\e -> srvWeight e /= 0) records of
        [] -> pure ()
        (r : rs) -> action (r :| rs)

    fetchIpsAndPorts :: (Member DNSLookup r) => NonEmpty SrvEntry -> Sem r [(IP.IPv4, Word16)]
    fetchIpsAndPorts =
      fmap catMaybes
        . mapM
          ( \r -> do
              eitherIPs <- lookupA . srvTargetDomain $ srvTarget r
              case eitherIPs of
                Left _err -> pure Nothing
                Right [] -> pure Nothing
                Right (first : _) -> do
                  pure $ Just (first, srvTargetPort $ srvTarget r)
          )
        . NonEmpty.toList

turnURIFromSRV :: Scheme -> Maybe Transport -> SrvEntry -> TurnURI
turnURIFromSRV sch mtr SrvEntry {..} =
  turnURI
    sch
    ( TurnHostName
        . Text.decodeUtf8With lenientDecode
        . stripDot
        $ srvTargetDomain srvTarget
    )
    (Port $ srvTargetPort srvTarget)
    mtr
  where
    stripDot h
      | "." `BS.isSuffixOf` h = BS.take (BS.length h - 1) h
      | otherwise = h

turnURIFromIpAndPort :: IP.IPv4 -> Word16 -> TurnURI
turnURIFromIpAndPort ip port =
  turnURI SchemeTurn (TurnHostIp . IpAddr $ IP.IPv4 ip) (Port port) Nothing

startFileBasedTurnDiscovery :: Log.Logger -> FS.WatchManager -> Opts.TurnServersFiles -> TurnServersRef -> TurnServersRef -> IO ()
startFileBasedTurnDiscovery l w files v1ServersRef v2ServersRef = do
  v1FileCanonicalPath <- canonicalizePath (Opts.tsfServers files)
  v2FileCanonicalPath <- canonicalizePath (Opts.tsfServersV2 files)
  atomicWriteIORef v1ServersRef
    . maybe NotDiscoveredYet Discovered
    =<< readTurnList v1FileCanonicalPath
  atomicWriteIORef v2ServersRef
    . maybe NotDiscoveredYet Discovered
    =<< readTurnList v2FileCanonicalPath
  Log.info l $
    Log.msg (Log.val "Waiting for TURN files")
      . Log.field "v1File" v1FileCanonicalPath
      . Log.field "v2File" v2FileCanonicalPath
  startWatching w v1FileCanonicalPath (replaceTurnServers l v1ServersRef)
  startWatching w v2FileCanonicalPath (replaceTurnServers l v2ServersRef)

replaceTurnServers :: Log.Logger -> IORef (Discovery (NonEmpty TurnURI)) -> FS.Event -> IO ()
replaceTurnServers g ref e = do
  let logErr x = Log.err g (Log.msg $ Log.val "Error loading turn servers: " Log.+++ show x)
  handleAny logErr $
    readTurnList (FS.eventPath e) >>= \case
      Just servers -> do
        atomicWriteIORef ref (Discovered servers)
        Log.info g (Log.msg $ Log.val "New turn servers loaded.")
      Nothing -> Log.warn g (Log.msg $ Log.val "Empty or malformed turn servers list, ignoring!")

startWatching :: FS.WatchManager -> FilePath -> FS.Action -> IO ()
startWatching w p = void . FS.watchDir w (Path.dropFileName p) predicate
  where
    predicate (FS.Added f _ _) = Path.equalFilePath f p
    predicate (FS.Modified f _ _) = Path.equalFilePath f p
    predicate FS.Removed {} = False
    predicate FS.Unknown {} = False
    predicate FS.ModifiedAttributes {} = False
    predicate FS.WatchedDirectoryRemoved {} = False
    predicate FS.CloseWrite {} = False

readTurnList :: FilePath -> IO (Maybe (NonEmpty TurnURI))
readTurnList = Text.readFile >=> pure . fn . mapMaybe (fromByteString . Text.encodeUtf8) . Text.lines
  where
    fn [] = Nothing
    fn (x : xs) = Just (x :| xs)
