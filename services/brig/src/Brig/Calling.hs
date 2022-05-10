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
    Discovery (..),
    TurnEnv,
    mkSFTEnv,
    mkTurnEnv,
    sftDiscoveryLoop,
    discoverSRVRecords,
    discoveryToMaybe,
    randomize,
    startSFTServiceDiscovery,
    startTurnDiscovery,
    turnServersV1,
    turnServersV1File,
    turnServersV2,
    turnServersV2File,
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
import Control.Exception.Enclosed (handleAny)
import Control.Lens
import Control.Monad.Random.Class (MonadRandom)
import Data.ByteString.Conversion (fromByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Range
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Imports
import qualified Network.DNS as DNS
import OpenSSL.EVP.Digest (Digest)
import Polysemy
import Polysemy.TinyLog
import qualified System.FSNotify as FS
import qualified System.FilePath as Path
import qualified System.Logger as Log
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.Shuffle
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV
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
  MonadRandom f =>
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

discoverSRVRecords :: Members [DNSLookup, TinyLog] r => DNS.Domain -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSRVRecords domain =
  lookupSRV domain >>= \case
    SrvAvailable es -> pure $ Just es
    SrvNotAvailable -> do
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

srvDiscoveryLoop :: Members [DNSLookup, TinyLog, Embed IO] r => DNS.Domain -> Int -> (NonEmpty SrvEntry -> IO ()) -> Sem r ()
srvDiscoveryLoop domain discoveryInterval saveAction = forever $ do
  servers <- discoverSRVRecords domain
  case servers of
    Nothing -> pure ()
    Just es -> liftIO $ saveAction es
  threadDelay discoveryInterval

mkSFTDomain :: SFTOptions -> DNS.Domain
mkSFTDomain SFTOptions {..} = DNS.normalize $ maybe defSftServiceName ("_" <>) sftSRVServiceName <> "._tcp." <> sftBaseDomain

sftDiscoveryLoop :: Members [DNSLookup, TinyLog, Embed IO] r => SFTEnv -> Sem r ()
sftDiscoveryLoop SFTEnv {..} =
  srvDiscoveryLoop sftDomain sftDiscoveryInterval $
    atomicWriteIORef sftServers . Discovered . SFTServers

mkSFTEnv :: SFTOptions -> IO SFTEnv
mkSFTEnv opts =
  SFTEnv
    <$> newIORef NotDiscoveredYet
    <*> pure (mkSFTDomain opts)
    <*> pure (diffTimeToMicroseconds (fromMaybe defSftDiscoveryIntervalSeconds (Opts.sftDiscoveryIntervalSeconds opts)))
    <*> pure (fromMaybe defSftListLength (Opts.sftListLength opts))

startSFTServiceDiscovery :: Log.Logger -> SFTEnv -> IO ()
startSFTServiceDiscovery logger =
  runM . loggerToTinyLog logger . runDNSLookupDefault . sftDiscoveryLoop

-- | >>> diffTimeToMicroseconds 1
-- 1000000
diffTimeToMicroseconds :: DiffTime -> Int
diffTimeToMicroseconds = fromIntegral . (`quot` 1000000) . diffTimeToPicoseconds

-- TURN specific

data TurnEnv = TurnEnv
  { _turnServersV1 :: IORef (Discovery (NonEmpty TurnURI)),
    _turnServersV2 :: IORef (Discovery (NonEmpty TurnURI)),
    _turnServersV1File :: FilePath,
    _turnServersV2File :: FilePath,
    _turnTokenTTL :: Word32,
    _turnConfigTTL :: Word32,
    _turnSecret :: ByteString,
    _turnSHA512 :: Digest,
    _turnPrng :: GenIO
  }

makeLenses ''TurnEnv

mkTurnEnv :: FilePath -> FilePath -> Word32 -> Word32 -> ByteString -> Digest -> IO TurnEnv
mkTurnEnv v1File v2File _turnTokenTTL _turnConfigTTL _turnSecret _turnSHA512 = do
  _turnServersV1 <- newIORef NotDiscoveredYet
  _turnServersV2 <- newIORef NotDiscoveredYet
  _turnPrng <- createSystemRandom
  _turnServersV1File <- canonicalizePath v1File
  _turnServersV2File <- canonicalizePath v2File
  pure $ TurnEnv {..}

-- | Returns an action which can be executed to stop this
startTurnDiscovery :: Log.Logger -> FS.WatchManager -> TurnEnv -> IO ()
startTurnDiscovery l w e = do
  atomicWriteIORef (e ^. turnServersV1)
    . maybe NotDiscoveredYet Discovered
    =<< readTurnList (e ^. turnServersV1File)
  atomicWriteIORef (e ^. turnServersV2)
    . maybe NotDiscoveredYet Discovered
    =<< readTurnList (e ^. turnServersV2File)
  Log.warn l $
    Log.msg (Log.val "Waiting for TURN files")
      . Log.field "file1" (e ^. turnServersV1File)
      . Log.field "file2" (e ^. turnServersV2File)
  startWatching w (e ^. turnServersV1File) (replaceTurnServers l (e ^. turnServersV1))
  startWatching w (e ^. turnServersV2File) (replaceTurnServers l (e ^. turnServersV2))

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

readTurnList :: FilePath -> IO (Maybe (NonEmpty TurnURI))
readTurnList = Text.readFile >=> return . fn . mapMaybe (fromByteString . Text.encodeUtf8) . Text.lines
  where
    fn [] = Nothing
    fn (x : xs) = Just (x :| xs)
