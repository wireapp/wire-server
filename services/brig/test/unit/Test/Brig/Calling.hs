{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Brig.Calling (tests) where

import Brig.Calling
import Brig.Calling.API
import Brig.Calling.Internal
import Brig.Effects.SFT
import Brig.Options
import Control.Concurrent.Timeout qualified as System
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Misc
import Data.Range
import Data.Set qualified as Set
import Data.Timeout
import Imports
import Network.DNS
import OpenSSL
import OpenSSL.EVP.Digest
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import Test.Brig.Effects.Delay
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import URI.ByteString
import UnliftIO.Async qualified as Async
import Wire.API.Call.Config
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV
import Wire.Sem.Logger.Level
import Wire.Sem.Logger.TinyLog

data FakeDNSEnv = FakeDNSEnv
  { fakeLookupSrv :: Domain -> SrvResponse,
    fakeLookupSrvCalls :: IORef [Domain]
  }

newFakeDNSEnv :: (Domain -> SrvResponse) -> IO FakeDNSEnv
newFakeDNSEnv lookupSrvFn =
  FakeDNSEnv lookupSrvFn <$> newIORef []

runFakeDNSLookup :: (Member (Embed IO) r) => FakeDNSEnv -> Sem (DNSLookup ': r) a -> Sem r a
runFakeDNSLookup FakeDNSEnv {..} = interpret $
  \(LookupSRV domain) -> do
    modifyIORef' fakeLookupSrvCalls (++ [domain])
    pure $ fakeLookupSrv domain

ignoreLogs :: Sem (TinyLog ': r) a -> Sem r a
ignoreLogs = discardTinyLogs

{-# ANN tests ("HLint: ignore" :: String) #-}
tests :: TestTree
tests =
  testGroup
    "Calling"
    [ testGroup "mkSFTDomain" $
        [ testCase "when service name is provided" $
            assertEqual
              "should use the service name to form domain"
              "_foo._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" (Just "foo") Nothing Nothing Nothing)),
          testCase "when service name is not provided" $
            assertEqual
              "should assume service name to be 'sft'"
              "_sft._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" Nothing Nothing Nothing Nothing))
        ],
      testGroup "sftDiscoveryLoop" $
        [ testCase "when service can be discovered" $ void testSFTDiscoveryLoopWhenSuccessful
        ],
      testGroup "srvDiscoveryLoop" $
        [ testCase "when service can be discovered" $ testSRVDiscoveryLoopWhenSuccessful,
          testCase "when service cannot be discovered" $ testSRVDiscoveryLoopWhenUnsuccessful
        ],
      testGroup "discoverSRVRecords" $
        [ testCase "when service is available" testSRVDiscoverWhenAvailable,
          testCase "when service is not available" testSRVDiscoverWhenNotAvailable,
          testCase "when dns lookup fails" testSRVDiscoverWhenDNSFails
        ],
      testGroup "Get Random SFT Servers" $
        [ testCase "more servers in SRV than limit" testSFTManyServers,
          testCase "fewer servers in SRV than limit" testSFTFewerServers
          -- the randomization part is not (yet?) tested here.
        ],
      testGroup
        "SFT static URL"
        [ testCase "deprecated endpoint" testSFTStaticDeprecatedEndpoint,
          testCase "v2 endpoint, no SFT static URL" testSFTStaticV2NoStaticUrl,
          testCase "v2 endpoint, SFT static URL without /sft_servers_all.json" testSFTStaticV2StaticUrlError,
          testCase "v2 endpoint, SFT static URL with    /sft_servers_all.json" testSFTStaticV2StaticUrlList,
          testCase "v2 endpoint, SFT static URL with with setSftListAllServers \"disabeld\"" testSFTStaticV2ListAllServersDisabled
        ]
    ]

testSFTDiscoveryLoopWhenSuccessful :: IO SFTEnv
testSFTDiscoveryLoopWhenSuccessful = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      returnedEntries = entry1 :| [entry2, entry3]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)
  let intervalInSeconds = 0.001
      intervalInMicroseconds = 1000
  Just sha512 <- getDigestByName "SHA512"
  sftEnv <- mkSFTEnv sha512 $ SFTOptions "foo.example.com" Nothing (Just intervalInSeconds) Nothing Nothing

  tick <- newEmptyMVar
  delayCallsTVar <- newTVarIO []
  discoveryLoop <-
    Async.async
      . runM
      . ignoreLogs
      . runDelayWithTick tick delayCallsTVar
      . runFakeDNSLookup fakeDNSEnv
      $ sftDiscoveryLoop sftEnv

  Async.race_ (Async.wait discoveryLoop) (System.timeout (30 # Second) $ takeMVar tick)

  actualServers <- readIORef (sftServers sftEnv)
  delayCalls <- readTVarIO delayCallsTVar
  assertEqual "servers should be the ones read from DNS" (Discovered (mkSFTServers returnedEntries)) actualServers
  assertEqual "delay should be called with the configured interval" intervalInMicroseconds (head delayCalls)
  pure sftEnv

testSRVDiscoveryLoopWhenSuccessful :: IO ()
testSRVDiscoveryLoopWhenSuccessful = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      returnedEntries = entry1 :| [entry2, entry3]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)
  let intervalInMicroseconds = 1000

  tick <- newEmptyMVar
  delayCallsTVar <- newTVarIO []
  savedSrvRecordsMVar <- newEmptyMVar
  discoveryLoop <-
    Async.async
      . runM
      . ignoreLogs
      . runDelayWithTick tick delayCallsTVar
      . runFakeDNSLookup fakeDNSEnv
      $ srvDiscoveryLoop "foo.example.com" intervalInMicroseconds (putMVar savedSrvRecordsMVar)

  Async.race_ (Async.wait discoveryLoop) (System.timeout (30 # Second) $ takeMVar tick)

  savedSrvRecords <- tryReadMVar savedSrvRecordsMVar
  delayCalls <- readTVarIO delayCallsTVar
  assertEqual "servers should be the ones read from DNS" (Just returnedEntries) savedSrvRecords
  assertEqual "delay should be called with the configured interval" intervalInMicroseconds (head delayCalls)

testSRVDiscoveryLoopWhenUnsuccessful :: IO ()
testSRVDiscoveryLoopWhenUnsuccessful = do
  fakeDNSEnv <- newFakeDNSEnv (const SrvNotAvailable)

  -- Irrelevant for this test, but types make us choose a number
  let intervalInMicroseconds = 1000
  tick <- newEmptyMVar
  delayCallsTVar <- newTVarIO []
  discoveryLoop <-
    Async.async
      . runM
      . ignoreLogs
      . runDelayWithTick tick delayCallsTVar
      . runFakeDNSLookup fakeDNSEnv
      . srvDiscoveryLoop "foo.example.com" intervalInMicroseconds
      $ ( \_ ->
            liftIO $ assertFailure "shouldn't try to save SRV records when they are unavailable"
        )
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once this tests that the loop keeps going even when discovery fails
  void . System.timeout (30 # Second) $ takeMVar tick
  Async.race_ (Async.wait discoveryLoop) (System.timeout (30 # Second) $ takeMVar tick)

  numberOfDealys <- length <$> readTVarIO delayCallsTVar
  assertBool "discovery loop should run again even if discovery fails" (numberOfDealys >= 2)

testSRVDiscoverWhenAvailable :: IO ()
testSRVDiscoverWhenAvailable = do
  logRecorder <- newLogRecorder
  let entry1 = SrvEntry 0 0 (SrvTarget "sft7.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft8.foo.example.com." 8843)
      returnedEntries = entry1 :| [entry2]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)

  assertEqual "discovered servers should be returned" (Just returnedEntries)
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSRVRecords "_sft._tcp.foo.example.com"
        )
  assertEqual "nothing should be logged" []
    =<< readIORef (recordedLogs logRecorder)

testSRVDiscoverWhenNotAvailable :: IO ()
testSRVDiscoverWhenNotAvailable = do
  logRecorder <- newLogRecorder
  fakeDNSEnv <- newFakeDNSEnv (const SrvNotAvailable)

  assertEqual "discovered servers should be returned" Nothing
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSRVRecords "_sft._tcp.foo.example.com"
        )
  assertEqual "should warn about it in the logs" [(Warn, "SRV Records not available, domain=_sft._tcp.foo.example.com\n")]
    =<< readIORef (recordedLogs logRecorder)

testSRVDiscoverWhenDNSFails :: IO ()
testSRVDiscoverWhenDNSFails = do
  logRecorder <- newLogRecorder
  fakeDNSEnv <- newFakeDNSEnv (const $ SrvResponseError IllegalDomain)

  assertEqual "no servers should be returned" Nothing
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSRVRecords "_sft._tcp.foo.example.com"
        )
  assertEqual "should warn about it in the logs" [(Error, "SRV Lookup failed, Error=IllegalDomain, domain=_sft._tcp.foo.example.com\n")]
    =<< readIORef (recordedLogs logRecorder)

testSFTManyServers :: IO ()
testSFTManyServers = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      entry4 = SrvEntry 0 0 (SrvTarget "sft4.foo.example.com." 443)
      entry5 = SrvEntry 0 0 (SrvTarget "sft5.foo.example.com." 443)
      entry6 = SrvEntry 0 0 (SrvTarget "sft6.foo.example.com." 443)
      entry7 = SrvEntry 0 0 (SrvTarget "sft7.foo.example.com." 443)
      entries = entry1 :| [entry2, entry3, entry4, entry5, entry6, entry7]
      sftServers = mkSFTServers entries
  someServers <- getRandomElements (unsafeRange 3) . unSFTServers $ sftServers
  assertEqual "should return only 3 servers" 3 (length someServers)

testSFTFewerServers :: IO ()
testSFTFewerServers = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      entry4 = SrvEntry 0 0 (SrvTarget "sft4.foo.example.com." 443)
      entries = entry1 :| [entry2, entry3, entry4]
      sftServers = mkSFTServers entries

  allServers <- getRandomElements (unsafeRange 10) . unSFTServers $ sftServers
  assertEqual "should return all of them" (Set.fromList $ NonEmpty.toList allServers) (Set.fromList $ NonEmpty.toList entries)

-- | Creates a calling environment and an https URL to be used in unit-testing
-- the logic of call configuration endpoints
sftStaticEnv :: IO (TurnEnv, HttpsUrl)
sftStaticEnv = withOpenSSL $ do
  turnUri <- generate arbitrary
  let tokenTtl = 10 -- seconds
      configTtl = 10 -- seconds
      secret = "secret word"
      turnSource = TurnSourceFiles (TurnServersFiles "test/resources/turn/servers.txt" "test/resources/turn/servers-v2.txt")
  Just sha512 <- getDigestByName "SHA512"
  env <- mkTurnEnv turnSource tokenTtl configTtl secret sha512
  let TurnServersFromFiles _ serversV1IORef serversV2IORef = env ^. turnServers
  atomicWriteIORef serversV1IORef (Discovered turnUri)
  atomicWriteIORef serversV2IORef (Discovered turnUri)
  let Right staticUrl =
        mkHttpsUrl
          =<< first
            show
            (parseURI laxURIParserOptions "https://sft01.integration-tests.zinfra.io:443")
  pure (env, staticUrl)

-- The deprecated endpoint `GET /calls/config` without an SFT static URL
testSFTStaticDeprecatedEndpoint :: IO ()
testSFTStaticDeprecatedEndpoint = do
  env <- fst <$> sftStaticEnv
  turnUri <- generate arbitrary
  uid <- generate arbitrary
  cfg <-
    runM @IO
      . ignoreLogs
      . interpretSFTInMemory mempty
      . throwErrorInIO @_ @NoTurnServers
      $ newConfig uid env (Discovered turnUri) Nothing Nothing Nothing HideAllSFTServers CallsConfigDeprecated True
  assertEqual
    "when SFT static URL is disabled, sft_servers should be empty."
    Set.empty
    (Set.fromList $ maybe [] NonEmpty.toList $ cfg ^. rtcConfSftServers)

-- The v2 endpoint `GET /calls/config/v2` without an SFT static URL
testSFTStaticV2NoStaticUrl :: IO ()
testSFTStaticV2NoStaticUrl = do
  uid <- generate arbitrary
  env <- fst <$> sftStaticEnv
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      servers = entry1 :| [entry2, entry3]
  sftEnv <-
    SFTEnv
      <$> newIORef (Discovered . mkSFTServers $ servers)
      <*> pure "foo.example.com"
      <*> pure 5
      <*> pure (unsafeRange 1)
      <*> pure Nothing
  turnUri <- generate arbitrary
  cfg <-
    runM @IO
      . ignoreLogs
      . interpretSFTInMemory mempty
      . throwErrorInIO @_ @NoTurnServers
      $ newConfig uid env (Discovered turnUri) Nothing (Just sftEnv) (Just . unsafeRange $ 2) ListAllSFTServers (CallsConfigV2 Nothing) True
  assertEqual
    "when SFT static URL is disabled, sft_servers_all should be from SFT environment"
    (Just . fmap ((^. sftURL) . sftServerFromSrvTarget . srvTarget) . toList $ servers)
    ((^. authURL) <$$> cfg ^. rtcConfSftServersAll)

-- The v2 endpoint `GET /calls/config/v2` with an SFT static URL that gives an error
testSFTStaticV2StaticUrlError :: IO ()
testSFTStaticV2StaticUrlError = do
  (env, staticUrl) <- sftStaticEnv
  turnUri <- generate arbitrary
  uid <- generate arbitrary
  cfg <-
    runM @IO
      . ignoreLogs
      . interpretSFTInMemory mempty -- an empty lookup map, meaning there was an error
      . throwErrorInIO @_ @NoTurnServers
      $ newConfig uid env (Discovered turnUri) (Just staticUrl) Nothing (Just . unsafeRange $ 2) ListAllSFTServers (CallsConfigV2 Nothing) True
  assertEqual
    "when SFT static URL is enabled (and setSftListAllServers is enabled), but returns error, sft_servers_all should be omitted"
    Nothing
    (cfg ^. rtcConfSftServersAll)

-- The v2 endpoint `GET /calls/config/v2` with an SFT static URL's /sft_servers_all.json
testSFTStaticV2StaticUrlList :: IO ()
testSFTStaticV2StaticUrlList = do
  (env, staticUrl) <- sftStaticEnv
  -- 10 servers compared to the limit of 3 below that should be disregarded
  -- for sft_servers_all
  servers <- generate $ replicateM 10 arbitrary
  turnUri <- generate arbitrary
  uid <- generate arbitrary
  cfg <-
    runM @IO
      . ignoreLogs
      . interpretSFTInMemory (Map.singleton staticUrl (SFTGetResponse $ Right servers))
      . throwErrorInIO @_ @NoTurnServers
      $ newConfig uid env (Discovered turnUri) (Just staticUrl) Nothing (Just . unsafeRange $ 3) ListAllSFTServers (CallsConfigV2 Nothing) True
  assertEqual
    "when SFT static URL and setSftListAllServers are enabled, sft_servers_all should be from /sft_servers_all.json"
    ((^. sftURL) <$$> Just servers)
    ((^. authURL) <$$> cfg ^. rtcConfSftServersAll)

testSFTStaticV2ListAllServersDisabled :: IO ()
testSFTStaticV2ListAllServersDisabled = do
  (env, staticUrl) <- sftStaticEnv
  -- 10 servers compared to the limit of 3 below that should be disregarded
  -- for sft_servers_all
  servers <- generate $ replicateM 10 arbitrary
  turnUri <- generate arbitrary
  uid <- generate arbitrary
  cfg <-
    runM @IO
      . ignoreLogs
      . interpretSFTInMemory (Map.singleton staticUrl (SFTGetResponse . Right $ servers))
      . throwErrorInIO @_ @NoTurnServers
      $ newConfig uid env (Discovered turnUri) (Just staticUrl) Nothing (Just . unsafeRange $ 3) HideAllSFTServers (CallsConfigV2 Nothing) True
  assertEqual
    "when SFT static URL is enabled and setSftListAllServers is \"disabled\" then sft_servers_all is missing"
    Nothing
    (cfg ^. rtcConfSftServersAll)

throwErrorInIO :: (Member (Embed IO) r, Exception e) => Sem (Error e ': r) a -> Sem r a
throwErrorInIO action = do
  eitherResult <- runError action
  case eitherResult of
    Right x -> pure x
    Left e -> liftIO $ throwM e
