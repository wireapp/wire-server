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

module Test.Brig.Calling where

import Brig.Calling
import Brig.Calling.API (CallsConfigVersion (..), newConfig)
import Brig.Calling.Internal (sftServerFromSrvTarget)
import Brig.Effects.SFT
import Brig.Options
import Control.Lens ((^.))
import Control.Retry
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Misc
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions
import Imports
import Network.DNS
import Polysemy
import Polysemy.TinyLog
import qualified System.Logger as Log
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Arbitrary (..), generate)
import URI.ByteString
import qualified UnliftIO.Async as Async
import Wire.API.Call.Config
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV

data FakeDNSEnv = FakeDNSEnv
  { fakeLookupSrv :: Domain -> SrvResponse,
    fakeLookupSrvCalls :: IORef [Domain]
  }

newFakeDNSEnv :: (Domain -> SrvResponse) -> IO FakeDNSEnv
newFakeDNSEnv lookupSrvFn =
  FakeDNSEnv lookupSrvFn <$> newIORef []

runFakeDNSLookup :: Member (Embed IO) r => FakeDNSEnv -> Sem (DNSLookup ': r) a -> Sem r a
runFakeDNSLookup FakeDNSEnv {..} = interpret $
  \(LookupSRV domain) -> do
    modifyIORef' fakeLookupSrvCalls (++ [domain])
    pure $ fakeLookupSrv domain

newtype LogRecorder = LogRecorder {recordedLogs :: IORef [(Log.Level, LByteString)]}

newLogRecorder :: IO LogRecorder
newLogRecorder = LogRecorder <$> newIORef []

recordLogs :: Member (Embed IO) r => LogRecorder -> Sem (TinyLog ': r) a -> Sem r a
recordLogs LogRecorder {..} = interpret $ \(Polylog lvl msg) ->
  modifyIORef' recordedLogs (++ [(lvl, Log.render (Log.renderDefault ", ") msg)])

ignoreLogs :: Sem (TinyLog ': r) a -> Sem r a
ignoreLogs = interpret $ \(Polylog _ _) -> pure ()

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
              (mkSFTDomain (SFTOptions "example.com" (Just "foo") Nothing Nothing)),
          testCase "when service name is not provided" $
            assertEqual
              "should assume service name to be 'sft'"
              "_sft._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" Nothing Nothing Nothing))
        ],
      testGroup "sftDiscoveryLoop" $
        [ testCase "when service can be discovered" $ void testDiscoveryLoopWhenSuccessful,
          testCase "when service can be discovered and the URLs change" testDiscoveryLoopWhenURLsChange,
          testCase "when service cannot be discovered" testDiscoveryLoopWhenUnsuccessful,
          testCase "when service cannot be discovered after a successful discovery" testDiscoveryLoopWhenUnsuccessfulAfterSuccess
        ],
      testGroup "discoverSFTServers" $
        [ testCase "when service is available" testSFTDiscoverWhenAvailable,
          testCase "when service is not available" testSFTDiscoverWhenNotAvailable,
          testCase "when dns lookup fails" testSFTDiscoverWhenDNSFails
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

testDiscoveryLoopWhenSuccessful :: IO SFTEnv
testDiscoveryLoopWhenSuccessful = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      returnedEntries = entry1 :| [entry2, entry3]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)
  sftEnv <- mkSFTEnv $ SFTOptions "foo.example.com" Nothing (Just 0.001) Nothing

  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEvery10MicrosWhileN 2000 (== 0) (length <$> readIORef (fakeLookupSrvCalls fakeDNSEnv))
  -- We don't want to stop the loop before it has written to the sftServers IORef
  void $ retryEvery10MicrosWhileN 2000 (== NotDiscoveredYet) (readIORef (sftServers sftEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" (Discovered (mkSFTServers returnedEntries)) actualServers
  pure sftEnv

testDiscoveryLoopWhenUnsuccessful :: IO ()
testDiscoveryLoopWhenUnsuccessful = do
  fakeDNSEnv <- newFakeDNSEnv (const SrvNotAvailable)
  sftEnv <- mkSFTEnv $ SFTOptions "foo.example.com" Nothing (Just 0.001) Nothing

  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEvery10MicrosWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupSrvCalls fakeDNSEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" NotDiscoveredYet actualServers

testDiscoveryLoopWhenUnsuccessfulAfterSuccess :: IO ()
testDiscoveryLoopWhenUnsuccessfulAfterSuccess = do
  sftEnv <- testDiscoveryLoopWhenSuccessful
  previousEntries <- readIORef (sftServers sftEnv)

  -- In the following lines we re-use the 'sftEnv' from a successful lookup to
  -- replicate what will happen when a dns lookup fails after success
  failingFakeDNSEnv <- newFakeDNSEnv (const SrvNotAvailable)
  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup failingFakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEvery10MicrosWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupSrvCalls failingFakeDNSEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers shouldn't get overwriten" previousEntries actualServers

testDiscoveryLoopWhenURLsChange :: IO ()
testDiscoveryLoopWhenURLsChange = do
  sftEnv <- testDiscoveryLoopWhenSuccessful

  -- In the following lines we re-use the 'sftEnv' from a successful lookup to
  -- replicate what will happen when a dns lookup returns new URLs
  let entry1 = SrvEntry 0 0 (SrvTarget "sft4.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft5.foo.example.com." 443)
      newEntries = entry1 :| [entry2]

  fakeDNSEnv <- newFakeDNSEnv (const $ SrvAvailable newEntries)
  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEvery10MicrosWhileN 2000 (== 0) (length <$> readIORef (fakeLookupSrvCalls fakeDNSEnv))
  -- We don't want to stop the loop before it has written to the sftServers IORef
  void $ retryEvery10MicrosWhileN 2000 (== Discovered (mkSFTServers newEntries)) (readIORef (sftServers sftEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should get overwritten" (Discovered (mkSFTServers newEntries)) actualServers

testSFTDiscoverWhenAvailable :: IO ()
testSFTDiscoverWhenAvailable = do
  logRecorder <- newLogRecorder
  let entry1 = SrvEntry 0 0 (SrvTarget "sft7.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft8.foo.example.com." 8843)
      returnedEntries = entry1 :| [entry2]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)

  assertEqual "discovered servers should be returned" (Just returnedEntries)
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSFTServers "_sft._tcp.foo.example.com"
        )
  assertEqual "nothing should be logged" []
    =<< readIORef (recordedLogs logRecorder)

testSFTDiscoverWhenNotAvailable :: IO ()
testSFTDiscoverWhenNotAvailable = do
  logRecorder <- newLogRecorder
  fakeDNSEnv <- newFakeDNSEnv (const SrvNotAvailable)

  assertEqual "discovered servers should be returned" Nothing
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSFTServers "_sft._tcp.foo.example.com"
        )
  assertEqual "should warn about it in the logs" [(Log.Warn, "No SFT servers available\n")]
    =<< readIORef (recordedLogs logRecorder)

testSFTDiscoverWhenDNSFails :: IO ()
testSFTDiscoverWhenDNSFails = do
  logRecorder <- newLogRecorder
  fakeDNSEnv <- newFakeDNSEnv (const $ SrvResponseError IllegalDomain)

  assertEqual "no servers should be returned" Nothing
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSFTServers "_sft._tcp.foo.example.com"
        )
  assertEqual "should warn about it in the logs" [(Log.Error, "DNS Lookup failed for SFT Discovery, Error=IllegalDomain\n")]
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

retryEvery10MicrosWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryEvery10MicrosWhileN n f m =
  retrying
    (constantDelay 10 <> limitRetries n)
    (const (return . f))
    (const m)

-- | Creates a calling environment and an https URL to be used in unit-testing
-- the logic of call configuration endpoints
sftStaticEnv :: IO (Env, HttpsUrl)
sftStaticEnv = do
  turnUri <- generate arbitrary
  let tokenTtl = 10 -- seconds
      configTtl = 10 -- seconds
      secret = "secret word"
  env <- newEnv undefined (pure turnUri) tokenTtl configTtl secret
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
  cfg <-
    runM @IO
      . discardLogs
      . interpretSFTInMemory mempty
      $ newConfig env Nothing Nothing Nothing HideAllSFTServers CallsConfigDeprecated
  assertEqual
    "when SFT static URL is disabled, sft_servers should be empty."
    Set.empty
    (Set.fromList $ maybe [] NonEmpty.toList $ cfg ^. rtcConfSftServers)

-- The v2 endpoint `GET /calls/config/v2` without an SFT static URL
testSFTStaticV2NoStaticUrl :: IO ()
testSFTStaticV2NoStaticUrl = do
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
  cfg <-
    runM @IO
      . discardLogs
      . interpretSFTInMemory mempty
      $ newConfig env Nothing (Just sftEnv) (Just . unsafeRange $ 2) ListAllSFTServers CallsConfigV2
  assertEqual
    "when SFT static URL is disabled, sft_servers_all should be from SFT environment"
    (Just . fmap (sftServerFromSrvTarget . srvTarget) . toList $ servers)
    (cfg ^. rtcConfSftServersAll)

-- The v2 endpoint `GET /calls/config/v2` with an SFT static URL that gives an error
testSFTStaticV2StaticUrlError :: IO ()
testSFTStaticV2StaticUrlError = do
  (env, staticUrl) <- sftStaticEnv
  cfg <-
    runM @IO
      . discardLogs
      . interpretSFTInMemory mempty -- an empty lookup map, meaning there was
      -- an error
      $ newConfig env (Just staticUrl) Nothing (Just . unsafeRange $ 2) ListAllSFTServers CallsConfigV2
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
  cfg <-
    runM @IO
      . discardLogs
      . interpretSFTInMemory (Map.singleton staticUrl (SFTGetResponse . Right $ servers))
      $ newConfig env (Just staticUrl) Nothing (Just . unsafeRange $ 3) ListAllSFTServers CallsConfigV2
  assertEqual
    "when SFT static URL and setSftListAllServers are enabled, sft_servers_all should be from /sft_servers_all.json"
    (Just servers)
    (cfg ^. rtcConfSftServersAll)

testSFTStaticV2ListAllServersDisabled :: IO ()
testSFTStaticV2ListAllServersDisabled = do
  (env, staticUrl) <- sftStaticEnv
  -- 10 servers compared to the limit of 3 below that should be disregarded
  -- for sft_servers_all
  servers <- generate $ replicateM 10 arbitrary
  cfg <-
    runM @IO
      . discardLogs
      . interpretSFTInMemory (Map.singleton staticUrl (SFTGetResponse . Right $ servers))
      $ newConfig env (Just staticUrl) Nothing (Just . unsafeRange $ 3) HideAllSFTServers CallsConfigV2
  assertEqual
    "when SFT static URL is enabled and setSftListAllServers is \"disabled\" then sft_servers_all is missing"
    Nothing
    (cfg ^. rtcConfSftServersAll)
