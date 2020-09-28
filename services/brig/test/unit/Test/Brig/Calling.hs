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

module Test.Brig.Calling where

import Brig.Calling
import Brig.Options
import Brig.PolyLog
import Control.Retry
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Range
import qualified Data.Set as Set
import Imports
import Network.DNS
import Polysemy
import qualified System.Logger as Log
import Test.Tasty
import Test.Tasty.HUnit
import qualified UnliftIO.Async as Async
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV

data FakeDNSEnv = FakeDNSEnv
  { fakeLookupFn :: Domain -> SrvResponse,
    fakeLookupCalls :: IORef [Domain]
  }

newFakeDNSEnv :: (Domain -> SrvResponse) -> IO FakeDNSEnv
newFakeDNSEnv lookupFn =
  FakeDNSEnv lookupFn <$> newIORef []

runFakeDNSLookup :: Member (Embed IO) r => FakeDNSEnv -> Sem (DNSLookup ': r) a -> Sem r a
runFakeDNSLookup FakeDNSEnv {..} = interpret $ \case
  LookupSRV domain -> do
    modifyIORef' fakeLookupCalls (++ [domain])
    pure $ fakeLookupFn domain

newtype LogRecorder = LogRecorder {recordedLogs :: IORef [(Log.Level, LByteString)]}

newLogRecorder :: IO LogRecorder
newLogRecorder = LogRecorder <$> newIORef []

recordLogs :: Member (Embed IO) r => LogRecorder -> Sem (PolyLog ': r) a -> Sem r a
recordLogs LogRecorder {..} = interpret $ \(PolyLog lvl msg) ->
  modifyIORef' recordedLogs (++ [(lvl, Log.render (Log.renderDefault ", ") msg)])

ignoreLogs :: Sem (PolyLog ': r) a -> Sem r a
ignoreLogs = interpret $ \(PolyLog _ _) -> pure ()

{-# ANN tests ("HLint: ignore" :: String) #-}
tests :: TestTree
tests =
  testGroup "Calling" $
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
      testGroup "getRandomSFTServers" $
        [ testCase "more servers in SRV than limit" testSFTManyServers,
          testCase "fewer servers in SRV than limit" testSFTFewerServers
          -- the randomization part is not (yet?) tested here.
        ]
    ]

testDiscoveryLoopWhenSuccessful :: IO SFTEnv
testDiscoveryLoopWhenSuccessful = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      returnedEntries = entry1 :| [entry2, entry3]
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)
  sftEnv <- mkSFTEnv (SFTOptions "foo.example.com" Nothing (Just 0.001) Nothing)

  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEvery10MicrosWhileN 2000 (== 0) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
  -- We don't want to stop the loop before it has written to the sftServers IORef
  void $ retryEvery10MicrosWhileN 2000 (== NotDiscoveredYet) (readIORef (sftServers sftEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" (Discovered (mkSFTServers returnedEntries)) actualServers
  pure sftEnv

testDiscoveryLoopWhenUnsuccessful :: IO ()
testDiscoveryLoopWhenUnsuccessful = do
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvNotAvailable)
  sftEnv <- mkSFTEnv (SFTOptions "foo.example.com" Nothing (Just 0.001) Nothing)

  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEvery10MicrosWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" NotDiscoveredYet actualServers

testDiscoveryLoopWhenUnsuccessfulAfterSuccess :: IO ()
testDiscoveryLoopWhenUnsuccessfulAfterSuccess = do
  sftEnv <- testDiscoveryLoopWhenSuccessful
  previousEntries <- readIORef (sftServers sftEnv)

  -- In the following lines we re-use the 'sftEnv' from a successful lookup to
  -- replicate what will happen when a dns lookup fails after success
  failingFakeDNSEnv <- newFakeDNSEnv (\_ -> SrvNotAvailable)
  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup failingFakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEvery10MicrosWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupCalls failingFakeDNSEnv))
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
      newEntries = (entry1 :| [entry2])

  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable newEntries)
  discoveryLoop <- Async.async $ runM . ignoreLogs . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEvery10MicrosWhileN 2000 (== 0) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
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
      returnedEntries = (entry1 :| [entry2])
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
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvNotAvailable)

  assertEqual "discovered servers should be returned" Nothing
    =<< ( runM . recordLogs logRecorder . runFakeDNSLookup fakeDNSEnv $
            discoverSFTServers "_sft._tcp.foo.example.com"
        )
  assertEqual "should warn about it in the logs" [(Log.Warn, "No SFT servers available\n")]
    =<< readIORef (recordedLogs logRecorder)

testSFTDiscoverWhenDNSFails :: IO ()
testSFTDiscoverWhenDNSFails = do
  logRecorder <- newLogRecorder
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvResponseError IllegalDomain)

  assertEqual "discovered servers should be returned" Nothing
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
  someServers <- getRandomSFTServers (unsafeRange 3) sftServers
  assertEqual "should return only 3 servers" 3 (length someServers)

testSFTFewerServers :: IO ()
testSFTFewerServers = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      entry4 = SrvEntry 0 0 (SrvTarget "sft4.foo.example.com." 443)
      entries = entry1 :| [entry2, entry3, entry4]
      sftServers = mkSFTServers entries

  allServers <- getRandomSFTServers (unsafeRange 10) sftServers
  assertEqual "should return all of them" (Set.fromList $ NonEmpty.toList allServers) (Set.fromList $ NonEmpty.toList entries)

retryEvery10MicrosWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryEvery10MicrosWhileN n f m =
  retrying
    (constantDelay 10 <> limitRetries n)
    (const (return . f))
    (const m)
