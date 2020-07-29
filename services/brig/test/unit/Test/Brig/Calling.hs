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
import Control.Retry
import Data.List.NonEmpty (NonEmpty (..))
import Imports
import Network.DNS
import Polysemy
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
newFakeDNSEnv lookupFn = do
  FakeDNSEnv lookupFn <$> newIORef []

runFakeDNSLookup :: Member (Embed IO) r => FakeDNSEnv -> Sem (DNSLookup ': r) a -> Sem r a
runFakeDNSLookup FakeDNSEnv {..} = interpret $ \case
  LookupSRV domain -> do
    modifyIORef' fakeLookupCalls (++ [domain])
    pure $ fakeLookupFn domain

tests :: TestTree
tests =
  testGroup "Calling" $
    [ testGroup "mkSFTDomain" $
        [ testCase "when service name is provided" $
            assertEqual
              "should use the service name to form domain"
              "_foo._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" (Just "foo"))),
          testCase "when service name is not provided" $
            assertEqual
              "should assume service name to be 'sft'"
              "_sft._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" Nothing))
        ],
      testGroup "sftDiscoveryLoop" $
        [ testCase "when service can be discovered" $ void testDiscoveryWhenSuccessful,
          testCase "when service can be discovered and the URLs change" testDiscoveryWhenURLsChange,
          testCase "when service cannot be discovered" testDiscoveryWhenUnsuccessful,
          testCase "when service cannot be discovered after a successful discovery" testDiscoveryWhenUnsuccessfulAfterSuccess
        ]
    ]

testDiscoveryWhenSuccessful :: IO SFTEnv
testDiscoveryWhenSuccessful = do
  let entry1 = SrvEntry 0 0 (SrvTarget "sft1.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft2.foo.example.com." 443)
      entry3 = SrvEntry 0 0 (SrvTarget "sft3.foo.example.com." 443)
      returnedEntries = (entry1 :| [entry2, entry3])
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable returnedEntries)
  sftEnv <- mkSFTEnv (SFTOptions "foo.example.com" Nothing)

  discoveryLoop <- Async.async $ runM . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEveryMillisecondWhileN 2000 (== 0) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
  -- We don't want to stop the loop before it has written to the sftServers IORef
  void $ retryEveryMillisecondWhileN 2000 (isNothing) (readIORef (sftServers sftEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" (Just returnedEntries) actualServers
  pure sftEnv

testDiscoveryWhenUnsuccessful :: IO ()
testDiscoveryWhenUnsuccessful = do
  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvNotAvailable)
  sftEnv <- mkSFTEnv (SFTOptions "foo.example.com" Nothing)

  discoveryLoop <- Async.async $ runM . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEveryMillisecondWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should be the ones read from DNS" Nothing actualServers

testDiscoveryWhenUnsuccessfulAfterSuccess :: IO ()
testDiscoveryWhenUnsuccessfulAfterSuccess = do
  sftEnv <- testDiscoveryWhenSuccessful
  previousEntries <- readIORef (sftServers sftEnv)

  -- In the following lines we re-use the 'sftEnv' from a successful lookup to
  -- replicate what will happen when a dns lookup fails after success
  failingFakeDNSEnv <- newFakeDNSEnv (\_ -> SrvNotAvailable)
  discoveryLoop <- Async.async $ runM . runFakeDNSLookup failingFakeDNSEnv $ sftDiscoveryLoop sftEnv
  -- We wait for at least two lookups to be sure that the lookup loop looped at
  -- least once
  void $ retryEveryMillisecondWhileN 2000 (<= 1) (length <$> readIORef (fakeLookupCalls failingFakeDNSEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers shouldn't get overwriten" previousEntries actualServers

testDiscoveryWhenURLsChange :: IO ()
testDiscoveryWhenURLsChange = do
  sftEnv <- testDiscoveryWhenSuccessful

  -- In the following lines we re-use the 'sftEnv' from a successful lookup to
  -- replicate what will happen when a dns lookup returns new URLs
  let entry1 = SrvEntry 0 0 (SrvTarget "sft4.foo.example.com." 443)
      entry2 = SrvEntry 0 0 (SrvTarget "sft5.foo.example.com." 443)
      newEntries = (entry1 :| [entry2])

  fakeDNSEnv <- newFakeDNSEnv (\_ -> SrvAvailable newEntries)
  discoveryLoop <- Async.async $ runM . runFakeDNSLookup fakeDNSEnv $ sftDiscoveryLoop sftEnv
  void $ retryEveryMillisecondWhileN 2000 (== 0) (length <$> readIORef (fakeLookupCalls fakeDNSEnv))
  -- We don't want to stop the loop before it has written to the sftServers IORef
  void $ retryEveryMillisecondWhileN 2000 (== Just newEntries) (readIORef (sftServers sftEnv))
  Async.cancel discoveryLoop

  actualServers <- readIORef (sftServers sftEnv)
  assertEqual "servers should get overwritten" (Just newEntries) actualServers

retryEveryMillisecondWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryEveryMillisecondWhileN n f m =
  retrying
    (constantDelay 1000 <> limitRetries n)
    (const (return . f))
    (const m)
