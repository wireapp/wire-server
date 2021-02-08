{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Test.Federator.APISpec where

import Control.Monad.Except (MonadError (..))
import Imports
import Mu.Server
import Network.HTTP2.Client
import qualified System.Logger as L
import qualified System.Logger.Class as LC
import Test.Hspec

-- Copied from Spar
-- it ::
--   HasCallStack =>
--   -- or, more generally:
--   -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
--   String ->
--   TestFederator () ->
--   SpecWith TestEnv
-- it msg bdy = Test.Hspec.it msg $ runReaderT bdy

-- instance LC.MonadLogger IO where
--   log = undefined

-- instance LC.MonadLogger (ExceptT ServerError IO) where
--   log = undefined

newtype Foo m a = Foo {runFoo :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadError ServerError m => MonadError ServerError (Foo m) where
  throwError = Foo . throwError @_ @m
  catchError a f =
    Foo $ catchError (runFoo a) (runFoo . f)

instance MonadIO m => LC.MonadLogger (Foo m) where
  log level msg = do
    logger <- LC.new LC.defSettings
    L.log logger level msg

-- instance MonadError ServerError m => MonadErrorServer Foo (m

-- instance LC.MonadLogger (ExceptT ServerError (Foo IO)) where
--   log level msg = do
--     lift $ LC.log level msg
--

--

tests :: Spec
tests = do
  testHello
  testNetworkHops

testHello :: Spec -- With TestEnv
testHello = do
  describe "sayHello" $ do
    it "answers dummy hello grpc calls for Alice" $ do
      True `shouldBe` False

-- reply <- sayHello' "127.0.0.1" 8097 "Alice"
-- -- FUTUREWORK: How to extract/compare things without cumbersome pattern matching?
-- case reply of
--   GRpcOk contents -> contents `shouldBe` "hi, Alice"
--   _ -> expectationFailure "reply ought to be a GRpcOk"

-- | We kind of want this network flow (and back) for a user handle lookup:
--
-- +------+         +---------+        +---------+          +------+
-- | brig |   grpc  |federator| grpc   |federator|   http   | brig |
-- |      +-------->+         +------->+         +--------->+      |
-- +------+         +-+-------+        +---------+          +------+
--
-- What can we test inside the federator-integration
-- tests without having multiple of these services running?
-- We can simplify the above to make the following network calls,
-- to at least test the basic functioning of network calls:
--
--                                grpc
--                              +-----+
--                              |     |
-- +----------+                 |     |
-- |federator-|          +------+--+  |
-- |integration  grpc    |federator|  |
-- |          |--------->+         +<-+
-- |          |          +----+----+
-- +----------+               |
--                            |
--                            v
--                        +---+--+
--                        | brig |
--                        |      |
--                        +------+
--
-- (ascii diagrams from asciiflow.com)
testNetworkHops :: Spec
testNetworkHops = do
  describe "multi-hop network tests" $ do
    it "getHandleInfo federator -> federator -> brig (NotFound case)" $ do
      True `shouldBe` False

-- let handle = QualifiedHandle "invalid.com" "alice123"
-- let x = iGetUserIdByHandle' "127.0.0.1" 8097 handle
-- reply <- runExceptT . runFoo $ x
-- reply `shouldSatisfy` (either (\(ServerError code _) -> code == NotFound) (const False))
-- putStr "reply: "
-- print reply

-------------------------------------------

-- FUTUREWORK: lookup brig and federator from another namespace via DNS for integration testing:
--
-- We could make use of kubernetes-internal DNS records:
--
-- currently the federator chart, for a given namespace, can be SRV queried like so:
--
-- dig  _http._tcp.federator.<NAMESPACE>.svc.cluster.local SRV
--
-- returning ;; ANSWER SECTION:
-- _http._tcp.federator.joe-dev1.svc.cluster.local. 5 IN SRV 0 100 8080 federator.joe-dev1.svc.cluster.local.
-- ;; ADDITIONAL SECTION:
-- federator.joe-dev1.svc.cluster.local. 5 IN A 10.233.37.171
--
-- if we really need to modify/create more DNS records, e.g. https://coredns.io/2017/05/08/custom-dns-entries-for-kubernetes/ could be a starting point.
