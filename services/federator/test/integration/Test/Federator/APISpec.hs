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

import Control.Monad.Error (MonadError (..))
import qualified Data.Text as T
import Federator.GRPC.Proto
import Federator.GRPC.Service
import qualified Federator.ParsingError as ParsingError
import Imports
import Mu.GRpc.Client.TyApps
import Mu.Server
import Network.HTTP2.Client
import qualified System.Logger as L
import qualified System.Logger.Class as LC
import Test.Federator.Util
import Test.Hspec
import qualified Test.Hspec

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

tests :: Spec -- With TestEnv
tests = do
  describe "sayHello" $ do
    it "answers dummy hello grpc calls for Alice" $ do
      reply <- sayHello' "127.0.0.1" 8097 "Alice"
      -- FUTUREWORK: How to extract/compare things without cumbersome pattern matching?
      case reply of
        GRpcOk contents -> contents `shouldBe` "hi, Alice"
        _ -> expectationFailure "reply ought to be a GRpcOk"

    -- it "answers dummy hello grpc calls for Bob" $ do
    --   liftIO $ do
    --     reply <- sayHello' "127.0.0.1" 8097 "Bob"
    --     case reply of
    --       GRpcOk contents -> contents `shouldBe` "hi, Bob"
    --       stuff -> do
    --         print stuff
    --         expectationFailure "reply ought to be a GRpcOk"

    it "getHandleInfo federator -> federator -> brig" $ do
      let handle = QualifiedHandle "invalid.com" "alice123"
      let x = iGetUserIdByHandle' "127.0.0.1" 8097 handle
      reply <- runExceptT . runFoo $ x
      True `shouldBe` True
      print reply

    it "manual test" $ do
      ParsingError.main
      True `shouldBe` True

-------------------------------------------

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply T.Text)
sayHello' host port req = do
  Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
  fmap (\(HelloReplyMessage r) -> r) <$> sayHello c (HelloRequestMessage req)

sayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
sayHello x msg = do
  foo <- gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello" x msg
  pure foo

-- brig -> grpc call to federator
-- federator -> grpc call to ... nginz?
-- nginz -> option 1: TCP proxy to federator? (TODO unclear)
--       -> option 2: strip TLS layer of the request, forward some added header to federator
-- federator receive call from outside;
-- federator to brig would be
--    - restful?
--    - also grpc?
--
--
--
