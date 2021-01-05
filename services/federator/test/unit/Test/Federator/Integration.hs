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

module Test.Federator.Integration where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Domain (mkDomain)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Federator.GRPC.Proto
import Federator.Options
import Imports
import Mu.GRpc.Client.TyApps
import Network.HTTP2.Client
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Options"
    [ stuff
    ]

stuff :: TestTree
stuff = testCase "stuff" $ do
  reply <- sayHello' "127.0.0.1" 8097 "Bob"
  print reply

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply T.Text)
sayHello' host port req = do
  Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
  fmap (\(HelloReplyMessage r) -> r) <$> sayHello c (HelloRequestMessage req)

sayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
sayHello = gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello"

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
