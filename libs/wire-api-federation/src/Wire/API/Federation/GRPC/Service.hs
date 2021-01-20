{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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

module Wire.API.Federation.GRPC.Service where

import Control.Monad.Except (MonadError)
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps
import Mu.Server hiding (resolver)
import Network.HTTP2.Client
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as Log
import Wire.API.Federation.GRPC.Proto

type FederationServiceMonad m = (MonadIO m, MonadLogger m, MonadError ServerError m)

----------------------------------------------------------
-- dummy

outboundSayHello' :: HostName -> PortNumber -> T.Text -> IO ()
outboundSayHello' host port req = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- liftIO $ fmap (\(HelloReplyMessage r) -> r) <$> outBoundSayHello c (HelloRequestMessage req)
      print x
    _ -> undefined

outBoundSayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
outBoundSayHello = gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello"

----------------------------------------------------------
-- calls to other backends

outBoundGetUserIdByHandle :: GrpcClient -> QualifiedHandle -> IO (GRpcReply QualifiedId)
outBoundGetUserIdByHandle = gRpcCall @'MsgProtoBuf @Service @"Service" @"FederatedGetUserIdByHandle"

outBoundGetUserIdByHandle' :: FederationServiceMonad m => HostName -> PortNumber -> QualifiedHandle -> m QualifiedId
outBoundGetUserIdByHandle' host port handle = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- liftIO $ outBoundGetUserIdByHandle c handle
      Log.warn $ Log.msg $ "%%-> outBoundGetUserIdByHandle. Result: " <> (show x)
      case x of
        GRpcOk result -> pure result
        err -> throwError $ ServerError NotFound ("some error on outBoundGetUserIdByHandle: " <> show err)
    Left err -> throwError $ ServerError NotFound ("some error when creating a grpcClient: " <> show err)

----------------------------------------------------------
-- client calls needed A) for federator integration testing and B) by other services e.g. brig.
-- FUTUREWORK: place these elsewhere and expose them so brig can make use of them

iGetUserIdByHandle :: MonadIO m => GrpcClient -> QualifiedHandle -> m (GRpcReply QualifiedId)
iGetUserIdByHandle c handle = liftIO $ gRpcCall @'MsgProtoBuf @Service @"Service" @"GetUserIdByHandle" c handle

iGetUserIdByHandle' :: FederationServiceMonad m => HostName -> PortNumber -> QualifiedHandle -> m QualifiedId
iGetUserIdByHandle' host port handle = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- iGetUserIdByHandle c handle
      Log.warn $ Log.msg $ "%%-> iGetUserIdByHandle. Result: " <> (show x)
      case x of
        GRpcOk result -> pure result
        err -> throwError $ ServerError NotFound ("some error on iGetUserIdByHandle: " <> show err)
    Left err -> throwError $ ServerError NotFound ("some error when creating a grpcClient: " <> show err)

-- FUTUREWORK: re-use grpc clients?
-- is it threadsafe?
-- what to do when talking to 10000 other backends? (is that even a relevant scenario?)
-- how to do connection pooling with grpc and http2?
-- what about load balancers?
