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

module Federator.Run
  ( run,
    mkApp,

    -- * App Environment
    newEnv,
    closeEnv,

    -- * functions that FUTUREWORK should probably move to another module
    lookupDomainFake,
    lookupDomainByDNS,

    -- * dummy functions
    outBoundSayHello,
    outboundSayHello',
  )
where

import Control.Exception hiding (handle)
import Control.Lens (view, (^.))
import Control.Monad.Catch hiding (handle)
import Data.Default (def)
import Data.Domain (domainText)
import Data.Id (idToText)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Metrics.Middleware as Metrics
import Data.Qualified
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Federator.App
import qualified Federator.Brig as Brig
import Federator.GRPC.Proto
import qualified Federator.Impl as Impl
import Federator.Options as Opt
import Federator.Types
import Imports
import Mu.GRpc.Client.TyApps
import Mu.GRpc.Server
import Mu.Server hiding (resolver)
import Network.DNS (Resolver)
import qualified Network.DNS.Lookup as Lookup
import qualified Network.DNS.Resolver as Resolver
import Network.HTTP2.Client
import Network.Wai (Application)
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.API.User.Handle
import Wire.Network.DNS.SRV

------------------------------------------------------------------------------
-- run/app

run :: Opts -> IO ()
run opts = do
  (_app, env) <- mkApp opts
  -- settings <- Server.newSettings (restServer env)
  -- TODO: Combine the restful things and the grpc things
  -- Warp.runSettings settings app
  -- let grpcApplication = gRpcAppTrans msgProtoBuf (transformer env) grpcServer
  runGRpcAppTrans msgProtoBuf port (transformer env) grpcServer
  where
    endpoint = federator opts
    port = fromIntegral $ endpoint ^. epPort

    transformer :: Env -> AppIO a -> ServerErrorIO a
    transformer env action = runAppT env action

-- restServer env = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (env ^. applog) (env ^. metrics)

mkApp :: Opts -> IO (Application, Env)
mkApp opts = do
  env <- newEnv opts
  pure (Impl.app env, env)

------------------------------------------------------------------------------
-- grpc server api

grpcServer :: SingleServerT i Service AppIO _
grpcServer =
  singleService
    ( method @"SayHello" sayHello,
      method @"GetUserIdByHandle" getUserIdByHandle,
      method @"FederatedGetUserIdByHandle" federatedGetUserIdByHandle
    )

sayHello :: HelloRequestMessage -> AppIO HelloReplyMessage
sayHello (HelloRequestMessage nm) = do
  case nm of
    "Bob" -> throwError $ ServerError NotFound "Bob not allowed"
    _ -> pure $ HelloReplyMessage ("hi, " <> nm)

-- FUTUREWORK: getUserByHandle should return a full profile (see https://wearezeta.atlassian.net/browse/SQCORE-365)
-- getUserByHandle :: (MonadServer m) => QualifiedHandle -> m UserProfile
-- getUserByHandle (QualifiedHandle domain handle) = do
--   undefined

getUserIdByHandle :: QualifiedHandle -> AppIO QualifiedId
getUserIdByHandle handle@(QualifiedHandle federationDomain _) = do
  -- FUTUREWORK: we should parse federationDomain as Domain, not Text
  SrvTarget domain port <- lookupDomainFake federationDomain
  outBoundGetUserIdByHandle' (cs domain) (fromIntegral port) handle

federatedGetUserIdByHandle :: QualifiedHandle -> AppIO QualifiedId
federatedGetUserIdByHandle handle = do
  -- FUTUREWORK: validate the domain to be this installation's domain to avoid people misusing this server as a proxy to another federation backend
  askBrigForHandle handle

------------------------------------------------------------------------------
-- internal client calls, e.g. to brig

askBrigForHandle :: QualifiedHandle -> AppIO QualifiedId
askBrigForHandle handle = do
  res <- userHandleId <$> Brig.run handle
  pure $ QualifiedId (domainText $ qDomain res) (idToText $ qUnqualified res)

------------------------------------------------------------------------------
-- external client calls to other backends

outboundSayHello' :: HostName -> PortNumber -> T.Text -> AppIO ()
outboundSayHello' host port req = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- liftIO $ fmap (\(HelloReplyMessage r) -> r) <$> outBoundSayHello c (HelloRequestMessage req)
      print x
    _ -> undefined

outBoundSayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
outBoundSayHello = gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello"

outBoundGetUserIdByHandle :: GrpcClient -> QualifiedHandle -> IO (GRpcReply QualifiedId)
outBoundGetUserIdByHandle = gRpcCall @'MsgProtoBuf @Service @"Service" @"FederatedGetUserIdByHandle"

outBoundGetUserIdByHandle' :: HostName -> PortNumber -> QualifiedHandle -> AppIO QualifiedId
outBoundGetUserIdByHandle' host port handle = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- liftIO $ outBoundGetUserIdByHandle c handle
      print x
      case x of
        GRpcOk result -> pure result
        err -> throwError $ ServerError NotFound ("some error on outBoundGetUserIdByHandle: " <> show err)
    Left err -> throwError $ ServerError NotFound ("some error when creating a grpcClient: " <> show err)

-- FUTUREWORK: abstract the concrete things away as federator doesn't need to know all of it, all it needs is to know where the end request goes to (brig, in this case)
-- talkToBrig :: (MonadServer m) => Domain -> ByteString -> m ByteString
--

------------------------------------------------------------------------------
-- helper functions

lookupDomainFake :: Text -> AppIO SrvTarget
lookupDomainFake _ = do
  pure $ SrvTarget "127.0.0.1" 8097

lookupDomainByDNS :: Text -> AppIO SrvTarget
lookupDomainByDNS federationDomain = do
  resolver <- view dnsResolver
  let domainSrv = cs $ "_wire-server._tcp." <> federationDomain
  res <- liftIO $ interpretResponse <$> Lookup.lookupSRV resolver domainSrv
  case res of
    SrvAvailable entries -> do
      -- FUTUREWORK: orderSrvResult and try the list in order
      pure $ srvTarget $ NonEmpty.head entries
    SrvNotAvailable -> throwM $ ErrorCall $ "No SRV record for" <> (cs domainSrv) <> "available"
    SrvResponseError _ -> throwM $ ErrorCall $ "error srv lookup" <> (cs domainSrv)

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> IO Env
newEnv o = do
  _metrics <- Metrics.metrics
  _applog <- Log.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = (Opt.optSettings o)
  _dnsResolver <- mkDnsResolver
  return Env {..}

-- | Set up a thread-safe resolver with a global cache. This means that SRV
-- records will only be re-resolved after their TTLs expire
-- FUTUREWORK: move to dns-util lib?
mkDnsResolver :: IO Resolver
mkDnsResolver = do
  let resolvConf = Resolver.defaultResolvConf {Resolver.resolvCache = Just Resolver.defaultCacheConf}
  resolvSeed <- Resolver.makeResolvSeed resolvConf
  Resolver.withResolver resolvSeed $ \resolver -> return resolver

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog
