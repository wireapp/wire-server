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
  )
where

import qualified Bilge as RPC
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
import Data.Text.Encoding (encodeUtf8)
import Federator.App
import qualified Federator.Brig as Brig
import Federator.Federate (serveRouteToInternal)
import Federator.GRPC.Proto
import Federator.GRPC.Service
import qualified Federator.Impl as Impl
import Federator.Options as Opt
import Federator.Types
import Imports
import Mu.GRpc.Server
import Mu.Server hiding (resolver)
import qualified Network.DNS.Lookup as Lookup
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import Network.HTTP2.Client
import Network.Wai (Application)
import OpenSSL.Session
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified System.Logger.Class as Log
import qualified System.Logger.Extended as LogExt
import UnliftIO (race_)
import Util.Options
import Wire.API.User.Handle
import qualified Wire.Network.DNS.Helper as DNS
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
  let internalServer1 = runGRpcAppTrans msgProtoBuf port (transformer env) grpcServer
      -- TODO:Remove the line above and un-hardcode the port
      internalServer2 = serveRouteToInternal env 9999
  race_ internalServer1 internalServer2
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

meh :: String -> AppIO ()
meh msg = do
  Log.warn $ Log.msg msg

sayHello :: HelloRequestMessage -> AppIO HelloReplyMessage
sayHello (HelloRequestMessage nm) = do
  meh $ "%%-> Run/sayHello. Name:" <> show nm
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
  meh $ "%%-> Run/getUserIdByHandle. Handle:" <> show handle
  SrvTarget domain port <- lookupDomainFake federationDomain
  outBoundGetUserIdByHandle' (cs domain) (fromIntegral port) handle

federatedGetUserIdByHandle :: QualifiedHandle -> AppIO QualifiedId
federatedGetUserIdByHandle handle = do
  meh $ "%%-> Run/federatedGetUserIdByHandle. Handle:" <> show handle
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
--
-- imported from GRPC.Service

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
  _applog <- LogExt.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = Opt.optSettings o
  _dnsResolver <- DNS.mkDnsResolver
  let _brig = mkEndpoint (Opt.brig o)
  _httpManager <- initHttpManager
  return Env {..}
  where
    mkEndpoint service = RPC.host (encodeUtf8 (service ^. epHost)) . RPC.port (service ^. epPort) $ RPC.empty

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

-- | Copied from brig, do we want to put this somehwere common?
initHttpManager :: IO HTTP.Manager
initHttpManager = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  -- Unfortunately, there are quite some AWS services we talk to
  -- (e.g. SES, Dynamo) that still only support TLSv1.
  -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  HTTP.newManager
    (HTTP.opensslManagerSettings (pure ctx))
      { HTTP.managerConnCount = 1024,
        HTTP.managerIdleConnectionCount = 4096,
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000
      }
