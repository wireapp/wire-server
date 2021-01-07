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
  )
where

-- import Data.Domain

import Control.Lens (view, (^.))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
-- import Data.Text (unpack)
-- import Federator.App (AppIO)

-- import qualified Network.Wai.Handler.Warp as Warp
-- import Network.Wai.Utilities.Server as Server

import Federator.App (AppIO, runAppT)
import Federator.GRPC.Proto
import qualified Federator.Impl as Impl
import Federator.Options as Opt
import Federator.Types
import Imports
import Mu.GRpc.Server
import Mu.Server hiding (resolver)
import Network.DNS.Resolver (Resolver)
import qualified Network.DNS.Resolver as Resolver
import Network.HTTP2.Client
import Network.Wai (Application)
import qualified Servant.Server as Servant
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.Network.DNS.SRV

run :: Opts -> IO ()
run opts = do
  (_app, env) <- mkApp opts
  -- settings <- Server.newSettings (restServer env)
  -- TODO: Combine the restful things and the grpc things
  -- Warp.runSettings settings app
  -- let grpcApplication = gRpcAppTrans msgProtoBuf (transformer env) grpcServer
  runGRpcAppTrans msgProtoBuf port (transformer env) grpcServer
  undefined
  where
    endpoint = federator opts
    port = fromIntegral $ endpoint ^. epPort

    -- These Monad stack conversions confuse me greatly. Help?
    transformer :: Env -> AppIO a -> ServerErrorIO a
    transformer _env _action = undefined -- runAppT env

-- restServer env = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (env ^. applog) (env ^. metrics)

mkApp :: Opts -> IO (Application, Env)
mkApp opts = do
  env <- newEnv opts
  pure (Impl.app env, env)

sayHello :: HelloRequestMessage -> AppIO HelloReplyMessage
sayHello (HelloRequestMessage nm) =
  pure $ HelloReplyMessage ("hi, " <> nm)

-- FUTUREWORK: getUserByHandle should return a full profile (see https://wearezeta.atlassian.net/browse/SQCORE-365)
-- getUserByHandle :: (MonadServer m) => QualifiedHandle -> m UserProfile
-- getUserByHandle (QualifiedHandle domain handle) = do
--   undefined

getUserIdByHandle :: QualifiedHandle -> AppIO QualifiedId
getUserIdByHandle (QualifiedHandle domain handle) = do
  resolver <- view dnsResolver
  undefined

-- otherBackend <- lookupBackend domain
-- client <- constructClient otherBackend

-- lookupBackend :: Domain -> AppIO Backend
-- lookupBackend = undefined

-- constructClient :: Backend -> AppIO GRpcClient
-- constructClient = undefined

data Backend = Backend {host :: HostName, port :: PortNumber} -- TODO don't we already have a definition of something like this?

-- FUTUREWORK: abstract the concrete things away as federator doesn't need to know all of it, all it needs is to know where the end request goes to (brig, in this case)
-- talkToBrig :: (MonadServer m) => Domain -> ByteString -> m ByteString

grpcServer :: SingleServerT i Service AppIO _
grpcServer = singleService (method @"SayHello" sayHello, method @"GetUserIdByHandle" getUserIdByHandle)

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> IO Env
newEnv o = do
  _metrics <- Metrics.metrics
  _applog <- Log.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = (Opt.optSettings o)

  -- Set up a thread-safe resolver with a global cache. This means that SRV
  -- records will only be re-resolved after their TTLs expire
  let resolvConf = Resolver.defaultResolvConf {Resolver.resolvCache = Just Resolver.defaultCacheConf}
  resolvSeed <- Resolver.makeResolvSeed resolvConf
  Resolver.withResolver resolvSeed $ \_dnsResolver ->
    return Env {..}

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog
