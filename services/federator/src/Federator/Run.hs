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

import Control.Lens ((^.))
import Data.Default (def)
import Data.Domain
import qualified Data.Metrics.Middleware as Metrics
import Data.Text (unpack)
import Federator.App (AppIO)
import Federator.GRPC.Proto
import qualified Federator.Impl as Impl
import Federator.Options as Opt
import Federator.Types
import Imports
import Mu.GRpc.Server
import Mu.Server
import Network.HTTP2.Client
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Server as Server
import qualified System.Logger.Extended as Log
import Util.Options

run :: Opts -> IO ()
run opts = do
  (app, env) <- mkApp opts
  settings <- Server.newSettings (restServer env)
  -- TODO: Combine the restful things and the grpc things
  -- Warp.runSettings settings app
  let grpcApplication = gRpcApp msgProtoBuf grpcServer
  runGRpcApp msgProtoBuf (fromIntegral $ endpoint ^. epPort) grpcServer
  where
    endpoint = federator opts
    restServer env = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (env ^. applog) (env ^. metrics)

mkApp :: Opts -> IO (Application, Env)
mkApp opts = do
  env <- newEnv opts
  pure (Impl.app env, env)

sayHello :: (MonadServer m) => HelloRequestMessage -> m HelloReplyMessage
sayHello (HelloRequestMessage nm) =
  pure $ HelloReplyMessage ("hi, " <> nm)

getUserByHandle :: (MonadServer m) => QualifiedHandle -> m UserProfile
getUserByHandle (QualifiedHandle domain handle) = do
  undefined

getUserIdByHandle :: (MonadServer m) => QualifiedHandle -> m QualifiedId
getUserIdByHandle (QualifiedHandle domain handle) = do
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

grpcServer :: (MonadServer m) => SingleServerT i Service m _
grpcServer = singleService (method @"SayHello" sayHello, method @"GetUserIdByHandle" getUserIdByHandle)

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> IO Env
newEnv o = do
  _metrics <- Metrics.metrics
  _applog <- Log.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = (Opt.optSettings o)
  return Env {..}

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog
