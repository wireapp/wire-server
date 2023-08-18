{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

    -- * App Environment
    newEnv,
    closeEnv,

    -- * Re-exports
    mkTLSSettingsOrThrow,
    FederationSetupError (..),
  )
where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Data.Default (def)
import Data.Metrics.Middleware qualified as Metrics
import Federator.Env
  ( Env (..),
    applog,
    mkHttp2Manager,
    onNewSSLContext,
  )
import Federator.ExternalServer (serveInward)
import Federator.InternalServer (serveOutward)
import Federator.Monitor
  ( FederationSetupError (..),
    mkTLSSettingsOrThrow,
    withMonitor,
  )
import Federator.Options as Opt
  ( Opts
      ( brig,
        cargohold,
        federatorExternal,
        federatorInternal,
        galley,
        logFormat,
        logLevel,
        logNetStrings,
        optSettings
      ),
    RunSettings (dnsHost, dnsPort),
  )
import Imports
import Network.DNS qualified as DNS
import Network.HTTP.Client qualified as HTTP
import System.Logger qualified as Log
import System.Logger.Extended qualified as LogExt
import Util.Options (Endpoint (_port), port)
import Wire.API.FederationUpdate
import Wire.API.MakesFederatedCall
  ( Component (Brig, Cargohold, Galley),
  )
import Wire.API.Routes.FederationDomainConfig
import Wire.Network.DNS.Helper qualified as DNS

------------------------------------------------------------------------------
-- run/app

-- FUTUREWORK(federation): Add metrics and status endpoints
run :: Opts -> IO ()
run opts = do
  let resolvConf = mkResolvConf (optSettings opts) DNS.defaultResolvConf
  DNS.withCachingResolver resolvConf $ \res -> do
    logger <- LogExt.mkLogger (Opt.logLevel opts) (Opt.logNetStrings opts) (Opt.logFormat opts)
    (ioref, updateFedDomainsThread) <- syncFedDomainConfigs (brig opts) logger emptySyncFedDomainConfigsCallback
    bracket (newEnv opts res logger ioref) closeEnv $ \env -> do
      let externalServer = serveInward env portExternal
          internalServer = serveOutward env portInternal
      withMonitor logger (onNewSSLContext env) (optSettings opts) $ do
        internalServerThread <- async internalServer
        externalServerThread <- async externalServer
        void $ waitAnyCancel [updateFedDomainsThread, internalServerThread, externalServerThread]
  where
    endpointInternal = federatorInternal opts
    portInternal = fromIntegral $ endpointInternal ^. port

    endpointExternal = federatorExternal opts
    portExternal = fromIntegral $ endpointExternal ^. port

    mkResolvConf :: RunSettings -> DNS.ResolvConf -> DNS.ResolvConf
    mkResolvConf settings conf =
      case (dnsHost settings, dnsPort settings) of
        (Just h, Nothing) ->
          conf {DNS.resolvInfo = DNS.RCHostName h}
        (Just h, Just p) ->
          conf {DNS.resolvInfo = DNS.RCHostPort h (fromIntegral p)}
        (_, _) -> conf

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> DNS.Resolver -> Log.Logger -> IORef FederationDomainConfigs -> IO Env
newEnv o _dnsResolver _applog _domainConfigs = do
  _metrics <- Metrics.metrics
  let _requestId = def
      _runSettings = Opt.optSettings o
      _service Brig = Opt.brig o
      _service Galley = Opt.galley o
      _service Cargohold = Opt.cargohold o
      _externalPort = o.federatorExternal._port
      _internalPort = o.federatorInternal._port
  _httpManager <- initHttpManager
  sslContext <- mkTLSSettingsOrThrow _runSettings
  _http2Manager <- newIORef =<< mkHttp2Manager sslContext
  pure Env {..}

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

initHttpManager :: IO HTTP.Manager
initHttpManager =
  HTTP.newManager
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 1024,
        HTTP.managerIdleConnectionCount = 4096,
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000
      }
