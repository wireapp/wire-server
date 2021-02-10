{-# LANGUAGE FlexibleContexts #-}
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

    -- * App Environment
    newEnv,
    closeEnv,
  )
where

import qualified Bilge as RPC
import Control.Lens ((^.))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text.Encoding (encodeUtf8)
import Federator.Federate (serveRouteToInternal, serveRouteToRemote)
import Federator.Options as Opt
import Federator.Types
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import OpenSSL.Session
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified System.Logger.Class as Log
import qualified System.Logger.Extended as LogExt
import UnliftIO (bracket)
import UnliftIO.Async (async, waitAnyCancel)
import Util.Options
import qualified Wire.Network.DNS.Helper as DNS

------------------------------------------------------------------------------
-- run/app

run :: Opts -> IO ()
run opts =
  bracket (newEnv opts) closeEnv $ \env -> do
    let externalServer = serveRouteToInternal env portExternal
        internalServer = serveRouteToRemote env portInternal
    internalServerThread <- async internalServer
    externalServerThread <- async externalServer
    void $ waitAnyCancel [internalServerThread, externalServerThread]
  where
    endpointInternal = federatorInternal opts
    portInternal = fromIntegral $ endpointInternal ^. epPort

    endpointExternal = federatorExternal opts
    portExternal = fromIntegral $ endpointExternal ^. epPort

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
  let _brigEndpoint = Opt.brig o
  _httpManager <- initHttpManager
  return Env {..}
  where
    mkEndpoint service = RPC.host (encodeUtf8 (service ^. epHost)) . RPC.port (service ^. epPort) $ RPC.empty

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

-- | Copied (and adjusted) from brig, do we want to put this somehwere common?
-- FUTUREWORK: review certificate and protocol security setting for this TLS
-- manager
initHttpManager :: IO HTTP.Manager
initHttpManager = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  HTTP.newManager
    (HTTP.opensslManagerSettings (pure ctx))
      { HTTP.managerConnCount = 1024,
        HTTP.managerIdleConnectionCount = 4096,
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000
      }
