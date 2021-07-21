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
    mkCAStore,
    closeEnv,
  )
where

import qualified Bilge as RPC
import Control.Exception (throw)
import Control.Lens ((^.))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text.Encoding (encodeUtf8)
import Data.X509.CertificateStore
import Federator.Env
import Federator.ExternalServer (serveInward)
import Federator.InternalServer (serveOutward)
import Federator.Options as Opt
import Imports
import qualified Network.DNS as DNS
import qualified Network.HTTP.Client as HTTP
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import qualified System.Logger.Class as Log
import qualified System.Logger.Extended as LogExt
import System.X509
import UnliftIO (bracket)
import UnliftIO.Async (async, waitAnyCancel)
import Util.Options
import Wire.API.Federation.GRPC.Types
import qualified Wire.Network.DNS.Helper as DNS

------------------------------------------------------------------------------
-- run/app

-- FUTUREWORK(federation): Add metrics and status endpoints
-- (this probably requires using HTTP. A Servant API could be used; and the
-- internal grpc server converted to a WAI application, and the grpc application be
-- "merged" using Servant's 'Raw' type (like in 'brig') with servant's http
-- endpoints and exposed on the same port.
run :: Opts -> IO ()
run opts =
  DNS.withCachingResolver $ \res ->
    bracket (newEnv opts res) closeEnv $ \env -> do
      let externalServer = serveInward env portExternal
          internalServer = serveOutward env portInternal
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

newtype InvalidCAStore = InvalidCAStore FilePath
  deriving (Show)

instance Exception InvalidCAStore

newEnv :: Opts -> DNS.Resolver -> IO Env
newEnv o _dnsResolver = do
  _metrics <- Metrics.metrics
  _applog <- LogExt.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = Opt.optSettings o
  let _service Brig = mkEndpoint (Opt.brig o)
      _service Galley = mkEndpoint (Opt.galley o)
  _httpManager <- initHttpManager
  _caStore <- mkCAStore _runSettings
  return Env {..}
  where
    mkEndpoint s = RPC.host (encodeUtf8 (s ^. epHost)) . RPC.port (s ^. epPort) $ RPC.empty

mkCAStore :: RunSettings -> IO CertificateStore
mkCAStore settings = do
  customCAStore <- fmap (fromRight mempty) . Polysemy.runM . Polysemy.runError @() $ do
    path <- maybe (Polysemy.throw ()) pure $ remoteCAStore settings
    Polysemy.embed $ readCertificateStore path >>= maybe (throw $ InvalidCAStore path) pure
  -- FUTUREWORK: review if a fallback to system trust store is a good idea
  systemCAStore <-
    if fromMaybe True (useSystemCAStore settings)
      then getSystemCertificateStore
      else pure mempty
  pure (customCAStore <> systemCAStore)

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

-- | Copied (and adjusted) from brig, do we want to put this somehwere common?
-- FUTUREWORK(federation): review certificate and protocol security setting for this TLS
-- manager
initHttpManager :: IO HTTP.Manager
initHttpManager =
  -- See Note [SSL context]
  HTTP.newManager
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 1024,
        HTTP.managerIdleConnectionCount = 4096,
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000
      }
