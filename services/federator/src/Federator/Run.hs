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
    mkTLSSettings,
    FederationSetupError (..),
    closeEnv,
  )
where

import qualified Bilge as RPC
import Control.Exception (handle, throw)
import Control.Lens ((^.))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text.Encoding (encodeUtf8)
import qualified Data.X509 as X509
import Data.X509.CertificateStore
import Federator.Env
import Federator.ExternalServer (serveInward)
import Federator.InternalServer (serveOutward)
import Federator.Options as Opt
import Imports
import qualified Network.DNS as DNS
import qualified Network.HTTP.Client as HTTP
import qualified Network.TLS as TLS
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

data FederationSetupError
  = InvalidCAStore FilePath
  | InvalidClientCertificate String
  deriving (Show)

instance Exception FederationSetupError

newEnv :: Opts -> DNS.Resolver -> IO Env
newEnv o _dnsResolver = do
  _metrics <- Metrics.metrics
  _applog <- LogExt.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = Opt.optSettings o
  let _service Brig = mkEndpoint (Opt.brig o)
      _service Galley = mkEndpoint (Opt.galley o)
  _httpManager <- initHttpManager
  _tls <- mkTLSSettings _runSettings
  return Env {..}
  where
    mkEndpoint s = RPC.host (encodeUtf8 (s ^. epHost)) . RPC.port (s ^. epPort) $ RPC.empty

mkCAStore :: RunSettings -> IO CertificateStore
mkCAStore settings = do
  customCAStore <- fmap (fromRight mempty) . Polysemy.runM . Polysemy.runError @() $ do
    path <- maybe (Polysemy.throw ()) pure $ remoteCAStore settings
    Polysemy.embed $ readCertificateStore path >>= maybe (throw $ InvalidCAStore path) pure
  systemCAStore <-
    if useSystemCAStore settings
      then getSystemCertificateStore
      else pure mempty
  pure (customCAStore <> systemCAStore)

getClientCredentials :: RunSettings -> Either String (Maybe (FilePath, FilePath))
getClientCredentials settings = case clientCertificate settings of
  Nothing -> noCreds1 $> Nothing
  Just cert -> Just . (cert,) <$> getCreds1
  where
    noCreds1 :: Either String ()
    noCreds1
      | isNothing (clientPrivateKey settings) = pure ()
      | otherwise = Left "invalid client credentials: no certificate"

    getCreds1 :: Either String FilePath
    getCreds1 =
      maybe (Left "invalid client credentials: no private key") pure $
        clientPrivateKey settings

mkCreds :: RunSettings -> IO (Maybe TLS.Credential)
mkCreds settings = handle h $ case getClientCredentials settings of
  Left e -> throw (InvalidClientCertificate e)
  Right Nothing -> pure Nothing
  Right (Just (cert, key)) ->
    TLS.credentialLoadX509 cert key >>= \case
      Left e -> throw (InvalidClientCertificate e)
      Right (X509.CertificateChain [], _) ->
        throw (InvalidClientCertificate "could not read client certificate")
      Right x -> pure (Just x)
  where
    h :: IOException -> IO a
    h = throw . InvalidClientCertificate . show

mkTLSSettings :: RunSettings -> IO TLSSettings
mkTLSSettings settings =
  TLSSettings
    <$> mkCAStore settings
    <*> mkCreds settings

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
