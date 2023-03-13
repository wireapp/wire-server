{-# LANGUAGE NumericUnderscores #-}

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

module Network.Wai.Utilities.MockServer where

import qualified Control.Concurrent.Async as Async
import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.Codensity
import Data.Streaming.Network (bindRandomPortTCP)
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified System.Timeout as System

-- | Thrown in IO by mock federator if the server could not be started after 10
-- seconds.
newtype MockTimeout = MockTimeout Warp.Port
  deriving (Eq, Show, Typeable)

instance Exception MockTimeout

withMockServer :: Wai.Application -> Codensity IO Word16
withMockServer app = Codensity $ \k ->
  bracket
    (liftIO $ startMockServer Nothing app)
    (liftIO . fst)
    (k . fromIntegral . snd)

-- FUTUREWORK: Ignore HTTP2.ConnectionIsClosed after upgrading to more recent
-- http2 library.
ignoreHTTP2NonError :: Maybe Wai.Request -> SomeException -> IO ()
ignoreHTTP2NonError = Warp.defaultOnException

-- | Start a mock warp server on a random port, serving the given Wai application.
--
-- If the 'Warp.TLSSettings` argument is provided, start an HTTPS server,
-- otherwise start a plain HTTP server.
--
-- Returns an action to kill the spawned server, and the port on which the
-- server is running.
--
-- This function should normally be used within 'bracket', e.g.:
-- @
--     bracket (startMockServer Nothing app) fst $ \(close, port) ->
--       makeRequest "localhost" port
-- @
startMockServer :: Maybe Warp.TLSSettings -> Wai.Application -> IO (IO (), Warp.Port)
startMockServer mtlsSettings app = do
  (port, sock) <- bindRandomPortTCP "*6"
  serverStarted <- newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
          & Warp.setOnException ignoreHTTP2NonError

  serverThread <- Async.async $ case mtlsSettings of
    Just tlsSettings -> Warp.runTLSSocket tlsSettings wsettings sock app
    Nothing -> Warp.runSettingsSocket wsettings sock app
  serverStartedSignal <- System.timeout 10_000_000 (readMVar serverStarted)
  let closeMock = do
        me <- Async.poll serverThread
        case me of
          Nothing -> Async.cancel serverThread
          Just (Left e) -> throw e
          Just (Right a) -> pure a
  case serverStartedSignal of
    Nothing -> do
      Async.cancel serverThread
      throw (MockTimeout port)
    Just _ -> pure (closeMock, port)
