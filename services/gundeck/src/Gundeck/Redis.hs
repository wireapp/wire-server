{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

module Gundeck.Redis
  ( RobustConnection,
    rrConnection,
    rrReconnect,
    connectRobust,
    runRobust,
    PingException,
  )
where

import Control.Concurrent.Extra (once)
import Control.Lens
import qualified Control.Monad.Catch as Catch
import Control.Retry
import Database.Redis
import Imports
import qualified System.Logger as Log
import System.Logger.Extended
import UnliftIO.Exception

-- | Connection to Redis which allows reconnecting.
type RobustConnection = MVar ReConnection

data ReConnection = ReConnection
  { _rrConnection :: Connection, -- established (and potentially breaking) connection to Redis
    _rrReconnect :: IO () -- action which can be called to reconnect to Redis
  }

makeLenses ''ReConnection

-- | Connection to Redis which can be reestablished on connection errors.
--
-- Reconnecting even when Redis IPs change as long as the DNS name remains
-- constant. The server type (cluster or not) and the connection information of
-- the initial connection are used when reconnecting.
--
-- Throws 'ConnectError', 'ConnectTimeout', 'ConnectionLostException',
-- 'PingException', or 'IOException' if retry policy is finite.
connectRobust ::
  Logger ->
  -- | e. g., @exponentialBackoff 50000@
  RetryPolicy ->
  -- | action returning a fresh initial 'Connection', e. g., @(connect connInfo)@ or @(connectCluster connInfo)@
  IO Connection ->
  IO RobustConnection
connectRobust l retryStrategy connectLowLevel = do
  robustConnection <- newEmptyMVar @IO @ReConnection
  reconnectRedis robustConnection
  pure robustConnection
  where
    reconnectRedis robustConnection = do
      conn <- connectLowLevel

      Log.info l $ Log.msg (Log.val "lazy connection established, running ping...")
      -- TODO With ping, we only verify that a single node is running as opposed
      -- to verifying that all nodes of the cluster are up and running. It
      -- remains unclear how cluster health can be verified in hedis.
      void . runRedis conn $ do
        res <- ping
        case res of
          Left r -> throwIO $ PingException r
          Right _ -> pure ()
      Log.info l $ Log.msg (Log.val "ping went through")

      reconnectOnce <-
        once $ -- avoid concurrent attempts to reconnect
          recovering -- retry connecting, e. g., with exponential back-off
            retryStrategy
            [ const $ Catch.Handler (\(e :: ConnectError) -> logEx (Log.err l) e "Redis not in cluster mode" >> pure True),
              const $ Catch.Handler (\(e :: ConnectTimeout) -> logEx (Log.err l) e "timeout when connecting to Redis" >> pure True),
              const $ Catch.Handler (\(e :: ConnectionLostException) -> logEx (Log.err l) e "Redis connection lost during request" >> pure True),
              const $ Catch.Handler (\(e :: PingException) -> logEx (Log.err l) e "pinging Redis failed" >> pure True),
              const $ Catch.Handler (\(e :: IOException) -> logEx (Log.err l) e "network error when connecting to Redis" >> pure True)
            ]
            $ const $
              reconnectRedis robustConnection
      let newReConnection = ReConnection {_rrConnection = conn, _rrReconnect = reconnectOnce}
      unlessM (tryPutMVar robustConnection newReConnection) $
        void $ swapMVar robustConnection newReConnection

-- | Run a 'Redis' action through a 'RobustConnection'.
--
-- Blocks on connection errors as long as the connection is not reestablished.
-- Without externally enforcing timeouts, this may lead to leaking threads.
runRobust :: RobustConnection -> Redis a -> IO a
runRobust mvar action = do
  robustConnection <- readMVar mvar
  catches
    (runRedis (_rrConnection robustConnection) action)
    [ Handler (\(_ :: ConnectionLostException) -> reconnectRetry robustConnection), -- Redis connection lost during request
      Handler (\(_ :: IOException) -> reconnectRetry robustConnection) -- Redis unreachable
    ]
  where
    reconnectRetry robustConnection = do
      _rrReconnect robustConnection
      runRobust mvar action

logEx :: Show e => ((Msg -> Msg) -> IO ()) -> e -> ByteString -> IO ()
logEx lLevel e description = lLevel $ Log.msg $ Log.val $ description <> ": " <> fromString (show e)

data PingException = PingException Reply deriving (Show)

instance Exception PingException
