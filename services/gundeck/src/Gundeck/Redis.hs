{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    connectRobust,
    runRobust,
    PingException,
  )
where

import Control.Concurrent.Async (Async, async)
import Control.Monad.Catch qualified as Catch
import Control.Retry
import Database.Redis
import Gundeck.Redis.HedisExtensions
import Imports
import System.Logger qualified as Log
import System.Logger.Class (MonadLogger)
import System.Logger.Class qualified as LogClass
import System.Logger.Extended
import UnliftIO.Exception

-- | Connection to Redis which allows reconnecting.
type RobustConnection = MVar Connection

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
  -- | action returning a fresh initial 'Connection', e. g., @(checkedConnect connInfo)@ or @(checkedConnectCluster connInfo)@
  IO Connection ->
  IO (Async (), RobustConnection)
connectRobust l retryStrategy connectLowLevel = do
  robustConnection <- newEmptyMVar @IO @Connection
  thread <-
    async $ safeForever l $ do
      Log.info l $ Log.msg (Log.val "connecting to Redis")
      conn <- retry connectLowLevel
      Log.info l $ Log.msg (Log.val "successfully connected to Redis")
      putMVar robustConnection conn
      catch
        ( forever $ do
            _ <- runRedis conn ping
            threadDelay 1e6
        )
        $ \(_ :: SomeException) -> void $ takeMVar robustConnection
  pure (thread, robustConnection)
  where
    retry =
      recovering -- retry connecting, e. g., with exponential back-off
        retryStrategy
        [ const $ Catch.Handler (\(e :: ClusterDownError) -> logEx (Log.err l) e "Redis cluster down" >> pure True),
          const $ Catch.Handler (\(e :: ConnectError) -> logEx (Log.err l) e "Redis not in cluster mode" >> pure True),
          const $ Catch.Handler (\(e :: ConnectTimeout) -> logEx (Log.err l) e "timeout when connecting to Redis" >> pure True),
          const $ Catch.Handler (\(e :: ConnectionLostException) -> logEx (Log.err l) e "Redis connection lost during request" >> pure True),
          const $ Catch.Handler (\(e :: PingException) -> logEx (Log.err l) e "pinging Redis failed" >> pure True),
          const $ Catch.Handler (\(e :: IOException) -> logEx (Log.err l) e "network error when connecting to Redis" >> pure True)
        ]
        . const -- ignore RetryStatus
    logEx :: (Show e) => ((Msg -> Msg) -> IO ()) -> e -> ByteString -> IO ()
    logEx lLevel e description = lLevel $ Log.msg (Log.val description) . Log.field "error" (show e)

-- | Run a 'Redis' action through a 'RobustConnection'.
--
-- Blocks on connection errors as long as the connection is not reestablished.
-- Without externally enforcing timeouts, this may lead to leaking threads.
runRobust :: (MonadUnliftIO m, MonadLogger m, Catch.MonadMask m) => RobustConnection -> Redis a -> m a
runRobust mvar action = retry $ do
  robustConnection <- readMVar mvar
  liftIO $ runRedis robustConnection action
  where
    retryStrategy = capDelay 1000000 (exponentialBackoff 50000)
    retry =
      recovering -- retry connecting, e. g., with exponential back-off
        retryStrategy
        [ logAndHandle $ Catch.Handler (\(_ :: ConnectionLostException) -> pure True),
          logAndHandle $ Catch.Handler (\(_ :: IOException) -> pure True)
        ]
        . const -- ignore RetryStatus
    logAndHandle (Handler handler) _ =
      Handler $ \e -> do
        LogClass.err $ Log.msg (Log.val "Redis connection failed") . Log.field "error" (show e)
        handler e

data PingException = PingException Reply deriving (Show)

instance Exception PingException

safeForever ::
  forall m.
  (MonadUnliftIO m) =>
  Logger ->
  m () ->
  m ()
safeForever l action =
  forever $
    action `catchAny` \e -> do
      Log.err l $ Log.msg (Log.val "Uncaught exception while connecting to redis") . Log.field "error" (displayException e)
      threadDelay 1e6 -- pause to keep worst-case noise in logs manageable
