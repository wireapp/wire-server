{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Cannon.Types
  ( Env (..),
    Cannon,
    connectionLimit,
    mapConcurrentlyCannon,
    mkEnv,
    runCannon,
    clients,
    wsenv,
    runCannonToServant,
  )
where

import Bilge (Manager)
import Bilge.RPC (HasRequestId (..))
import Cannon.Dict (Dict)
import Cannon.Options
import Cannon.RabbitMq
import Cannon.WS (Clock, Key, Websocket)
import Cannon.WS qualified as WS
import Cassandra (ClientState)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.Codensity
import Data.Id
import Data.Text.Encoding
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended (AmqpEndpoint)
import Prometheus
import Servant qualified
import System.Logger qualified as Logger
import System.Logger.Class hiding (info)
import System.Random.MWC (GenIO)

connectionLimit :: Int -- TODO rename to max number of buckets in Dict
connectionLimit = 128

-----------------------------------------------------------------------------
-- Cannon monad

data Env = Env
  { opts :: !Opts,
    applog :: !Logger,
    websockets :: !(Dict Key Websocket),
    rabbitConnections :: (Dict Key Q.Connection),
    reqId :: !RequestId,
    env :: !WS.Env
  }

newtype Cannon a = Cannon
  { unCannon :: ReaderT Env IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadMonitor
    )

mapConcurrentlyCannon :: (Traversable t) => (a -> Cannon b) -> t a -> Cannon (t b)
mapConcurrentlyCannon action inputs =
  Cannon $
    ask >>= \e ->
      liftIO $ mapConcurrently ((`runReaderT` e) . unCannon . action) inputs

instance MonadLogger Cannon where
  log l m = Cannon $ do
    g <- asks applog
    r <- field "request" . unRequestId <$> asks reqId
    liftIO $ Logger.log g l (r . m)

instance HasRequestId Cannon where
  getRequestId = Cannon $ asks reqId

mkEnv ::
  ByteString ->
  Opts ->
  ClientState ->
  Logger ->
  Dict Key Websocket ->
  Dict Key Q.Connection ->
  Manager ->
  GenIO ->
  Clock ->
  AmqpEndpoint ->
  Codensity IO Env
mkEnv external o cs l d conns p g t endpoint = do
  let poolOpts =
        RabbitMqPoolOptions
          { endpoint = endpoint,
            maxConnections = o ^. rabbitMqMaxConnections,
            maxChannels = o ^. rabbitMqMaxChannels
          }
  pool <- createRabbitMqPool poolOpts l
  let wsEnv =
        WS.env
          external
          (o ^. cannon . port)
          (encodeUtf8 $ o ^. gundeck . host)
          (o ^. gundeck . port)
          l
          p
          d
          conns
          g
          t
          (o ^. drainOpts)
          cs
          pool
  pure $ Env o l d conns (RequestId defRequestId) wsEnv

runCannon :: Env -> Cannon a -> IO a
runCannon e c = runReaderT (unCannon c) e

clients :: Cannon (Dict Key Websocket)
clients = Cannon $ asks websockets

wsenv :: Cannon WS.Env
wsenv = Cannon $ do
  e <- asks env
  r <- asks reqId
  pure $ WS.setRequestId r e

-- | Natural transformation from 'Cannon' to 'Handler' monad.
-- Used to call 'Cannon' from servant.
runCannonToServant :: Cannon.Types.Env -> Cannon x -> Servant.Handler x
runCannonToServant env c = liftIO $ runCannon env c
