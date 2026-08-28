{-# LANGUAGE TemplateHaskell #-}

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

module Gundeck.Env where

import Bilge hiding (host, port)
import Cassandra (ClientState)
import Cassandra.Util (initCassandraForService)
import Control.AutoUpdate
import Control.Lens (makeLenses, (^.))
import Data.Id
import Data.Misc (Milliseconds (..))
import Data.Time.Clock.POSIX
import Gundeck.Aws qualified as Aws
import Gundeck.Options
import Gundeck.ThreadBudget
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Network.AMQP (Channel)
import Network.AMQP.Extended qualified as Q
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Logger.Extended qualified as Logger

data Env = Env
  { _reqId :: !RequestId,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _hasqlPool :: !HasqlPoolExt.Pool,
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState),
    _rabbitMqChannel :: MVar Channel
  }

makeLenses ''Env

createEnv :: Opts -> IO Env
createEnv o = do
  l <- Logger.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = o ^. settings . httpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }

  pgPool <- HasqlPoolExt.initPostgresPool (o ^. postgresqlPool) (o ^. postgresql) (o ^. postgresqlPassword)

  p <-
    initCassandraForService
      (o ^. cassandra)
      "gundeck"
      (o ^. discoUrl)
      Nothing
      l

  a <- Aws.mkEnv l o n
  io <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = Ms . round . (* 1000) <$> getPOSIXTime
        }
  mtbs <- mkThreadBudgetState `mapM` (o ^. settings . maxConcurrentNativePushes)
  rabbitMqChannelMVar <- Q.mkRabbitMqChannelMVar l (Just "gundeck") (o ^. rabbitmq)
  pure $! Env (RequestId defRequestId) o l n p pgPool a io mtbs rabbitMqChannelMVar

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}
