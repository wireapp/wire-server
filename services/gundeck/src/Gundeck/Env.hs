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
import Data.Misc (Milliseconds (..))
import Data.Time.Clock.POSIX
import Gundeck.Aws qualified as Aws
import Gundeck.Options as Opt hiding (host, port)
import Gundeck.ThreadBudget
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended (mkRabbitMqChannelMVar)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Logger.Extended qualified as Logger

data Env = Env
  { _reqId :: !RequestId,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rabbitmqChannel :: !(MVar Q.Channel),
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState)
  }

makeLenses ''Env

schemaVersion :: Int32
schemaVersion = 7

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
  rabbit <- mkRabbitMqChannelMVar l o._rabbitmq
  pure $! Env (RequestId "N/A") o l n p rabbit a io mtbs

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}
