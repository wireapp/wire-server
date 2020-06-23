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

module Gundeck.Env where

import Bilge
import Cassandra (ClientState, Keyspace (..))
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.AutoUpdate
import Control.Lens (makeLenses, (^.))
import Data.Default (def)
import qualified Data.List.NonEmpty as NE
import Data.Metrics.Middleware (Metrics)
import Data.Misc (Milliseconds (..))
import Data.Text (unpack)
import Data.Time.Clock.POSIX
import qualified Database.Redis.IO as Redis
import qualified Gundeck.Aws as Aws
import Gundeck.Options as Opt
import Gundeck.ThreadBudget
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified System.Logger.Extended as Logger
import Util.Options

data Env = Env
  { _reqId :: !RequestId,
    _monitor :: !Metrics,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rstate :: !Redis.Pool,
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState)
  }

makeLenses ''Env

schemaVersion :: Int32
schemaVersion = 7

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
  l <- Logger.mkLogger (o ^. optLogLevel) (o ^. optLogNetStrings) (o ^. optLogFormat)
  c <-
    maybe
      (C.initialContactsPlain (o ^. optCassandra . casEndpoint . epHost))
      (C.initialContactsDisco "cassandra_gundeck")
      (unpack <$> o ^. optDiscoUrl)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = (o ^. optSettings . setHttpPoolSize),
          managerIdleConnectionCount = 3 * (o ^. optSettings . setHttpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }
  r <-
    Redis.mkPool (Logger.clone (Just "redis.gundeck") l) $
      Redis.setHost (unpack $ o ^. optRedis . epHost)
        . Redis.setPort (o ^. optRedis . epPort)
        . Redis.setMaxConnections 100
        . Redis.setPoolStripes 4
        . Redis.setConnectTimeout 3
        . Redis.setSendRecvTimeout 5
        $ Redis.defSettings
  p <-
    C.init $
      C.setLogger (C.mkLogger (Logger.clone (Just "cassandra.gundeck") l))
        . C.setContacts (NE.head c) (NE.tail c)
        . C.setPortNumber (fromIntegral $ o ^. optCassandra . casEndpoint . epPort)
        . C.setKeyspace (Keyspace (o ^. optCassandra . casKeyspace))
        . C.setMaxConnections 4
        . C.setMaxStreams 128
        . C.setPoolStripes 4
        . C.setSendTimeout 3
        . C.setResponseTimeout 10
        . C.setProtocolVersion C.V4
        $ C.defSettings
  a <- Aws.mkEnv l o n
  io <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = Ms . round . (* 1000) <$> getPOSIXTime
        }
  mtbs <- mkThreadBudgetState `mapM` (o ^. optSettings . setMaxConcurrentNativePushes)
  return $! Env def m o l n p r a io mtbs

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}
