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
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Database.Redis as Redis
import qualified Gundeck.Aws as Aws
import Gundeck.Options as Opt
import Gundeck.ThreadBudget
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified System.Logger as Log
import qualified System.Logger.Extended as Logger
import Util.Options

data Env = Env
  { _reqId :: !RequestId,
    _monitor :: !Metrics,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rstate :: !Redis.Connection,
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
        { managerConnCount = o ^. optSettings . setHttpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. optSettings . setHttpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }

  let redisConnInfo =
        Redis.defaultConnectInfo
          { Redis.connectHost = unpack $ o ^. optRedis . rHost,
            Redis.connectPort = Redis.PortNumber (fromIntegral $ o ^. optRedis . rPort),
            Redis.connectTimeout = Just (secondsToNominalDiffTime 5),
            Redis.connectMaxConnections = 100
          }

  -- TODO: local tests with this function and a redis in cluster mode sometimes leads to
  -- a timeout after a long time (2 min) for unknown reasons, even though redis pods are
  -- up. To Reproduce, open a telepresence to a k8s with a clustered redis; set the
  -- following; then run gudeck process once now; kill it; then run it again after
  -- having deleted some or all redis pods. Even once they are back up and healthy, the
  -- error can be observed in some cases:
  --
  --  redis:
  -- -  host: 127.0.0.1
  -- +  host: redis-cluster-leader-headless
  --    port: 6379
  -- -  connectionMode: "master" # master | sentinel | cluster
  -- +  connectionMode: cluster
  --
  -- TODO: should we wrap the checkedConnectCluster in a shorter 'timeout' so as not to
  -- potentially have to wait 2 minutes when there's a connection problem?
  --
  -- Example observed error after 2 minutes:
  --
  --   ❯ ./dist/gundeck -c services/gundeck/gundeck.integration.yaml
  --   I, starting connection to redis..., connectionMode=Cluster, connInfo=ConnInfo {connectHost = "redis-cluster-leader-headless", connectPort = PortNumber 6379, connectAuth = Nothing, connectDatabase = 0, connectMaxConnections = 100, connectMaxIdleTime = 30s, connectTimeout = Just 5s, connectTLSParams = Nothing}
  --   I, starting connection to redis in cluster mode ...
  --   I, lazy connection established, running ping...

  --   gundeck: Network.Socket.connect: <socket: 12>: timeout (Connection timed out)
  --   ~/Documents/git/wire-server hedis-redis*  2m 11s
  --
  -- In some other cases, a timeout is quicker:
  --
  --   I, starting connection to redis in cluster mode ...
  --   gundeck: ConnectTimeout PhaseUnknown
  Log.info l $
    Log.msg (Log.val "starting connection to redis...")
      . Log.field "connectionMode" (show $ o ^. optRedis . rConnectionMode)
      . Log.field "connInfo" (show redisConnInfo)
  r <- case o ^. optRedis . rConnectionMode of
    Master -> Redis.checkedConnect redisConnInfo
    Cluster -> checkedConnectCluster l redisConnInfo
  Log.info l $ Log.msg (Log.val "Established connection to redis")
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
        . C.setPolicy (C.dcFilterPolicyIfConfigured l (o ^. optCassandra . casFilterNodesByDatacentre))
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

-- | Similarly to 'checkedConnect' but for redis cluster:
-- Constructs a 'Connection' pool to a Redis server designated by the
-- given 'ConnectInfo', then tests if the server is actually there.
-- Throws an exception if the connection to the Redis server can't be
-- established.
--
-- Throws 'gundeck: ClusterConnectError (Error "ERR This instance has cluster support disabled")' when the redis server doesn't support cluster mode.
checkedConnectCluster :: Logger.Logger -> Redis.ConnectInfo -> IO Redis.Connection
checkedConnectCluster l connInfo = do
  Log.info l $ Log.msg (Log.val "starting connection to redis in cluster mode ...")
  conn <- Redis.connectCluster connInfo
  Log.info l $ Log.msg (Log.val "lazy connection established, running ping...")
  void . Redis.runRedis conn $ do
    ping <- Redis.ping
    case ping of
      Left r -> error ("could not ping redis cluster: " <> show r)
      Right _ -> pure ()
  Log.info l $ Log.msg (Log.val "ping went through")
  return conn
