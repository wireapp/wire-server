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
import Cassandra (ClientState, Keyspace (..))
import Cassandra qualified as C
import Cassandra.Settings qualified as C
import Control.AutoUpdate
import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses, (^.))
import Control.Retry (capDelay, exponentialBackoff)
import Data.List.NonEmpty qualified as NE
import Data.Metrics.Middleware (Metrics)
import Data.Misc (Milliseconds (..))
import Data.Text (unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Redis qualified as Redis
import Gundeck.Aws qualified as Aws
import Gundeck.Options as Opt hiding (host, port)
import Gundeck.Options qualified as O
import Gundeck.Redis qualified as Redis
import Gundeck.Redis.HedisExtensions qualified as Redis
import Gundeck.ThreadBudget
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL.Session qualified as OpenSSL
import System.Logger qualified as Log
import System.Logger.Extended qualified as Logger
import Util.Options

data Env = Env
  { _reqId :: !RequestId,
    _monitor :: !Metrics,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rstate :: !Redis.RobustConnection,
    _rstateAdditionalWrite :: !(Maybe Redis.RobustConnection),
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState)
  }

makeLenses ''Env

schemaVersion :: Int32
schemaVersion = 7

createEnv :: Metrics -> Opts -> IO ([Async ()], Env)
createEnv m o = do
  l <- Logger.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  c <-
    maybe
      (C.initialContactsPlain (o ^. cassandra . endpoint . host))
      (C.initialContactsDisco "cassandra_gundeck" . unpack)
      (o ^. discoUrl)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = o ^. settings . httpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }

  (rThread, r) <- createRedisPool l (o ^. redis) "main-redis"

  (rAdditionalThreads, rAdditional) <- case o ^. redisAdditionalWrite of
    Nothing -> pure ([], Nothing)
    Just additionalRedis -> do
      (rAddThread, rAdd) <- createRedisPool l additionalRedis "additional-write-redis"
      pure ([rAddThread], Just rAdd)

  mbSSLContext <- createSSLContext (o ^. cassandra)
  let basicCasSettings =
        C.setLogger (C.mkLogger (Logger.clone (Just "cassandra.gundeck") l))
          . C.setContacts (NE.head c) (NE.tail c)
          . C.setPortNumber (fromIntegral $ o ^. cassandra . endpoint . port)
          . C.setKeyspace (Keyspace (o ^. cassandra . keyspace))
          . C.setMaxConnections 4
          . C.setMaxStreams 128
          . C.setPoolStripes 4
          . C.setSendTimeout 3
          . C.setResponseTimeout 10
          . C.setProtocolVersion C.V4
          . C.setPolicy (C.dcFilterPolicyIfConfigured l (o ^. cassandra . filterNodesByDatacentre))
          $ C.defSettings
      casSettings = maybe basicCasSettings (\sslCtx -> C.setSSLContext sslCtx basicCasSettings) mbSSLContext

  p <- C.init casSettings
  a <- Aws.mkEnv l o n
  io <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = Ms . round . (* 1000) <$> getPOSIXTime
        }
  mtbs <- mkThreadBudgetState `mapM` (o ^. settings . maxConcurrentNativePushes)
  pure $! (rThread : rAdditionalThreads,) $! Env (RequestId "N/A") m o l n p r rAdditional a io mtbs
  where
    createSSLContext :: CassandraOpts -> IO (Maybe OpenSSL.SSLContext)
    createSSLContext cassOpts
      | cassOpts ^. useTLS = do
          sslContext <- OpenSSL.context
          maybe (pure ()) (OpenSSL.contextSetCAFile sslContext) (cassOpts ^. tlsCert)
          OpenSSL.contextSetVerificationMode
            sslContext
            OpenSSL.VerifyPeer
              { vpFailIfNoPeerCert = False,
                vpClientOnce = True,
                vpCallback = Nothing
              }
          pure $ Just sslContext
      | otherwise = pure Nothing

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}

createRedisPool :: Logger.Logger -> RedisEndpoint -> ByteString -> IO (Async (), Redis.RobustConnection)
createRedisPool l ep identifier = do
  let redisConnInfo =
        Redis.defaultConnectInfo
          { Redis.connectHost = unpack $ ep ^. O.host,
            Redis.connectPort = Redis.PortNumber (fromIntegral $ ep ^. O.port),
            Redis.connectTimeout = Just (secondsToNominalDiffTime 5),
            Redis.connectMaxConnections = 100
          }

  Log.info l $
    Log.msg (Log.val $ "starting connection to " <> identifier <> "...")
      . Log.field "connectionMode" (show $ ep ^. O.connectionMode)
      . Log.field "connInfo" (show redisConnInfo)
  let connectWithRetry = Redis.connectRobust l (capDelay 1000000 (exponentialBackoff 50000))
  r <- case ep ^. O.connectionMode of
    Master -> connectWithRetry $ Redis.checkedConnect redisConnInfo
    Cluster -> connectWithRetry $ Redis.checkedConnectCluster redisConnInfo
  Log.info l $ Log.msg (Log.val $ "Established connection to " <> identifier <> ".")
  pure r
