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
import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses, (^.))
import Control.Retry (capDelay, exponentialBackoff)
import Data.ByteString.Char8 qualified as BSChar8
import Data.Id
import Data.Misc (Milliseconds (..))
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.X509.CertificateStore as CertStore
import Database.Redis qualified as Redis
import Gundeck.Aws qualified as Aws
import Gundeck.Options as Opt hiding (host, port)
import Gundeck.Options qualified as O
import Gundeck.Redis qualified as Redis
import Gundeck.Redis.HedisExtensions qualified as Redis
import Gundeck.ThreadBudget
import Imports
import Network.AMQP (Channel)
import Network.AMQP.Extended qualified as Q
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.TLS as TLS
import Network.TLS.Extra qualified as TLS
import System.Logger qualified as Log
import System.Logger.Extended qualified as Logger

data Env = Env
  { _reqId :: !RequestId,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rstate :: !Redis.RobustConnection,
    _rstateAdditionalWrite :: !(Maybe Redis.RobustConnection),
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState),
    _rabbitMqChannel :: MVar Channel
  }

makeLenses ''Env

createEnv :: Opts -> IO ([Async ()], Env)
createEnv o = do
  l <- Logger.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = o ^. settings . httpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }

  redisUsername <- BSChar8.pack <$$> lookupEnv "REDIS_USERNAME"
  redisPassword <- BSChar8.pack <$$> lookupEnv "REDIS_PASSWORD"
  (rThread, r) <- createRedisPool l (o ^. redis) redisUsername redisPassword "main-redis"

  (rAdditionalThreads, rAdditional) <- case o ^. redisAdditionalWrite of
    Nothing -> pure ([], Nothing)
    Just additionalRedis -> do
      additionalRedisUsername <- BSChar8.pack <$$> lookupEnv "REDIS_ADDITIONAL_WRITE_USERNAME"
      addtionalRedisPassword <- BSChar8.pack <$$> lookupEnv "REDIS_ADDITIONAL_WRITE_PASSWORD"
      (rAddThread, rAdd) <- createRedisPool l additionalRedis additionalRedisUsername addtionalRedisPassword "additional-write-redis"
      pure ([rAddThread], Just rAdd)

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
  rabbitMqChannelMVar <- Q.mkRabbitMqChannelMVar l "gundeck" (o ^. rabbitmq)
  pure $! (rThread : rAdditionalThreads,) $! Env (RequestId defRequestId) o l n p r rAdditional a io mtbs rabbitMqChannelMVar

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}

createRedisPool :: Logger.Logger -> RedisEndpoint -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Async (), Redis.RobustConnection)
createRedisPool l ep username password identifier = do
  customCertStore <- case ep._tlsCa of
    Nothing -> pure Nothing
    Just caPath -> CertStore.readCertificateStore caPath
  let defClientParams = defaultParamsClient (Text.unpack ep._host) ""
      tlsParams =
        guard ep._enableTls
          $> defClientParams
            { clientHooks =
                if ep._insecureSkipVerifyTls
                  then defClientParams.clientHooks {onServerCertificate = \_ _ _ _ -> pure []}
                  else defClientParams.clientHooks,
              clientShared =
                case customCertStore of
                  Nothing -> defClientParams.clientShared
                  Just sharedCAStore -> defClientParams.clientShared {sharedCAStore},
              clientSupported =
                defClientParams.clientSupported
                  { supportedVersions = [TLS.TLS13, TLS.TLS12],
                    supportedCiphers = TLS.ciphersuite_strong
                  }
            }
  let redisConnInfo =
        Redis.defaultConnectInfo
          { Redis.connectHost = Text.unpack $ ep ^. O.host,
            Redis.connectPort = Redis.PortNumber (fromIntegral $ ep ^. O.port),
            Redis.connectUsername = username,
            Redis.connectAuth = password,
            Redis.connectTimeout = Just (secondsToNominalDiffTime 5),
            Redis.connectMaxConnections = 100,
            Redis.connectTLSParams = tlsParams
          }

  Log.info l $
    Log.msg (Log.val $ "starting connection to " <> identifier <> "...")
      . Log.field "connectionMode" (show $ ep ^. O.connectionMode)
      . Log.field "connInfo" (safeShowConnInfo redisConnInfo)
  let connectWithRetry = Redis.connectRobust l (capDelay 1000000 (exponentialBackoff 50000))
  r <- case ep ^. O.connectionMode of
    Master -> connectWithRetry $ Redis.checkedConnect redisConnInfo
    Cluster -> connectWithRetry $ Redis.checkedConnectCluster redisConnInfo
  Log.info l $ Log.msg (Log.val $ "Established connection to " <> identifier <> ".")
  pure r

safeShowConnInfo :: Redis.ConnectInfo -> String
safeShowConnInfo connInfo = show $ connInfo {Redis.connectAuth = "[REDACTED]" <$ Redis.connectAuth connInfo}
