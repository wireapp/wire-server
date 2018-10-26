{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gundeck.Env where

import Imports
import Bilge
import Cassandra (ClientState, Keyspace (..))
import Control.AutoUpdate
import Control.Lens ((^.), makeLenses)
import Data.Metrics.Middleware (Metrics)
import Data.Misc (Milliseconds (..))
import Data.Text (unpack)
import Data.Time.Clock.POSIX
import Util.Options
import Gundeck.Options as Opt
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL.EVP.Cipher (Cipher, getCipherByName)
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import System.Logger.Class hiding (Error, info)

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import qualified Database.Redis.IO as Redis
import qualified Data.List.NonEmpty as NE
import qualified Gundeck.Aws as Aws
import qualified Gundeck.Push.Native.Fallback.Queue as Fallback
import qualified System.Logger as Logger

data Env = Env
    { _reqId   :: !RequestId
    , _monitor :: !Metrics
    , _options :: !Opts
    , _applog  :: !Logger
    , _manager :: !Manager
    , _cstate  :: !ClientState
    , _rstate  :: !Redis.Pool
    , _awsEnv  :: !Aws.Env
    , _digest  :: !Digest
    , _cipher  :: !Cipher
    , _fbQueue :: !Fallback.Queue
    , _time    :: !(IO Milliseconds)
    }

makeLenses ''Env

schemaVersion :: Int32
schemaVersion = 7

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    c <- maybe (C.initialContactsPlain (o^.optCassandra.casEndpoint.epHost))
               (C.initialContactsDisco "cassandra_gundeck")
               (unpack <$> o^.optDiscoUrl)
    n <- newManager tlsManagerSettings
            { managerConnCount           = (o^.optSettings.setHttpPoolSize)
            , managerIdleConnectionCount = 3 * (o^.optSettings.setHttpPoolSize)
            , managerResponseTimeout     = responseTimeoutMicro 5000000
            }
    r <- Redis.mkPool (Logger.clone (Just "redis.gundeck") l) $
              Redis.setHost (unpack $ o^.optRedis.epHost)
            . Redis.setPort (o^.optRedis.epPort)
            . Redis.setMaxConnections 100
            . Redis.setPoolStripes 4
            . Redis.setConnectTimeout 3
            . Redis.setSendRecvTimeout 5
            $ Redis.defSettings
    p <- C.init (Logger.clone (Just "cassandra.gundeck") l) $
              C.setContacts (NE.head c) (NE.tail c)
            . C.setPortNumber (fromIntegral $ o^.optCassandra.casEndpoint.epPort)
            . C.setKeyspace (Keyspace (o^.optCassandra.casKeyspace))
            . C.setMaxConnections 4
            . C.setMaxStreams 128
            . C.setPoolStripes 4
            . C.setSendTimeout 3
            . C.setResponseTimeout 10
            . C.setProtocolVersion C.V3
            $ C.defSettings
    a <- Aws.mkEnv l o n
    dg <- getDigestByName "SHA256" >>= maybe (error "OpenSSL: SHA256 digest not found") return
    ci <- getCipherByName "AES-256-CBC" >>= maybe (error "OpenSSL: AES-256-CBC cipher not found") return
    qu <- initFallbackQueue o
    io <- mkAutoUpdate defaultUpdateSettings {
            updateAction = Ms . round . (* 1000) <$> getPOSIXTime
    }
    return $! Env mempty m o l n p r a dg ci qu io

initFallbackQueue :: Opts -> IO Fallback.Queue
initFallbackQueue o =
    let delay = Fallback.Delay (o^.optFallback.fbQueueDelay)
        limit = Fallback.Limit (o^.optFallback.fbQueueLimit)
        burst = Fallback.Burst (o^.optFallback.fbQueueBurst)
    in Fallback.newQueue delay limit burst

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}
