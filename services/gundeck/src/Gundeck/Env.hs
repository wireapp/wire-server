{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gundeck.Env where

import Bilge hiding (Request, header, statusCode, options)
import Cassandra (ClientState)
import Control.AutoUpdate
import Control.Lens (makeLenses, (^.))
import Data.Int (Int32)
import Data.Metrics.Middleware (Metrics)
import Data.Time.Clock.POSIX
import Gundeck.Options
import Gundeck.Types.Presence (Milliseconds (..))
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
    c <- maybe (return $ NE.fromList [o^.cassHost])
               (C.initialContacts "cassandra_gundeck")
               (o^.discoUrl)
    n <- newManager tlsManagerSettings
            { managerConnCount           = o^.httpPoolSize
            , managerIdleConnectionCount = 3 * (o^.httpPoolSize)
            , managerResponseTimeout     = responseTimeoutMicro 5000000
            }
    r <- Redis.mkPool (Logger.clone (Just "redis.gundeck") l) $
              Redis.setHost (o^.redisHost)
            . Redis.setPort (o^.redisPort)
            . Redis.setMaxConnections 100
            . Redis.setPoolStripes 4
            . Redis.setConnectTimeout 3
            . Redis.setSendRecvTimeout 5
            $ Redis.defSettings
    p <- C.init (Logger.clone (Just "cassandra.gundeck") l) $
              C.setContacts (NE.head c) (NE.tail c)
            . C.setPortNumber (fromIntegral $ o^.cassPort)
            . C.setKeyspace (o^.keyspace)
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
    let delay = Fallback.Delay (o^.fbQueueDelay)
        limit = Fallback.Limit (o^.fbQueueLimit)
        burst = Fallback.Burst (o^.fbQueueBurst)
    in Fallback.newQueue delay limit burst

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}
