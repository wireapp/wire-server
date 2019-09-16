module Proxy.Env
    ( Env
    , createEnv
    , destroyEnv
    , reqId
    , monitor
    , options
    , applog
    , manager
    , secrets
    ) where

import Imports
import Control.Lens (makeLenses, (^.))
import Data.Configurator
import Data.Configurator.Types
import Data.Default (def)
import Data.Id (RequestId)
import Data.Metrics.Middleware (Metrics)
import Proxy.Options
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified System.Logger.Extended as Logger

data Env = Env
    { _reqId   :: !RequestId
    , _monitor :: !Metrics
    , _options :: !Opts
    , _applog  :: !Logger.Logger
    , _manager :: !Manager
    , _secrets :: !Config
    , _loader  :: !ThreadId
    }

makeLenses ''Env

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
    g <- Logger.mkLogger (o^.logLevel) (o^.logNetStrings) (o^.logFormat)
    n <- newManager tlsManagerSettings
            { managerConnCount           = o^.httpPoolSize
            , managerIdleConnectionCount = 3 * (o^.httpPoolSize)
            , managerResponseTimeout     = responseTimeoutMicro 5000000
            }
    let ac = AutoConfig 60 (reloadError g)
    (c, t) <- autoReload ac [Required $ o^.secretsConfig]
    return $! Env def m o g n c t
  where
    reloadError g x =
        Logger.err g (Logger.msg $ Logger.val "Failed reloading config: " Logger.+++ show x)

destroyEnv :: Env -> IO ()
destroyEnv e = do
    killThread (e^.loader)
    Logger.close (e^.applog)
