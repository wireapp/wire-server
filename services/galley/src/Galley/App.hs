{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Galley.App
    ( -- * Environment
      Env
    , reqId
    , monitor
    , options
    , applog
    , manager
    , cstate
    , deleteQueue
    , createEnv
    , extEnv
    , aEnv

    , ExtEnv
    , extGetManager

      -- * Galley monad
    , Galley
    , runGalley
    , evalGalley
    , ask

    , DeleteItem (..)

      -- * Utilities
    , ifNothing
    , fromBody
    , fromProtoBody
    ) where

import Bilge hiding (Request, header, statusCode, options)
import Bilge.RPC
import Cassandra hiding (Error, Set)
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch hiding (tryJust)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Id (TeamId, UserId, ConnId)
import Data.Metrics.Middleware
import Data.Misc (Fingerprint, Rsa)
import Data.Serialize.Get (runGetLazy)
import Data.String (fromString)
import Galley.Options
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai
import Network.Wai.Utilities
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.Session as Ssl
import System.Logger.Class hiding (Error, info)

import qualified Cassandra                as C
import qualified Cassandra.Settings       as C
import qualified Data.List.NonEmpty       as NE
import qualified Data.ProtocolBuffers     as Proto
import qualified Data.Text.Lazy           as Lazy
import qualified Galley.Aws               as Aws
import qualified Galley.Queue             as Q
import qualified OpenSSL.X509.SystemStore as Ssl
import qualified System.Logger            as Logger

data DeleteItem = TeamItem TeamId UserId (Maybe ConnId)
    deriving (Eq, Ord, Show)

-- | Main application environment.
data Env = Env
    { _reqId       :: RequestId
    , _monitor     :: Metrics
    , _options     :: Opts
    , _applog      :: Logger
    , _manager     :: Manager
    , _cstate      :: ClientState
    , _deleteQueue :: Q.Queue DeleteItem
    , _extEnv      :: ExtEnv
    , _aEnv        :: Maybe Aws.Env
    }

-- | Environment specific to the communication with external
-- service providers.
data ExtEnv = ExtEnv
    { _extGetManager :: [Fingerprint Rsa] -> Manager
    }

makeLenses ''Env
makeLenses ''ExtEnv

newtype Galley a = Galley
    { unGalley :: ReaderT Env Client a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadClient
               )

instance MonadBase IO Galley where
    liftBase = liftIO

instance MonadBaseControl IO Galley where
    type StM Galley a = StM (ReaderT Env Client) a
    liftBaseWith f    = Galley $ liftBaseWith $ \run -> f (run . unGalley)
    restoreM          = Galley . restoreM

instance MonadLogger Galley where
    log l m = do
        e <- ask
        Logger.log (e^.applog) l (reqIdMsg (e^.reqId) . m)

instance MonadHttp Galley where
    getManager = view manager

instance HasRequestId Galley where
    getRequestId = view reqId

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
    l   <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    mgr <- initHttpManager o
    Env mempty m o l mgr <$> initCassandra o l
                         <*> Q.new 16000
                         <*> initExtEnv
                         <*> maybe (return Nothing) (fmap Just . Aws.mkEnv l mgr) (o^.journalOpts)

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra o l = do
    c <- maybe (return $ NE.fromList [o^.cassHost])
               (C.initialContacts "cassandra_galley")
               (o^.discoUrl)
    C.init (Logger.clone (Just "cassandra.galley") l) $
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

initHttpManager :: Opts -> IO Manager
initHttpManager o = do
    ctx <- Ssl.context
    Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
    Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
    Ssl.contextSetCiphers ctx rsaCiphers
    Ssl.contextLoadSystemCerts ctx
    newManager (opensslManagerSettings ctx)
        { managerResponseTimeout     = responseTimeoutMicro 10000000
        , managerConnCount           = o^.httpPoolSz
        , managerIdleConnectionCount = 3 * (o^.httpPoolSz)
        }

initExtEnv :: IO ExtEnv
initExtEnv = do
    ctx <- Ssl.context
    Ssl.contextSetVerificationMode ctx Ssl.VerifyNone
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
    Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
    Ssl.contextSetCiphers ctx rsaCiphers
    Ssl.contextLoadSystemCerts ctx
    mgr <- newManager (opensslManagerSettings ctx)
        { managerResponseTimeout = responseTimeoutMicro 10000000
        , managerConnCount       = 100
        }
    Just sha <- getDigestByName "SHA256"
    return $ ExtEnv (mkManager ctx sha mgr)
  where
    mkManager ctx sha mgr fprs =
        let pinset = map toByteString' fprs
        in setOnConnection ctx (verifyRsaFingerprint sha pinset) mgr

runGalley :: Env -> Request -> Galley ResponseReceived -> IO ResponseReceived
runGalley e r m =
    let e' = reqId .~ lookupReqId r $ e
    in evalGalley e' m

evalGalley :: Env -> Galley a -> IO a
evalGalley e m = runClient (e^.cstate) (runReaderT (unGalley m) e)

lookupReqId :: Request -> RequestId
lookupReqId = maybe mempty RequestId . lookup requestIdName . requestHeaders

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

fromBody :: FromJSON a => Request -> (Lazy.Text -> Error) -> Galley a
fromBody r f = exceptT (throwM . f) return (parseBody r)
{-# INLINE fromBody #-}

fromProtoBody :: Proto.Decode a => Request -> (Lazy.Text -> Error) -> Galley a
fromProtoBody r f = do
    b <- readBody r
    either (throwM . f . fromString) return (runGetLazy Proto.decodeMessage b)
{-# INLINE fromProtoBody #-}

ifNothing :: Error -> Maybe a -> Galley a
ifNothing e = maybe (throwM e) return
{-# INLINE ifNothing #-}
