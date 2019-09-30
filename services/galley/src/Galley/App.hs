{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

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

    , ExtEnv (..)
    , extGetManager

      -- * Galley monad
    , Galley
    , runGalley
    , evalGalley
    , ask

    , DeleteItem (..)

      -- * Utilities
    , ifNothing
    , fromJsonBody
    , fromProtoBody
    , initExtEnv
    ) where

import Imports
import Bilge hiding (Request, header, statusCode, options)
import Bilge.RPC
import Cassandra hiding (Set)
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (tryJust)
import Data.Aeson (FromJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Default (def)
import Data.Id (TeamId, UserId, ConnId)
import Data.Metrics.Middleware
import Data.Misc (Fingerprint, Rsa)
import Data.Serialize.Get (runGetLazy)
import Data.Text (unpack)
import Galley.Options
import Galley.API.Error
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai
import Network.Wai.Utilities
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.Session as Ssl
import Ssl.Util
import System.Logger.Class hiding (Error, info)
import Util.Options

import qualified Cassandra                as C
import qualified Cassandra.Settings       as C
import qualified Data.List.NonEmpty       as NE
import qualified Data.ProtocolBuffers     as Proto
import qualified Galley.Aws               as Aws
import qualified Galley.Queue             as Q
import qualified OpenSSL.X509.SystemStore as Ssl
import qualified System.Logger.Extended   as Logger

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
    { _extGetManager :: (Manager, [Fingerprint Rsa] -> Ssl.SSL -> IO ())
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

instance MonadUnliftIO Galley where
    askUnliftIO =
        Galley $ ReaderT $ \r ->
        withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . flip runReaderT r . unGalley))

instance MonadLogger Galley where
    log l m = do
        e <- ask
        Logger.log (e^.applog) l (reqIdMsg (e^.reqId) . m)

instance MonadHttp Galley where
    handleRequestWithCont req handler = do
        httpManager <- view manager
        liftIO $ withResponse req httpManager handler


instance HasRequestId Galley where
    getRequestId = view reqId

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
    l   <- Logger.mkLogger (o ^. optLogLevel) (o ^. optLogNetStrings) (o ^. optLogFormat)
    mgr <- initHttpManager o
    Env def m o l mgr <$> initCassandra o l
                      <*> Q.new 16000
                      <*> initExtEnv
                      <*> maybe (return Nothing) (fmap Just . Aws.mkEnv l mgr) (o^.optJournal)

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra o l = do
    c <- maybe (C.initialContactsPlain (o^.optCassandra.casEndpoint.epHost))
               (C.initialContactsDisco "cassandra_galley")
               (unpack <$> o^.optDiscoUrl)
    C.init
            . C.setLogger (C.mkLogger (Logger.clone (Just "cassandra.galley") l))
            . C.setContacts (NE.head c) (NE.tail c)
            . C.setPortNumber (fromIntegral $ o^.optCassandra.casEndpoint.epPort)
            . C.setKeyspace (Keyspace $ o^.optCassandra.casKeyspace)
            . C.setMaxConnections 4
            . C.setMaxStreams 128
            . C.setPoolStripes 4
            . C.setSendTimeout 3
            . C.setResponseTimeout 10
            . C.setProtocolVersion C.V4
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
    newManager (opensslManagerSettings (pure ctx))
        { managerResponseTimeout     = responseTimeoutMicro 10000000
        , managerConnCount           = o^.optSettings.setHttpPoolSize
        , managerIdleConnectionCount = 3 * (o^.optSettings.setHttpPoolSize)
        }

-- TODO: somewhat duplicates Brig.App.initExtGetManager
initExtEnv :: IO ExtEnv
initExtEnv = do
    ctx <- Ssl.context
    Ssl.contextSetVerificationMode ctx Ssl.VerifyNone
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
    Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
    Ssl.contextSetCiphers ctx rsaCiphers
    Ssl.contextLoadSystemCerts ctx
    mgr <- newManager (opensslManagerSettings (pure ctx))
        { managerResponseTimeout = responseTimeoutMicro 10000000
        , managerConnCount       = 100
        }
    Just sha <- getDigestByName "SHA256"
    return $ ExtEnv (mgr, mkVerify sha)
  where
    mkVerify sha fprs =
        let pinset = map toByteString' fprs
        in  verifyRsaFingerprint sha pinset

runGalley :: Env -> Request -> Galley ResponseReceived -> IO ResponseReceived
runGalley e r m =
    let e' = reqId .~ lookupReqId r $ e
    in evalGalley e' m

evalGalley :: Env -> Galley a -> IO a
evalGalley e m = runClient (e^.cstate) (runReaderT (unGalley m) e)

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

fromJsonBody :: FromJSON a => JsonRequest a -> Galley a
fromJsonBody r = exceptT (throwM . invalidPayload) return (parseBody r)
{-# INLINE fromJsonBody #-}

fromProtoBody :: Proto.Decode a => Request -> Galley a
fromProtoBody r = do
    b <- readBody r
    either (throwM . invalidPayload . fromString) return (runGetLazy Proto.decodeMessage b)
{-# INLINE fromProtoBody #-}

ifNothing :: Error -> Maybe a -> Galley a
ifNothing e = maybe (throwM e) return
{-# INLINE ifNothing #-}
