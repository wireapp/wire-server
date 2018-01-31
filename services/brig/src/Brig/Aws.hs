{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Brig.AWS
    ( -- * Monad
      Env
    , mkEnv
    , Amazon
    , amazonkaAwsEnv
    , execute
    , sesQueue
    , internalQueue
    -- , sqsEnv

    , listen
    , enqueue
    -- , dynamoEnv
    , dynamoBlacklistTable
    , dynamoPrekeyTable

    , exec
    , execCatch

    , sendMail
    ) where

import Blaze.ByteString.Builder (toLazyByteString)
import Bilge.Retry (httpHandlers)
import Brig.Options as Opt
import Control.Applicative
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Concurrent.Lifted (threadDelay)
import Control.Error hiding (err)
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry (recovering, retrying, limitRetries, exponentialBackoff)
import Data.Aeson hiding ((.=))
import Data.Attoparsec.Text
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Id
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Data.Yaml (FromJSON (..))
import Network.AWS (AWSRequest, Rs, Region (..))
import Network.AWS (serviceCode, serviceMessage, serviceAbbrev, serviceStatus)
import Network.AWS.Data
import Network.AWS.SQS (rmrsMessages)
import Network.AWS.SQS.Types
import Network.HTTP.Client (Manager, HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types
import Network.Mail.Mime
import System.Logger.Class

import qualified Network.AWS.DynamoDB    as Aws
import qualified Control.Exception.Lens  as EL
import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Base64  as B64
import qualified Data.HashMap.Strict     as Map
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LT
import qualified Gundeck.Types.Push      as Push
import qualified Network.AWS             as Aws
import qualified Network.AWS.Env         as Aws
import qualified Network.AWS.Data        as Aws
import qualified Network.AWS.SES         as Ses
import qualified Network.AWS.SES.Types   as Ses
import qualified Network.AWS.SNS         as Sns
import qualified Network.AWS.SQS         as Sqs
import qualified Network.AWS.DynamoDB    as Ddb
import qualified Network.TLS             as TLS
import qualified System.Logger           as Logger

newtype QueueUrl = QueueUrl Text deriving Show

newtype Account  = Account { fromAccount :: Text } deriving (Eq, Show, ToText, FromJSON)

data Env = Env
    { _logger               :: !Logger
    , _account              :: !Account
    , _sesQueue             :: !QueueUrl
    , _internalQueue        :: !QueueUrl
    , _dynamoBlacklistTable :: !Text
    , _dynamoPrekeyTable    :: !Text
    , _amazonkaAwsEnv       :: !Aws.Env
    }

makeLenses ''Env

newtype Amazon a = Amazon
    { unAmazon :: ReaderT Env (ResourceT IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadBase IO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadResource
               )

instance MonadLogger Amazon where
    log l m = view logger >>= \g -> Logger.log g l m

instance MonadBaseControl IO Amazon where
    type StM Amazon a = StM (ReaderT Env (ResourceT IO)) a
    liftBaseWith    f = Amazon $ liftBaseWith $ \run -> f (run . unAmazon)
    restoreM          = Amazon . restoreM

instance Aws.MonadAWS Amazon where
    liftAWS a = view amazonkaAwsEnv >>= flip Aws.runAWS a

mkEnv :: Logger -> AWSOpts -> Manager -> IO Env
mkEnv lgr opts mgr = do
    let g = Logger.clone (Just "aws.brig") lgr
    let (bl, pk) = (amazonkaBlacklistTable opts, amazonkaPrekeyTable opts)
    e  <- mkAwsEnv g sesEnd sqsEnd dynEnd
    sq <- getQueueUrl e (amazonkaSesQueue opts)
    iq <- getQueueUrl e (amazonkaInternalQueue opts)
    return (Env g (Account (amazonkaAccount opts)) sq iq bl pk e)
  where
    sesEnd = Aws.setEndpoint True "email.eu-west-1.amazonaws.com" 443 Ses.ses
    sqsEnd = Aws.setEndpoint True "sqs.eu-west-1.amazonaws.com" 443 Sqs.sqs
    dynEnd = Aws.setEndpoint True "dynamodb.eu-west-1.amazonaws.com" 443 Ddb.dynamoDB

    mkAwsEnv g ses sqs dyn =  set Aws.envLogger (awsLogger g)
                           .  set Aws.envRegion Aws.Ireland -- TODO: Necessary?
                          <$> Aws.newEnvWith Aws.Discover Nothing mgr
                          <&> Aws.configure ses
                          <&> Aws.configure sqs
                          <&> Aws.configure dyn

    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString

    mapLevel Aws.Info  = Logger.Info
    -- Debug output from amazonka can be very useful for tracing requests
    -- but is very verbose (and multiline which we don't handle well)
    -- distracting from our own debug logs, so we map amazonka's 'Debug'
    -- level to our 'Trace' level.
    mapLevel Aws.Debug = Logger.Trace
    mapLevel Aws.Trace = Logger.Trace
    -- n.b. Errors are either returned or thrown. In both cases they will
    -- already be logged if left unhandled. We don't want errors to be
    -- logged inside amazonka already, before we even had a chance to handle
    -- them, which results in distracting noise. For debugging purposes,
    -- they are still revealed on debug level.
    mapLevel Aws.Error = Logger.Debug

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
    GeneralError :: (Show e, Aws.AsError e) => e -> Error

deriving instance Show     Error
deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- SQS

listen :: (FromJSON a, Show a) => Text -> (a -> IO ()) -> Amazon ()
listen qn callback = do
    env <- view amazonkaAwsEnv
    QueueUrl url <- liftIO $ getQueueUrl env qn
    forever $ handleAny unexpectedError $ do
        msgs <- view rmrsMessages <$> send (receive url)
        void $ mapConcurrently (onMessage url) msgs
  where
    receive url =
        Sqs.receiveMessage url
            & set Sqs.rmWaitTimeSeconds (Just 20)
            . set Sqs.rmMaxNumberOfMessages (Just 10)

    onMessage url m =
        case decodeStrict =<< Text.encodeUtf8 <$> m^.mBody of
            Nothing -> err $ msg ("Failed to parse SQS event: " ++ show m)
            Just  n -> do
                debug $ msg ("Received SQS event: " ++ show n)
                liftIO $ callback n
                for_ (m^.mReceiptHandle) (void . send . Sqs.deleteMessage url)

    unexpectedError x = do
        err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
        threadDelay 3000000

enqueue :: QueueUrl -> BL.ByteString -> Amazon (Sqs.SendMessageResponse)
enqueue (QueueUrl url) m = do
    res <- retrying (limitRetries 5 <> exponentialBackoff 1000000) (const canRetry) $ const (sendCatch (req url))
    either (throwM . GeneralError) return res
  where
    req url = Sqs.sendMessage url $ Text.decodeLatin1 (BL.toStrict m)

--------------------------------------------------------------------------------
-- SES

sendMail :: Mail -> Amazon ()
sendMail m = do
    r <- liftIO $ BL.toStrict <$> renderMail' m
    let raw = Ses.rawMessage r
    let msg = Ses.sendRawEmail raw & Ses.sreDestinations .~ fmap addressEmail (mailTo m)
                                   & Ses.sreSource ?~ addressEmail (mailFrom m)
    void $ retrying retry5x (const canRetry) $ const (sendCatch msg)
  where
    -- TODO: Ensure that we handle SES throttling too
    -- canRetry x = statusIsServerError (sesStatusCode x)
    --           || sesErrorCode x == "Throttling"
    retry5x = limitRetries 5 <> exponentialBackoff 100000

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either Aws.Error (Rs r))
sendCatch = AWST.trying Aws._Error . Aws.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = either (throwM . GeneralError) return =<< sendCatch r

is :: Aws.Abbrev -> Int -> Aws.Error -> Bool
is srv s (Aws.ServiceError e) = srv == e^.serviceAbbrev && s == statusCode (e^.serviceStatus)
is _   _ _                    = False

isTimeout :: MonadIO m => Either Aws.Error a -> m Bool
isTimeout (Right _) = pure False
isTimeout (Left  e) = case e of
    Aws.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
    _                                                           -> pure False

getQueueUrl :: Aws.Env -> Text -> IO QueueUrl
getQueueUrl e q = QueueUrl . view Sqs.gqursQueueURL <$> exec e (Sqs.getQueueURL q)

execCatch :: (AWSRequest a, Aws.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m) => 
        r -> a -> m (Either Aws.Error (Rs a))
execCatch e cmd =
    runResourceT . AWST.runAWST e $ do
        AWST.trying Aws._Error $
            AWST.send cmd

exec :: (AWSRequest a, Aws.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m) => 
        r -> a -> m (Rs a)
exec e cmd = do
    x <- runResourceT . AWST.runAWST e $ do
        AWST.trying Aws._Error $
            AWST.send cmd
    either (throwM . GeneralError) return x

canRetry :: MonadIO m => Either Aws.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left  e) = case e of
    Aws.TransportError (HttpExceptionRequest _ ResponseTimeout)                   -> pure True
    Aws.ServiceError se | se^.Aws.serviceCode == Aws.ErrorCode "RequestThrottled" -> pure True
    _                                                                             -> pure False
