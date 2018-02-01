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
    , amazonkaEnv
    , execute
    , sesQueue
    , internalQueue
    , blacklistTable
    , prekeyTable

      -- * SES
    , sendMail
      -- * SQS
    , listen
    , enqueue

      -- * AWS
    , exec
    , execCatch

    ) where

import Blaze.ByteString.Builder (toLazyByteString)
import Bilge.Retry (httpHandlers)
import Brig.AWS.Types
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
import Control.Retry
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
import qualified Network.AWS             as AWS
import qualified Network.AWS.Env         as AWS
import qualified Network.AWS.Data        as AWS
import qualified Network.AWS.SES         as SES
import qualified Network.AWS.SES.Types   as SES
import qualified Network.AWS.SNS         as SNS
import qualified Network.AWS.SQS         as SQS
import qualified Network.AWS.DynamoDB    as DDB
import qualified Network.TLS             as TLS
import qualified System.Logger           as Logger

data Env = Env
    { _logger         :: !Logger
    , _account        :: !Account
    , _sesQueue       :: !Text
    , _internalQueue  :: !Text
    , _blacklistTable :: !Text
    , _prekeyTable    :: !Text
    , _amazonkaEnv    :: !AWS.Env
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

instance AWS.MonadAWS Amazon where
    liftAWS a = view amazonkaEnv >>= flip AWS.runAWS a

mkEnv :: Logger -> AWSOpts -> Manager -> IO Env
mkEnv lgr opts mgr = do
    let g = Logger.clone (Just "aws.brig") lgr
    let (bl, pk) = (amazonkaBlacklistTable opts, amazonkaPrekeyTable opts)
    e  <- mkAwsEnv g sesEnd sqsEnd dynEnd
    sq <- getQueueUrl e (amazonkaSesQueue opts)
    iq <- getQueueUrl e (amazonkaInternalQueue opts)
    return (Env g (Account (amazonkaAccount opts)) sq iq bl pk e)
  where
    sesEnd = AWS.setEndpoint True "email.eu-west-1.amazonaws.com" 443 SES.ses
    sqsEnd = AWS.setEndpoint True "sqs.eu-west-1.amazonaws.com" 443 SQS.sqs
    dynEnd = AWS.setEndpoint True "dynamodb.eu-west-1.amazonaws.com" 443 DDB.dynamoDB

    mkAwsEnv g ses sqs dyn =  set AWS.envLogger (awsLogger g)
                           .  set AWS.envRegion AWS.Ireland -- TODO: Necessary?
                          <$> AWS.newEnvWith AWS.Discover Nothing mgr
                          <&> AWS.configure ses
                          <&> AWS.configure sqs
                          <&> AWS.configure dyn

    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString

    mapLevel AWS.Info  = Logger.Info
    -- Debug output from amazonka can be very useful for tracing requests
    -- but is very verbose (and multiline which we don't handle well)
    -- distracting from our own debug logs, so we map amazonka's 'Debug'
    -- level to our 'Trace' level.
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    -- n.b. Errors are either returned or thrown. In both cases they will
    -- already be logged if left unhandled. We don't want errors to be
    -- logged inside amazonka already, before we even had a chance to handle
    -- them, which results in distracting noise. For debugging purposes,
    -- they are still revealed on debug level.
    mapLevel AWS.Error = Logger.Debug

    getQueueUrl e q = view SQS.gqursQueueURL <$> exec e (SQS.getQueueURL q)

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
    GeneralError :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show     Error
deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- SQS

listen :: (FromJSON a, Show a) => Text -> (a -> IO ()) -> Amazon ()
listen url callback = do
    forever $ handleAny unexpectedError $ do
        msgs <- view rmrsMessages <$> send receive
        void $ mapConcurrently onMessage msgs
  where
    receive =
        SQS.receiveMessage url
            & set SQS.rmWaitTimeSeconds (Just 20)
            . set SQS.rmMaxNumberOfMessages (Just 10)

    onMessage m =
        case decodeStrict =<< Text.encodeUtf8 <$> m^.mBody of
            Nothing -> err $ msg ("Failed to parse SQS event: " ++ show m)
            Just  n -> do
                debug $ msg ("Received SQS event: " ++ show n)
                liftIO $ callback n
                for_ (m^.mReceiptHandle) (void . send . SQS.deleteMessage url)

    unexpectedError x = do
        err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
        threadDelay 3000000

enqueue :: Text -> BL.ByteString -> Amazon (SQS.SendMessageResponse)
enqueue url m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req = SQS.sendMessage url $ Text.decodeLatin1 (BL.toStrict m)

--------------------------------------------------------------------------------
-- SES

sendMail :: Mail -> Amazon ()
sendMail m = do
    body <- liftIO $ BL.toStrict <$> renderMail' m
    let msg = SES.sendRawEmail (SES.rawMessage body)
            & SES.sreDestinations .~ fmap addressEmail (mailTo m)
            & SES.sreSource ?~ addressEmail (mailFrom m)
    void $ retrying retry5x (const canRetry) $ const (sendCatch msg)
  where
    -- TODO: Ensure that we handle SES throttling too
    -- canRetry x = statusIsServerError (sesStatusCode x)
    --           || sesErrorCode x == "Throttling"

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) return

is :: AWS.Abbrev -> Int -> AWS.Error -> Bool
is srv s (AWS.ServiceError e) = srv == e^.serviceAbbrev && s == statusCode (e^.serviceStatus)
is _   _ _                    = False

isTimeout :: MonadIO m => Either AWS.Error a -> m Bool
isTimeout (Right _) = pure False
isTimeout (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
    _                                                           -> pure False

execCatch :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
          => r -> a -> m (Either AWS.Error (Rs a))
execCatch e cmd = runResourceT . AWST.runAWST e
                $ AWST.trying AWS._Error $ AWST.send cmd

exec :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
     => r -> a -> m (Rs a)
exec e cmd = execCatch e cmd >>= either (throwM . GeneralError) return

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout)                   -> pure True
    AWS.ServiceError se | se^.AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
    _                                                                             -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
