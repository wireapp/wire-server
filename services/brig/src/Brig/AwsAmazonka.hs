{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Brig.AwsAmazonka
    ( -- * Monad
      Env
    , mkEnv
    , Amazon
    , execute

      -- * Feedback
    , listen
    ) where

import Blaze.ByteString.Builder (toLazyByteString)
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
import Control.Retry (retrying, limitRetries)
import Data.Aeson (decodeStrict)
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
import System.Logger.Class

import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.HashMap.Strict     as Map
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LT
import qualified Gundeck.Types.Push      as Push
import qualified Network.AWS             as AWS
import qualified Network.AWS.Env         as AWS
import qualified Network.AWS.Data        as AWS
import qualified Network.AWS.SNS         as SNS
import qualified Network.AWS.SQS         as SQS
import qualified Network.TLS             as TLS
import qualified System.Logger           as Logger

newtype QueueUrl = QueueUrl Text deriving Show

newtype Account  = Account { fromAccount :: Text } deriving (Eq, Show, ToText, FromJSON)

data Env = Env
    { _awsEnv     :: !AWS.Env
    , _logger     :: !Logger
    , _region     :: !Region
    , _account    :: !Account
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
    liftAWS a = view awsEnv >>= \e -> AWS.runAWS e a

mkEnv :: Logger -> AWSOptsAmazonka -> Manager -> IO Env
mkEnv lgr opts mgr = do
    let g = Logger.clone (Just "aws.brig") lgr
    e <- configure <$> mkAwsEnv g
    return (Env e g (amazonkaRegion opts) (Account (amazonkaAccount opts)))
  where
    mkAwsEnv g =  set AWS.envLogger (awsLogger g)
               .  set AWS.envRegion (amazonkaRegion opts)
              <$> AWS.newEnvWith AWS.Discover Nothing mgr

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

    configure = set AWS.envRetryCheck retryCheck
              . AWS.configure snsConfig

    snsConfig = SNS.sns & set AWS.serviceTimeout (Just (AWS.Seconds 5))

    -- Modified version of 'AWS.retryConnectionFailure' to take into
    -- account occasional TLS handshake failures.
    -- See: https://github.com/vincenthz/hs-tls/issues/124
    -- See: https://github.com/brendanhay/amazonka/issues/269
    retryCheck _ InvalidUrlException{} = False
    retryCheck n (HttpExceptionRequest _ ex) = case ex of
        _ | n >= 3                    -> False
        NoResponseDataReceived        -> True
        ConnectionTimeout             -> True
        ConnectionClosed              -> True
        ConnectionFailure _           -> True
        InternalException x           -> case fromException x of
            Just TLS.HandshakeFailed {} -> True
            _                           -> False
        _                             -> False

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
    GeneralError      :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show     Error
deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- Feedback

listen :: (FromJSON a) => Text -> (a -> IO ()) -> Amazon ()
listen qn callback = do
    env <- view awsEnv
    QueueUrl url <- liftIO $ getQueueUrl env qn
    forever $ handleAny unexpectedError $ do
        msgs <- view rmrsMessages <$> send (receive url)
        void $ mapConcurrently (onMessage url) msgs
  where
    getQueueUrl :: AWS.Env -> Text -> IO QueueUrl
    getQueueUrl e q = do
        x <- runResourceT . AWST.runAWST e $
            AWST.trying AWS._Error $
                AWST.send (SQS.getQueueURL q)
        either (throwM . GeneralError)
               (return . QueueUrl . view SQS.gqursQueueURL) x

    receive url =
        SQS.receiveMessage url
            & set SQS.rmWaitTimeSeconds (Just 20)
            . set SQS.rmMaxNumberOfMessages (Just 10)

    onMessage url m =
        case decodeStrict =<< Text.encodeUtf8 <$> m^.mBody of
            Nothing ->
                err . msg $ val "Failed to parse SQS event notification"
            Just e -> do
                debug . msg $ val "Received SQS event: " -- TODO: More logging?
                liftIO $ callback e
                for_ (m^.mReceiptHandle) (void . send . SQS.deleteMessage url)

    unexpectedError x = do
        err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
        threadDelay 3000000

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = either (throwM . GeneralError) return =<< sendCatch r

is :: AWS.Abbrev -> Int -> AWS.Error -> Bool
is srv s (AWS.ServiceError e) = srv == e^.serviceAbbrev && s == statusCode (e^.serviceStatus)
is _   _ _                    = False

isTimeout :: MonadIO m => Either AWS.Error a -> m Bool
isTimeout (Right _) = pure False
isTimeout (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
    _                                                           -> pure False

