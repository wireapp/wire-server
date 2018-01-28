{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Brig.Aws
    ( -- * DynamoDB
      DynamoError (..)
    , sendDynamo
    , tryDynamo

      -- * SQS
    , listen
    , sendSqs

      -- * SES
    , sendMail

      -- * Re-exports
    , module Types
    ) where

import Aws.DynamoDb
import Aws.Ses
import Aws.Sqs
import Bilge.Retry (httpHandlers)
import Brig.App
import Brig.Aws.Types as Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeAsyncException)
import Control.Lens (view, (^.))
import Control.Monad (forever, void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Retry
import Data.Aeson
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Status
import Network.Mail.Mime (Mail)
import System.Logger.Class (field, msg, val, (~~))

import qualified Aws.Core            as Aws
import qualified Aws.DynamoDb        as Ddb
import qualified Aws.Sqs             as Sqs
import qualified Ropes.Aws           as Aws
import qualified Ropes.Aws.Ses       as Ses
import qualified System.Logger.Class as Log

-------------------------------------------------------------------------------
-- DynamoDB

data DynamoError
    = DynamoErrorResponse !DdbError
    | DynamoException !SomeException
    deriving (Show)

sendDynamo :: (Aws.Transaction r a, Aws.ServiceConfiguration r ~ DdbConfiguration)
           => r
           -> AppIO (Aws.ResponseMetadata a, a)
sendDynamo r = do
    env <- view awsEnv
    cfg <- view awsConfig
    runAppResourceT
        $ recovering retry3x handlers
        $ const (Aws.sendRequest env (cfg^.ddbConfig) r)
  where
    handlers = httpHandlers ++ [ const . Handler $ return . canRetry ]

    canRetry = Ddb.shouldRetry . ddbErrCode

    retry3x  = limitRetries 3 <> exponentialBackoff 100000

tryDynamo :: (Aws.Transaction r a, Aws.ServiceConfiguration r ~ DdbConfiguration)
          => r
          -> AppIO (Either DynamoError (Aws.ResponseMetadata a, a))
tryDynamo r = catches (Right <$> sendDynamo r) handlers
  where
    handlers = [ Handler (\(ex :: SomeAsyncException) -> throwM ex)
               , Handler (\(ex ::           DdbError) -> return (Left (DynamoErrorResponse ex)))
               , Handler (\(ex ::      SomeException) -> return (Left (DynamoException ex)))
               ]

-------------------------------------------------------------------------------
-- SQS

listen :: (FromJSON a) => QueueName -> (a -> AppIO ()) -> AppIO ()
listen q f = do
    e <- ask
    forever $ handle anyException $ do
        msgs <- receive
        void . liftIO $ mapConcurrently (runAppT e . consume) msgs
  where
    receive =
        let req = Sqs.ReceiveMessage
                { Sqs.rmQueueName             = q
                , Sqs.rmMaxNumberOfMessages   = Just 10
                , Sqs.rmWaitTimeSeconds       = Just 20
                , Sqs.rmAttributes            = []
                , Sqs.rmUserMessageAttributes = []
                , Sqs.rmVisibilityTimeout     = Nothing
                }
        in Sqs.rmrMessages . snd <$> sendSqs req

    consume m = do
        case eitherDecodeStrict (encodeUtf8 (mBody m)) of
            Left  e -> Log.err $ field "error" e ~~ msg (val "Failed to parse SQS event")
            Right n -> f n
        delete (mReceiptHandle m)

    delete h = void (sendSqs (Sqs.DeleteMessage h q))

    anyException x = case fromException x of
        Just (HttpExceptionRequest _ ResponseTimeout) -> liftIO $ threadDelay 3000000
        _                                             -> Log.err $ msg (show x)

sendSqs :: (Aws.Transaction r a, Aws.ServiceConfiguration r ~ SqsConfiguration)
        => r
        -> AppIO (Aws.ResponseMetadata a, a)
sendSqs r = do
    env <- view awsEnv
    cfg <- view awsConfig
    runAppResourceT
        $ recovering retry5x handlers
        $ const (Aws.sendRequest env (error "cfg^.sqsConfig") r)
  where
    handlers = httpHandlers ++ [ const . Handler $ return . canRetry ]

    canRetry x = statusIsServerError (sqsStatusCode x)
              || sqsErrorCode x == "Throttling"

    retry5x = limitRetries 5 <> exponentialBackoff 100000

-------------------------------------------------------------------------------
-- SES

sendMail :: Mail -> AppIO ()
sendMail mail = do
    env <- view awsEnv
    cfg <- view awsConfig
    void . runAppResourceT $ do
        r <- Ses.sendRawEmail mail
        recovering retry5x handlers $ \(rsIterNumber -> n) ->
            Aws.sendRequest env (choose n (cfg^.sesConfig)) r
  where
    choose n xs = xs !! (n `mod` (length xs))

    handlers = httpHandlers ++ [ const $ Handler $ return . canRetry ]

    canRetry x = statusIsServerError (sesStatusCode x)
              || sesErrorCode x == "Throttling"

    retry5x = limitRetries 5 <> exponentialBackoff 100000
