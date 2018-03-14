{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module API.SQS where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, asyncExceptionFromException)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Id
import Data.Int
import Data.Monoid ((<>))
import Data.Text (pack)
import Safe (headDef)
import Data.ByteString.Lazy (toStrict)
import Data.ProtoLens.Encoding
import Data.Text (Text)
import Galley.Aws
import Galley.Options (JournalOpts(..))
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session as Ssl
import System.Logger.Class
import Test.Tasty.HUnit

import qualified Data.ByteString.Base64 as B64
import qualified Data.Currency as Currency
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Galley.Aws as Aws
import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as SQS
import qualified OpenSSL.X509.SystemStore as Ssl
import qualified Proto.TeamEvents as E
import qualified System.Logger as L

ensureQueueEmpty :: MonadIO m => Maybe Aws.Env -> m ()
ensureQueueEmpty (Just env) = liftIO $ Aws.execute env purgeQueue
ensureQueueEmpty Nothing    = return ()

assertQueue :: MonadIO m => String -> Maybe Aws.Env -> (String -> Maybe E.TeamEvent -> IO ()) -> m ()
assertQueue label (Just env) check = liftIO $ Aws.execute env $ fetchMessage label check
assertQueue _     Nothing    _     = return ()

assertQueue' :: MonadIO m => String -> Int -> Maybe Aws.Env -> (String -> Maybe E.TeamEvent -> IO ()) -> m ()
assertQueue' label timeout (Just env) check = liftIO $ Aws.execute env $ awaitMessage label timeout check
assertQueue' _     _       Nothing    _     = return ()

assertQueueEmpty :: MonadIO m => Maybe Aws.Env -> m ()
assertQueueEmpty (Just env) = liftIO $ Aws.execute env ensureNoMessages
assertQueueEmpty Nothing    = return ()

tActivateWithCurrency :: Maybe Currency.Alpha -> String -> Maybe E.TeamEvent -> IO ()
tActivateWithCurrency c l (Just e) = do
    assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e^.E.eventType)
    assertEqual "count" 1 (e^.E.eventData^.E.memberCount)
    -- NOTE: protobuf decodes absent, optional fields as (Just "")
    let cur = maybe "" (pack . show) c
    assertEqual "currency" (Just cur) (e^.E.eventData^?E.currency)
tActivateWithCurrency _ l Nothing  = assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

tActivate :: String -> Maybe E.TeamEvent -> IO ()
tActivate l (Just e) = do
    assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e^.E.eventType)
    assertEqual "count" 1 (e^.E.eventData^.E.memberCount)
tActivate l Nothing  = assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

tDelete :: String -> Maybe E.TeamEvent -> IO ()
tDelete l (Just e) = assertEqual (l <> ": eventType") E.TeamEvent'TEAM_DELETE (e^.E.eventType)
tDelete l Nothing  = assertFailure $ l <> ": Expected 1 TeamDelete, got nothing"

tSuspend :: String -> Maybe E.TeamEvent -> IO ()
tSuspend l (Just e) = assertEqual (l  <> "eventType") E.TeamEvent'TEAM_SUSPEND (e^.E.eventType)
tSuspend l Nothing  = assertFailure $ l <> ": Expected 1 TeamSuspend, got nothing"

tUpdate :: Int32 -> [UserId] -> String -> Maybe E.TeamEvent -> IO ()
tUpdate c uids l (Just e) = do
    assertEqual (l <> "eventType") E.TeamEvent'TEAM_UPDATE (e^.E.eventType)
    assertEqual "count" c (e^.E.eventData^.E.memberCount)
    assertEqual "billing users" (toStrict . UUID.toByteString . toUUID <$> uids) (e^.E.eventData^.E.billingUser)
tUpdate _ _ l Nothing = assertFailure $ l <> ": Expected 1 TeamUpdate, got nothing"

ensureNoMessages :: Amazon ()
ensureNoMessages = do
    QueueUrl url <- view eventQueue
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive 1 url)
    liftIO $ assertEqual "ensureNoMessages: length" 0 (length msgs)

fetchMessage :: String -> (String -> Maybe E.TeamEvent -> IO()) -> Amazon ()
fetchMessage label callback = do
    QueueUrl url <- view eventQueue
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive 1 url)
    events <- mapM (parseDeleteMessage url) msgs
    liftIO $ callback label (headDef Nothing events)

awaitMessage :: String -> Int -> (String -> Maybe E.TeamEvent -> IO()) -> Amazon ()
awaitMessage label timeout callback = do
    QueueUrl url <- view eventQueue
    -- TODO: Read all messages
    tryMatch label timeout url callback -- (headDef Nothing events)

type Err = String
type Success = String

-- tryMatch :: (MonadIO m, MonadCatch m, AWS.MonadAWS m)
--          => Text
--          -> (String -> Maybe E.TeamEvent -> IO())
--          -> m (Either Err Success)    
tryMatch :: String
         -> Int
         -> Text
         -> (String -> Maybe E.TeamEvent -> IO())
         -> Amazon ()    
tryMatch label tries url callback = go tries
  where
    go 0 = error "no journaled notification found fail!"
    go n = do
        msgs   <- view SQS.rmrsMessages <$> AWS.send (receive 10 url)
        events <- mapM parseMessage msgs
        liftIO $ print ("Callback with: " ++ show events)
        anyOK <- checkAnyOK . zip msgs <$> mapM check events
        liftIO $ print anyOK
        case anyOK of
            [] -> go (n - 1)
            xs -> do
                forM_ xs $ \(m,evt) -> do
                    for_ (m ^. SQS.mReceiptHandle) (void . AWS.send . SQS.deleteMessage url)
                    liftIO $ print ("deleted: " ++ show evt)
                -- go (n - 1)
        liftIO $ threadDelay (1 * 10^(6 :: Int))

    check :: Maybe E.TeamEvent -> Amazon (Either Err Success)
    check e = do
        liftIO $ callback label e
        liftIO $ print ("Successfully checked: " ++ show e)
        return (Right $ show e)
      `catchAll` \ex -> case asyncExceptionFromException ex of
        Just  x -> throwM (x :: SomeAsyncException)
        Nothing -> return (Left $ show ex)

    checkAnyOK :: [(SQS.Message, Either Err Success)] -> [(SQS.Message, Success)]
    checkAnyOK []     = []
    checkAnyOK (x:xs) = case x of
                            (m, Right r) -> (m, r) : checkAnyOK xs
                            _            -> checkAnyOK xs

purgeQueue :: Amazon ()
purgeQueue = do
    QueueUrl url <- view eventQueue
    void $ AWS.send (SQS.purgeQueue url)

receive :: Int -> Text -> SQS.ReceiveMessage
receive n url = SQS.receiveMessage url
              & set SQS.rmWaitTimeSeconds (Just 1)
              . set SQS.rmMaxNumberOfMessages (Just n)
              . set SQS.rmVisibilityTimeout (Just 1)

parseDeleteMessage :: Text -> SQS.Message -> Amazon (Maybe E.TeamEvent)
parseDeleteMessage url m = do
    evt <- parseMessage m
    for_ (m ^. SQS.mReceiptHandle) (void . AWS.send . SQS.deleteMessage url)
    return evt
    
parseMessage :: SQS.Message -> Amazon (Maybe E.TeamEvent)
parseMessage m =
    case (>>= decodeMessage) . B64.decode . Text.encodeUtf8 <$> (m^.SQS.mBody) of
        Just (Right e) -> do
            trace $ msg $ val "SQS event received"
            return (Just e)
        _ -> do
            err . msg $ val "Failed to parse SQS event"
            return Nothing

initHttpManager :: IO Manager
initHttpManager = do
    ctx <- Ssl.context
    Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
    Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
    Ssl.contextSetCiphers ctx rsaCiphers
    Ssl.contextLoadSystemCerts ctx
    newManager (opensslManagerSettings ctx)
        { managerResponseTimeout     = responseTimeoutMicro 10000000
        , managerConnCount           = 100
        , managerIdleConnectionCount = 300
        }

mkAWSEnv :: JournalOpts -> IO Aws.Env
mkAWSEnv opts = do
    l   <- L.new $ L.setOutput L.StdOut . L.setFormat Nothing $ L.defSettings
    mgr <- initHttpManager
    Aws.mkEnv l mgr opts
