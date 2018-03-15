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
import Data.UUID.V4 (nextRandom)
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

-- Try to assert an event in the queue for a `timeout` amount of seconds
tryAssertQueue :: MonadIO m => Int -> String -> Maybe Aws.Env -> (String -> Maybe E.TeamEvent -> IO ()) -> m ()
tryAssertQueue timeout label (Just env) check = liftIO $ Aws.execute env $ awaitMessage label timeout check
tryAssertQueue _       _     Nothing    _     = return ()

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
    tryMatch label timeout url callback

newtype MatchFailure = MatchFailure (Maybe E.TeamEvent, SomeException)
type MatchSuccess = String

tryMatch :: String
         -> Int
         -> Text
         -> (String -> Maybe E.TeamEvent -> IO())
         -> Amazon ()    
tryMatch label tries url callback = go tries
  where
    go 0 = liftIO (assertFailure $ label <> ": No matching team event found")
    go n = do
        msgs      <- readAllUntilEmpty
        (bad, ok) <- splitResults <$> mapM (check <=< parseDeleteMessage url) msgs
        -- Requeue all failed checks
        forM_ bad $ \(MatchFailure (evt, exp)) -> for_ evt queueEvent
        -- If no success, continue!
        when (null $ ok) $ do
            liftIO $ threadDelay (10^(6 :: Int))
            go (n - 1)

    check :: Maybe E.TeamEvent -> Amazon (Either MatchFailure String)
    check e = do
        liftIO $ callback label e
        return (Right $ show e)
      `catchAll` \ex -> case asyncExceptionFromException ex of
        Just  x -> throwM (x :: SomeAsyncException)
        Nothing -> return . Left $ MatchFailure (e, ex)

    splitResults :: [Either MatchFailure String] -> ([MatchFailure], [String])
    splitResults xs = go' xs ([], [])
      where
        go' []     acc      = acc
        go' (x:xs) (ls, rs) = case x of
                            Left l  -> go' xs (l:ls, rs  )
                            Right r -> go' xs (ls  , r:rs)

-- Note that Amazon's purge queue is a bit incovenient for testing purposes because
-- it may be delayed in ~60 seconds which causes messages that are published later
-- to be (unintentionally) deleted
purgeQueue :: Amazon ()
purgeQueue = void $ readAllUntilEmpty

receive :: Int -> Text -> SQS.ReceiveMessage
receive n url = SQS.receiveMessage url
              & set SQS.rmWaitTimeSeconds (Just 1)
              . set SQS.rmMaxNumberOfMessages (Just n)
              . set SQS.rmVisibilityTimeout (Just 1)

queueEvent :: E.TeamEvent -> Amazon ()
queueEvent e = do
    QueueUrl url <- view eventQueue
    rnd <- liftIO nextRandom
    void $ AWS.send (req url rnd)
  where
    event = Text.decodeLatin1 $ B64.encode $ encodeMessage e
    req url dedup = SQS.sendMessage url event
                  & SQS.smMessageGroupId .~ Just "team.events"
                  & SQS.smMessageDeduplicationId .~ Just (UUID.toText dedup)

readAllUntilEmpty :: Amazon [SQS.Message]
readAllUntilEmpty = do
    QueueUrl url <- view eventQueue
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive 10 url)
    readUntilEmpty msgs url msgs
  where
    readUntilEmpty acc _    []   = return acc
    readUntilEmpty acc url  msgs = do
        forM_ msgs $ deleteMessage url
        newMsgs <- view SQS.rmrsMessages <$> AWS.send (receive 10 url)
        readUntilEmpty (acc ++ newMsgs) url newMsgs

deleteMessage :: Text -> SQS.Message -> Amazon ()
deleteMessage url m = do
    for_ (m ^. SQS.mReceiptHandle)
         (void . AWS.send . SQS.deleteMessage url)

parseDeleteMessage :: Text -> SQS.Message -> Amazon (Maybe E.TeamEvent)
parseDeleteMessage url m = do
    evt <- case (>>= decodeMessage) . B64.decode . Text.encodeUtf8 <$> (m^.SQS.mBody) of
        Just (Right e) -> do
            trace $ msg $ val "SQS event received"
            return (Just e)
        _ -> do
            err . msg $ val "Failed to parse SQS message or event"
            return Nothing
    deleteMessage url m
    return evt

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
    print opts
    l   <- L.new $ L.setOutput L.StdOut . L.setFormat Nothing $ L.defSettings
    mgr <- initHttpManager
    Aws.mkEnv l mgr opts
