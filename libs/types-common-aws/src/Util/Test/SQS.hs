{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Util.Test.SQS where

import Amazonka qualified as AWS
import Amazonka.SQS qualified as SQS
import Amazonka.SQS.Lens qualified as SQS
import Control.Lens hiding ((.=))
import Data.ByteString.Base64 qualified as B64
import Data.List (delete)
import Data.ProtoLens
import Data.Text.Encoding qualified as Text
import Imports
import UnliftIO (Async, async)
import UnliftIO.Async qualified as Async
import UnliftIO.Exception
import UnliftIO.Resource (MonadResource, ResourceT)
import UnliftIO.Timeout (timeout)

data SQSWatcher a = SQSWatcher
  { watcherProcess :: Async (),
    events :: IORef [a]
  }

-- | Starts an async loop which continuously retrieves messages from a given SQS
-- queue, parses them and stores them in an `IORef [a]`.
--
-- This function drops everything in the queue before starting the async loop.
-- This helps test run faster and makes sure that initial tests don't timeout if
-- the queue has too many things in it before the tests start.
-- Note that the purgeQueue command is not guaranteed to be instant (can take up to 60 seconds)
-- Hopefully, the fake-aws implementation used during tests is fast enough.
watchSQSQueue :: (Message a) => AWS.Env -> Text -> IO (SQSWatcher a)
watchSQSQueue env queueName = do
  eventsRef <- newIORef []

  queueUrlRes <- execute env . sendEnv $ SQS.newGetQueueUrl queueName
  let queueUrl = view SQS.getQueueUrlResponse_queueUrl queueUrlRes

  ensureEmpty queueUrl
  process <- async $ do
    -- Every receive request takes ~300ms (on my machine). This puts a limit of
    -- ~3 notifications per second. Which makes tests reallly slow. SQS scales
    -- pretty well with multiple consumers, so we start 5 consumers here to bump
    -- the max throughput to about ~15 notifications per second.
    loop1 <- async $ receiveLoop queueUrl eventsRef
    loop2 <- async $ receiveLoop queueUrl eventsRef
    loop3 <- async $ receiveLoop queueUrl eventsRef
    loop4 <- async $ receiveLoop queueUrl eventsRef
    loop5 <- async $ receiveLoop queueUrl eventsRef
    _ <- Async.waitAny [loop1, loop2, loop3, loop4, loop5]
    throwIO $ BackgroundThreadNotRunning $ "One of the SQS receive loops finished, all of them are supposed to run forever"

  pure $ SQSWatcher process eventsRef
  where
    receiveLoop queueUrl ref = do
      let rcvReq =
            SQS.newReceiveMessage queueUrl
              & set SQS.receiveMessage_waitTimeSeconds (Just 10)
                . set SQS.receiveMessage_maxNumberOfMessages (Just 1)
                . set SQS.receiveMessage_visibilityTimeout (Just 1)
      rcvRes <- execute env $ sendEnv rcvReq
      let msgs = fromMaybe [] $ view SQS.receiveMessageResponse_messages rcvRes
      parsedMsgs <- fmap catMaybes . execute env $ mapM (parseDeleteMessage queueUrl) msgs
      case parsedMsgs of
        [] -> pure ()
        _ -> atomicModifyIORef ref $ \xs ->
          (parsedMsgs <> xs, ())
      receiveLoop queueUrl ref

    ensureEmpty :: Text -> IO ()
    ensureEmpty queueUrl = void $ execute env $ sendEnv (SQS.newPurgeQueue queueUrl)

data SQSWatcherError = BackgroundThreadNotRunning String
  deriving (Show)

instance Exception SQSWatcherError

-- | Waits for a message matching a predicate for a given number of seconds.
waitForMessage :: forall m a. (MonadUnliftIO m, Eq a, HasCallStack) => SQSWatcher a -> Int -> (a -> Bool) -> m (Maybe a)
waitForMessage watcher seconds predicate = timeout (seconds * 1_000_000) poll
  where
    poll :: (HasCallStack) => m a
    poll = do
      -- Check if the background thread is still alive. If not fail with a nicer error
      Async.poll watcher.watcherProcess >>= \case
        Nothing -> pure ()
        Just (Left err) -> throwIO $ BackgroundThreadNotRunning $ "Thread finished with exception: " <> show err
        Just (Right ()) -> throwIO $ BackgroundThreadNotRunning "Thread finished without any exceptions when it was supposed to run forever"
      matched <- atomicModifyIORef (events watcher) $ \events ->
        case filter predicate events of
          [] -> (events, Nothing)
          (x : _) -> (delete x events, Just x)
      maybe (threadDelay 10000 >> poll) pure matched

-- | First waits for a message matching a given predicate for 10 seconds (this
-- number could be chosen more scientifically) and then uses a callback to make
-- an assertion on such a message.
assertMessage :: (MonadUnliftIO m, Eq a, HasCallStack) => SQSWatcher a -> String -> (a -> Bool) -> (String -> Maybe a -> m ()) -> m ()
assertMessage watcher label predicate callback = do
  matched <- waitForMessage watcher 5 predicate
  callback label matched

-----------------------------------------------------------------------------
-- Generic AWS execution helpers
execute ::
  AWS.Env ->
  ReaderT AWS.Env (ResourceT IO) a ->
  IO a
execute env = AWS.runResourceT . flip runReaderT env

-----------------------------------------------------------------------------
-- Internal. Most of these functions _can_ be used outside of this function
-- but probably do not need to

deleteMessage :: (MonadReader AWS.Env m, MonadResource m) => Text -> SQS.Message -> m ()
deleteMessage url m = do
  for_
    (m ^. SQS.message_receiptHandle)
    (void . sendEnv . SQS.newDeleteMessage url)

parseDeleteMessage :: (Message a, MonadReader AWS.Env m, MonadResource m, MonadUnliftIO m) => Text -> SQS.Message -> m (Maybe a)
parseDeleteMessage url m = do
  let decodedMessage = decodeMessage <=< (B64.decode . Text.encodeUtf8)
  evt <- case decodedMessage <$> (m ^. SQS.message_body) of
    Just (Right e) -> pure (Just e)
    _ -> do
      liftIO $ putStrLn "Failed to parse SQS message or event"
      pure Nothing
  deleteMessage url m
    `catch` \case
      (fromException @SomeAsyncException -> Just asyncExc) ->
        throwIO asyncExc
      e ->
        liftIO $ putStrLn $ "Failed to delete message, this error will be ignored. Message: " <> show m <> ", Exception: " <> displayException e
  pure evt

sendEnv ::
  ( MonadReader AWS.Env m,
    MonadResource m,
    Typeable a,
    Typeable (AWS.AWSResponse a),
    AWS.AWSRequest a
  ) =>
  a ->
  m (AWS.AWSResponse a)
sendEnv x = flip AWS.send x =<< ask
