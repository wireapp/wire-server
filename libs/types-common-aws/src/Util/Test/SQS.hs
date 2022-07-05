{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified Amazonka as AWS
import qualified Amazonka.SQS as SQS
import qualified Amazonka.SQS.Lens as SQS
import Control.Exception (asyncExceptionFromException)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (bracket)
import qualified Data.ByteString.Base64 as B64
import Data.ProtoLens
import qualified Data.Text.Encoding as Text
import Imports
import Safe (headDef)
import Test.Tasty.HUnit
import UnliftIO.Resource (MonadResource, ResourceT)

-----------------------------------------------------------------------------
-- Assertions
assertQueue :: Message a => Text -> String -> AWS.Env -> (String -> Maybe a -> IO ()) -> IO ()
assertQueue url label env check = execute env $ fetchMessage url label check

assertNoMessages :: Text -> AWS.Env -> IO ()
assertNoMessages url env = do
  msgs <- execute env $ readAndDeleteAllUntilEmpty url
  assertEqual "ensureNoMessages: length" 0 (length msgs)

-----------------------------------------------------------------------------
-- Queue operations
purgeQueue :: (Monad m, MonadReader AWS.Env m, MonadResource m) => Text -> m ()
purgeQueue = void . readAndDeleteAllUntilEmpty

-- Note that Amazon's purge queue is a bit incovenient for testing purposes because
-- it may be delayed in ~60 seconds which causes messages that are published later
-- to be (unintentionally) deleted which is why we have our own for testing purposes
readAndDeleteAllUntilEmpty :: (Monad m, MonadReader AWS.Env m, MonadResource m) => Text -> m [SQS.Message]
readAndDeleteAllUntilEmpty url = do
  firstBatch <- fromMaybe [] . view SQS.receiveMessageResponse_messages <$> sendEnv (receive 1 url)
  readUntilEmpty firstBatch firstBatch
  where
    readUntilEmpty acc [] = pure acc
    readUntilEmpty acc msgs = do
      forM_ msgs $ deleteMessage url
      newMsgs <- fromMaybe [] . view SQS.receiveMessageResponse_messages <$> sendEnv (receive 1 url)
      forM_ newMsgs $ deleteMessage url
      readUntilEmpty (acc ++ newMsgs) newMsgs

deleteMessage :: (Monad m, MonadReader AWS.Env m, MonadResource m) => Text -> SQS.Message -> m ()
deleteMessage url m = do
  for_
    (m ^. SQS.message_receiptHandle)
    (void . sendEnv . SQS.newDeleteMessage url)

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
receive :: Int -> Text -> SQS.ReceiveMessage
receive n url =
  SQS.newReceiveMessage url
    & set SQS.receiveMessage_waitTimeSeconds (Just 1)
      . set SQS.receiveMessage_maxNumberOfMessages (Just n)
      . set SQS.receiveMessage_visibilityTimeout (Just 1)

fetchMessage :: (MonadIO m, Message a, MonadReader AWS.Env m, MonadResource m) => Text -> String -> (String -> Maybe a -> IO ()) -> m ()
fetchMessage url label callback = do
  msgs <- fromMaybe [] . view SQS.receiveMessageResponse_messages <$> sendEnv (receive 1 url)
  events <- mapM (parseDeleteMessage url) msgs
  liftIO $ callback label (headDef Nothing events)

parseDeleteMessage :: (Monad m, Message a, MonadIO m, MonadReader AWS.Env m, MonadResource m) => Text -> SQS.Message -> m (Maybe a)
parseDeleteMessage url m = do
  let decodedMessage = decodeMessage <=< (B64.decode . Text.encodeUtf8)
  evt <- case decodedMessage <$> (m ^. SQS.message_body) of
    Just (Right e) -> pure (Just e)
    _ -> do
      liftIO $ print ("Failed to parse SQS message or event" :: String)
      pure Nothing
  deleteMessage url m
  pure evt

queueMessage :: (MonadReader AWS.Env m, Message a, MonadResource m) => Text -> a -> m ()
queueMessage url e = do
  void $ sendEnv req
  where
    event = Text.decodeLatin1 $ B64.encode $ encodeMessage e
    req = SQS.newSendMessage url event

newtype MatchFailure a = MatchFailure {mFailure :: (a, SomeException)}

-- Try to match some assertions (callback) during the given timeout; if there's no
-- match during the timeout, it asserts with the given label
-- Matched matches are consumed while unmatched ones are republished to the queue
tryMatch ::
  (Show a, Message a, MonadReader AWS.Env m, MonadResource m, MonadThrow m, MonadCatch m) =>
  String ->
  Int ->
  Text ->
  (String -> Maybe a -> IO ()) ->
  m ()
tryMatch label tries url callback = go tries
  where
    go 0 = liftIO (assertFailure $ label <> ": No matching event found")
    go n = do
      msgs <- readAndDeleteAllUntilEmpty url
      (bad, ok) <- partitionEithers <$> mapM (check <=< parseDeleteMessage url) msgs
      -- Requeue all failed checks
      forM_ bad $ \x -> for_ (fst . mFailure $ x) (queueMessage url)
      -- If no success, continue!
      when (null ok) $ do
        liftIO $ threadDelay (10 ^ (6 :: Int))
        go (n - 1)
    check e =
      do
        liftIO $ callback label e
        pure (Right $ show e)
        `catchAll` \ex -> case asyncExceptionFromException ex of
          Just x -> throwM (x :: SomeAsyncException)
          Nothing -> pure . Left $ MatchFailure (e, ex)

sendEnv :: (MonadReader AWS.Env m, MonadResource m, AWS.AWSRequest a) => a -> m (AWS.AWSResponse a)
sendEnv x = flip AWS.send x =<< ask
