{-# OPTIONS_GHC -Wwarn -Wno-incomplete-uni-patterns #-}

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

module Gundeck.Notification.Data
  ( ResultPage (..),
    add,
    fetch,
    fetchId,
    fetchLast,
    deleteAll,
  )
where

import Cassandra as C
import Control.Exception qualified as CE
import Control.Lens (view, (^.))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as JSON
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.List1 (List1)
import Data.Map qualified as Map
import Data.Range (Range, fromRange)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Gundeck.Env
import Gundeck.Options (NotificationTTL (..), notificationTTL, settings)
import Gundeck.Push.Native.Serialise ()
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Types qualified as Q
import UnliftIO (pooledForConcurrentlyN_)
import UnliftIO.Timeout (timeout)
import Wire.API.Internal.Notification

data ResultPage = ResultPage
  { -- | A sequence of notifications.
    resultSeq :: Seq QueuedNotification,
    -- | Whether there might be more notifications that can be
    -- obtained through another query, starting the the ID of the
    -- last notification in 'resultSeq'.
    resultHasMore :: !Bool,
    -- | Whether there might be a gap in the 'resultSeq'. This is 'True'
    -- iff a start ID ('since') has been given which could not be found.
    resultGap :: !Bool
  }

add ::
  forall m.
  (MonadReader Env m, MonadUnliftIO m) =>
  List1 NotificationTarget ->
  List1 JSON.Object ->
  m ()
add tgts event = do
  --  TODO: maybe tryRead and fail?
  chan <- readMVar =<< view rabbitmqChannel
  pooledForConcurrentlyN_ 32 tgts $ \tgt -> do
    let uid = tgt ^. targetUser
    ensureNotifStream uid
    let msg =
          Q.newMsg
            { Q.msgBody =
                Aeson.encode $
                  Aeson.object
                    [ "target_clients" .= (tgt ^. targetClients),
                      "event" .= event
                    ]
            }
    liftIO $ Q.publishMsg chan "" (userStreamName uid) msg

ensureNotifStream :: (MonadReader Env m, MonadIO m) => UserId -> m ()
ensureNotifStream uid = do
  chan <- readMVar =<< view rabbitmqChannel
  NotificationTTL ttlSeconds <- view $ options . settings . notificationTTL
  let qOpts =
        Q.newQueue
          { Q.queueName = userStreamName uid,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  [ ("x-queue-type", (Q.FVString "stream")),
                    ("x-max-age", (Q.FVString $ Text.encodeUtf8 $ Text.pack $ show ttlSeconds <> "s"))
                  ]
          }
  void $ liftIO $ Q.declareQueue chan qOpts

userStreamName :: UserId -> Text
userStreamName uid = "client-notifications." <> Text.pack (show uid)

fetchId :: (MonadReader Env m, MonadUnliftIO m) => UserId -> NotificationId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchId u n c = do
  mMsg <- fetchNotifById u n
  pure $ mkNotif c =<< mMsg

fetchNotifById :: (MonadReader Env m, MonadUnliftIO m) => UserId -> NotificationId -> m (Maybe Q.Message)
fetchNotifById u n = do
  chan <- readMVar =<< view rabbitmqChannel
  notifsMVar <- newEmptyMVar
  liftIO $ Q.qos chan 0 1 False
  let processMsg (msg, _envelope) = handleErrors $ do
        void $ tryPutMVar notifsMVar msg
  consumerTag <-
    liftIO $
      Q.consumeMsgs'
        chan
        (userStreamName u)
        Q.Ack
        processMsg
        (const $ pure ())
        (Q.FieldTable $ Map.singleton "x-stream-offset" (Q.FVInt64 (read $ Text.unpack n)))
  -- This is a weird hack because we cannot know when we're done fetching notifs.
  mMsg <- timeout 1_000_000 (takeMVar notifsMVar)
  liftIO $ Q.cancelConsumer chan consumerTag
  pure mMsg

fetchLast :: forall m. (MonadUnliftIO m, MonadReader Env m) => UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast u c = do
  chan <- readMVar =<< view rabbitmqChannel
  notifsTVar <- newTVarIO Nothing
  liftIO $ Q.qos chan 0 1 False
  let processMsg (msg, _envelope) = handleErrors $ do
        atomically $ modifyTVar' notifsTVar $ const $ Just msg
  consumerTag <-
    liftIO $
      Q.consumeMsgs'
        chan
        (userStreamName u)
        Q.Ack
        processMsg
        (const $ pure ())
        (Q.FieldTable $ Map.singleton "x-stream-offset" (Q.FVString "last"))
  -- This is a weird hack because we cannot know when we're done fetching notifs.
  threadDelay 1_000_000
  liftIO $ Q.cancelConsumer chan consumerTag
  let go :: Maybe Q.Message -> m (Maybe QueuedNotification)
      go mMsg =
        case mMsg of
          Nothing -> pure Nothing
          Just m -> case mkNotif c m of
            Nothing -> do
              let Just (Q.FieldTable table) = m.msgHeaders
              let Just (Q.FVInt64 notifId) = Map.lookup "x-stream-offset" table
              let nId = Text.pack (show (notifId - 1))
              fetchNotifById u nId >>= go
            Just n -> pure (Just n)
  readTVarIO notifsTVar >>= go

fetch :: forall m. (MonadReader Env m, MonadClient m, MonadUnliftIO m) => UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch u c mSince (fromIntegral . fromRange -> pageSize) = do
  chan <- readMVar =<< view rabbitmqChannel
  notifsTVar <- newTVarIO mempty
  notifsFullMVar <- newEmptyMVar
  liftIO $ Q.qos chan 0 1 False
  let processMsg (msg, envelope) = handleErrors $ do
        isFull <- atomically $ stateTVar notifsTVar $ \allMsgs ->
          let allMsgsNew = allMsgs :|> msg
           in (length allMsgsNew >= pageSize, allMsgsNew)
        when isFull $ void $ tryPutMVar notifsFullMVar ()
        Q.ackMsg chan envelope.envDeliveryTag False
  let offset = maybe (Q.FVString "first") (Q.FVInt64 . read . Text.unpack) mSince
  consumerTag <-
    liftIO $
      Q.consumeMsgs'
        chan
        (userStreamName u)
        Q.Ack
        processMsg
        (const $ pure ())
        (Q.FieldTable $ Map.singleton "x-stream-offset" offset)
  -- This is a weird hack because we cannot know when we're done fetching notifs.
  mFull <- timeout (1_000_000) (takeMVar notifsFullMVar)
  liftIO $ Q.cancelConsumer chan consumerTag
  notifs <- foldMap (foldMap Seq.singleton . mkNotif c) <$> readTVarIO notifsTVar
  pure $
    ResultPage
      { resultSeq = notifs,
        resultHasMore = isJust mFull,
        resultGap = False
      }

handleErrors :: IO () -> IO ()
handleErrors action =
  action
    `CE.catches` [
                   -- rethrow this exception, since the AMPQ library uses it internally
                   CE.Handler $ \(e :: Q.ChanThreadKilledException) -> CE.throwIO e,
                   -- (optional) catch individual exceptions that your code may throw
                   -- CE.Handler $ \(e::CE.IOException) -> ...,
                   -- CE.Handler $ \(e::SomeOtherException) -> ...,

                   -- catch all exceptions that weren't handled above
                   CE.Handler $ \(_ :: CE.SomeException) -> pure ()
                 ]

-- returns empty if message cannot be converted to notif
-- TODO: log when a mesasge doesn't get translated to queued notification
mkNotif :: Maybe ClientId -> Q.Message -> Maybe QueuedNotification
mkNotif c msg = do
  Q.FieldTable headers <- msg.msgHeaders
  offsetVal <- Map.lookup "x-stream-offset" headers
  offset <- case offsetVal of
    Q.FVInt64 o -> Just o
    _ -> Nothing
  sm <- Aeson.decode @StoredMessage (Q.msgBody msg)
  if sm.smTargetClients == mempty || maybe True (flip Set.member sm.smTargetClients) c
    then Just $ queuedNotification (Text.pack $ show offset) sm.smEvent
    else Nothing

data StoredMessage = StoredMessage
  { smTargetClients :: Imports.Set ClientId,
    smEvent :: NonEmpty Aeson.Object
  }

instance JSON.FromJSON StoredMessage where
  parseJSON = JSON.withObject "StoredMessage" $ \o ->
    StoredMessage
      <$> o JSON..: "target_clients"
      <*> o JSON..: "event"

deleteAll :: (MonadClient m, MonadReader Env m) => UserId -> m ()
deleteAll u = do
  chan <- readMVar =<< view rabbitmqChannel
  void . liftIO . Q.deleteQueue chan $ userStreamName u
