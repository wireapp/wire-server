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
import Control.Error (MaybeT (..))
import Control.Lens (view, (^.), _1)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as JSON
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.List1 (List1)
import Data.Map qualified as Map
import Data.Range (Range, fromRange)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Gundeck.Env
import Gundeck.Options (NotificationTTL (..), internalPageSize, notificationTTL, settings)
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

data Payload = Payload

type PayloadId = Id 'Payload

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

fetchId :: (MonadClient m) => UserId -> NotificationId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchId u n c = runMaybeT $ do
  row <- MaybeT $ retry x1 $ query1 cqlById (params LocalQuorum (u, n))
  MaybeT $ fetchPayload c row
  where
    cqlById :: PrepQuery R (UserId, NotificationId) NotifRow
    cqlById =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id = ?"

fetchLast :: (MonadReader Env m, MonadClient m) => UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast u c = do
  pageSize <- fromMaybe 100 <$> asks (^. options . settings . internalPageSize)
  go (Page True [] (firstPage pageSize))
  where
    go page = case result page of
      (row : rows) -> do
        mNotif <- fetchPayload c row
        case mNotif of
          Nothing -> go page {result = rows}
          Just notif -> pure (Just notif)
      [] | hasMore page -> do
        page' <- liftClient (nextPage page)
        go page'
      _ -> pure Nothing

    -- The first page consists of at most one row. We retrieve the first page
    -- with a direct query with a LIMIT, and the following pages using
    -- Cassandra pagination.
    firstPage pageSize = do
      results <- retry x1 $ query cqlLast (params LocalQuorum (Identity u))
      let nextPage = case results of
            [] -> pure emptyPage
            (n : _) ->
              retry x1 $
                paginate cqlSeek (paramsP LocalQuorum (u, n ^. _1) pageSize)
      pure $ Page True results nextPage

    cqlLast :: PrepQuery R (Identity UserId) NotifRow
    cqlLast =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id DESC LIMIT 1"
    cqlSeek :: PrepQuery R (UserId, TimeUuid) NotifRow
    cqlSeek =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id < ? \
      \ORDER BY id DESC"

fetchPayload :: (MonadClient m) => Maybe ClientId -> NotifRow -> m (Maybe QueuedNotification)
fetchPayload c (id_, mbPayload, mbPayloadRef, _mbPayloadRefSize, mbClients) =
  case (mbPayload, mbPayloadRef) of
    (Just payload, _) -> pure $ toNotifSingle c (id_, payload, mbClients)
    (_, Just payloadRef) -> runMaybeT $ do
      pl <- MaybeT $ fmap (fmap runIdentity) (query1 cqlSelectPayload (params LocalQuorum (Identity payloadRef)))
      maybe mzero pure $ toNotifSingle c (id_, pl, mbClients)
    _ -> pure Nothing
  where
    cqlSelectPayload :: PrepQuery R (Identity PayloadId) (Identity Blob)
    cqlSelectPayload = "SELECT payload from notification_payload where id = ?"

type NotifRow = (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))

fetch :: forall m. (MonadReader Env m, MonadClient m, MonadUnliftIO m) => UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch u c mSince (fromIntegral . fromRange -> pageSize) = do
  chan <- readMVar =<< view rabbitmqChannel
  notifsTVar <- newTVarIO []
  notifsFullMVar <- newEmptyMVar
  liftIO $ Q.qos chan 0 1 False
  let processMsg (msg, _envelope) = do
        isFull <- atomically $ stateTVar notifsTVar $ \allMsgs ->
          let allMsgsNew = allMsgs <> [msg]
           in (length allMsgsNew >= pageSize, allMsgsNew)
        when isFull $ void $ tryPutMVar notifsFullMVar ()
  consumerTag <-
    liftIO $
      Q.consumeMsgs'
        chan
        (userStreamName u)
        Q.Ack
        processMsg
        (const $ pure ())
        (Q.FieldTable $ Map.singleton "x-stream-offset" $ maybe (Q.FVString "first") (Q.FVInt64 . read . Text.unpack) mSince)
  -- This is a weird hack because we cannot know when we're done fetching notifs.
  mFull <- timeout (1_000_000) (takeMVar notifsFullMVar)
  liftIO $ Q.cancelConsumer chan consumerTag
  msgs <- readTVarIO notifsTVar
  -- TODO: What is the starting notif id, assumed 0 here, but obv wrong. Q.msgTimestamp?
  notifs <- fmap catMaybes . traverse mkNotifs $ zip msgs [0 ..]
  pure $
    ResultPage
      { resultSeq = Seq.fromList notifs,
        resultHasMore = isJust mFull,
        resultGap = False
      }
  where
    mkNotifs :: (Q.Message, Int) -> m (Maybe QueuedNotification)
    mkNotifs (msg, offset) =
      case Aeson.decode @StoredMessage (Q.msgBody msg) of
        Nothing -> pure Nothing -- TODO: Log this
        Just sm ->
          if sm.smTargetClients == mempty || maybe True (flip Set.member sm.smTargetClients) c
            then pure $ Just $ queuedNotification (Text.pack $ show offset) sm.smEvent
            else pure Nothing

data StoredMessage = StoredMessage
  { smTargetClients :: Imports.Set ClientId,
    smEvent :: NonEmpty Aeson.Object
  }

instance JSON.FromJSON StoredMessage where
  parseJSON = JSON.withObject "StoredMessage" $ \o ->
    StoredMessage
      <$> o JSON..: "target_clients"
      <*> o JSON..: "event"

deleteAll :: (MonadClient m) => UserId -> m ()
deleteAll u = write cql (params LocalQuorum (Identity u)) & retry x5
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM notifications WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

toNotifSingle ::
  Maybe ClientId ->
  (TimeUuid, Blob, Maybe (C.Set ClientId)) ->
  Maybe QueuedNotification
toNotifSingle c (i, b, cs) =
  let clients = maybe [] fromSet cs
      notifId = Id (fromTimeUuid i)
   in case JSON.decode' (fromBlob b) of
        Nothing -> Nothing
        Just pl ->
          -- nb. At some point we should be able to do:
          -- @@@ if null clients || maybe False (`elem` clients) c @@@
          -- i.e. not return notifications targeted at specific clients,
          -- if no client ID is given. We currently return all of them
          -- in this case for backward compatibility with existing internal
          -- clients.
          if null clients || maybe True (`elem` clients) c
            then Just (queuedNotification (Text.pack $ show notifId) pl)
            else Nothing
