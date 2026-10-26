{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | The user push-notification store, extracted from "Gundeck.Notification.Data".
module Wire.NotificationStore
  ( NotificationStore (..),
    addNotification,
    deleteAllNotifications,
    fetchLastNotification,
    fetchNotificationById,
    fetchNotifications,
    ResultPage (..),
    NotificationTTL (..),
    NotificationStoreConfig (..),
    Payload,
    PayloadId,
    NotifRow,
    mkResultPage,
    payloadSize,
    toNotifSingle,
  )
where

import Cassandra as C
import Data.Aeson (FromJSON)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Range (Range)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Imports
import Polysemy
import Wire.API.Internal.Notification

-- | TTL (seconds) of stored notifications. Config key @notificationTTL@;
-- moved here from "Gundeck.Options".
newtype NotificationTTL = NotificationTTL
  {notificationTTLSeconds :: Word32}
  deriving (Eq, Ord, Show, Generic, FromJSON)

-- | Explicit replacement for the @MonadReader Env@ config reads of the
-- original "Gundeck.Notification.Data" module. 'Maybe' fields keep the
-- defaults (100 / 5 MiB) inside the store implementations.
data NotificationStoreConfig = NotificationStoreConfig
  { nscPageSize :: Maybe Int32,
    nscMaxPayloadLoadSize :: Maybe Int32
  }

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

data NotificationStore m a where
  AddNotification ::
    NotificationId ->
    NonEmpty NotificationTarget ->
    NonEmpty JSON.Object ->
    NotificationTTL ->
    NotificationStore m ()
  FetchNotifications ::
    UserId ->
    Maybe ClientId ->
    Maybe NotificationId ->
    Range 100 10000 Int32 ->
    NotificationStore m ResultPage
  FetchNotificationById ::
    UserId ->
    NotificationId ->
    Maybe ClientId ->
    NotificationStore m (Maybe QueuedNotification)
  FetchLastNotification ::
    UserId ->
    Maybe ClientId ->
    NotificationStore m (Maybe QueuedNotification)
  DeleteAllNotifications ::
    UserId ->
    NotificationStore m ()

makeSem ''NotificationStore

data Payload = Payload

type PayloadId = Id 'Payload

type NotifRow = (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))

mkResultPage :: Int -> Bool -> Seq QueuedNotification -> ResultPage
mkResultPage size more ns =
  ResultPage
    { resultSeq = Seq.take size ns,
      resultHasMore = Seq.length ns > size || more,
      resultGap = False
    }

payloadSize :: NotifRow -> Int32
payloadSize (_, mbPayload, _, mbPayloadRefSize, _) =
  case (mbPayload, mbPayloadRefSize) of
    (Just blob, _) -> fromIntegral $ BSL.length (fromBlob blob)
    (_, Just size) -> size
    _ -> 0

-- | Pure helper shared by both interpreters: decode a notification payload
-- and apply the client filter. An empty (or absent) client set means the
-- notification is targeted at all clients.
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
            then Just (queuedNotification notifId pl)
            else Nothing
