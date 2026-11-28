{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Storage operations for the notification fan-in streams. These operations
-- are intentionally separate from the legacy notification stores.
module Wire.FanInNotificationStore
  ( FanInNotification (..),
    FanInNotificationPage (..),
    FanInNotificationStoreError (..),
    fanInNotificationCreatedAt,
    fanInNotificationIdCreatedAt,
    FanInNotificationStore (..),
    appendFanInUserNotifications,
    getFanInUserNotificationPage,
    getLastFanInUserNotificationId,
    hasFanInUserNotification,
    appendFanInClientNotifications,
    getFanInClientNotificationPage,
    getLastFanInClientNotificationId,
    hasFanInClientNotification,
    appendFanInTeamNotifications,
    getFanInTeamNotificationPage,
    getLastFanInTeamNotificationId,
    hasFanInTeamNotification,
    appendFanInEpochNotifications,
    getFanInEpochNotificationPage,
    getLastFanInEpochNotificationId,
    hasFanInEpochNotification,
    appendFanInLocalConnectionNotifications,
    getFanInLocalConnectionNotificationPage,
    getLastFanInLocalConnectionNotificationId,
    hasFanInLocalConnectionNotification,
    appendFanInRemoteConnectionNotifications,
    getFanInRemoteConnectionNotificationPage,
    getLastFanInRemoteConnectionNotificationId,
    hasFanInRemoteConnectionNotification,
  )
where

import Data.Aeson qualified as JSON
import Data.Bits (shiftR, (.&.))
import Data.Domain (Domain)
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Range
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)
import Wire.API.Notification (NotificationId)

-- | A stored fan-in notification. Payload is kept as JSON because the fan-in
-- store persists the serialized event payload without interpreting it.
data FanInNotification = FanInNotification
  { fanInNotificationId :: NotificationId,
    fanInNotificationPayload :: JSON.Value,
    fanInNotificationOrigin :: Maybe UserId
  }
  deriving stock (Eq, Show)

-- | Invalid ids are rejected before accessing the database.
data FanInNotificationStoreError = InvalidFanInNotificationId NotificationId
  deriving stock (Eq, Show)

fanInNotificationCreatedAt :: FanInNotification -> Either FanInNotificationStoreError UTCTime
fanInNotificationCreatedAt = fanInNotificationIdCreatedAt . fanInNotificationId

-- | RFC 9562 section 5.7: the first 48 bits contain Unix milliseconds.
-- Validate both version 7 and the RFC variant before interpreting those bits.
-- Integer/rational arithmetic preserves millisecond precision, including at
-- the maximum 48-bit timestamp; this does not consult the wall clock.
fanInNotificationIdCreatedAt :: NotificationId -> Either FanInNotificationStoreError UTCTime
fanInNotificationIdCreatedAt nid =
  let (high, low, variant, _) = UUID.toWords (toUUID nid)
      milliseconds = toInteger high * 65536 + toInteger (low `shiftR` 16)
   in if (low `shiftR` 12) .&. 0xf == 7 && variant `shiftR` 30 == 2
        then Right (posixSecondsToUTCTime (fromRational (milliseconds % 1000)))
        else Left (InvalidFanInNotificationId nid)

data FanInNotificationPage = FanInNotificationPage
  { fanInNotifications :: [FanInNotification],
    fanInNotificationsHaveMore :: !Bool
  }
  deriving stock (Eq, Show)

-- | Has... checks the retained notification row, independently of the last-id
-- cursor. The subsystem needs both to detect expired, unacknowledged history.
-- All append operations atomically maintain the corresponding last-id cursor
-- using the greatest UUIDv7 id, and are idempotent on replay of the same event.
-- The partition timestamp is derived from the UUIDv7 id, so retries always
-- target the same primary key. Pages exclude the supplied notification cursor.
--
-- TODO: Make cursor ordering safe for concurrent publishers. UUIDv7 allocation
-- order can differ from commit order. If a lower ID commits after delivery has
-- advanced past it, subsequent pages will skip that event. For the first load
-- tests, use one publisher per stream and commit new IDs in increasing order.
data FanInNotificationStore m a where
  -- | Append notifications to user streams. The store also updates each stream's
  -- last-notification cursor in the same transaction.
  AppendFanInUserNotifications :: NonEmpty (UserId, FanInNotification) -> FanInNotificationStore m ()
  GetFanInUserNotificationPage :: UserId -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInUserNotificationId :: UserId -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInUserNotification :: UserId -> NotificationId -> FanInNotificationStore m Bool
  AppendFanInClientNotifications :: NonEmpty ((UserId, ClientId), FanInNotification) -> FanInNotificationStore m ()
  GetFanInClientNotificationPage :: UserId -> ClientId -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInClientNotificationId :: UserId -> ClientId -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInClientNotification :: UserId -> ClientId -> NotificationId -> FanInNotificationStore m Bool
  AppendFanInTeamNotifications :: NonEmpty (TeamId, FanInNotification) -> FanInNotificationStore m ()
  GetFanInTeamNotificationPage :: TeamId -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInTeamNotificationId :: TeamId -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInTeamNotification :: TeamId -> NotificationId -> FanInNotificationStore m Bool
  AppendFanInEpochNotifications :: NonEmpty ((GroupId, Epoch), FanInNotification) -> FanInNotificationStore m ()
  GetFanInEpochNotificationPage :: GroupId -> Epoch -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInEpochNotificationId :: GroupId -> Epoch -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInEpochNotification :: GroupId -> Epoch -> NotificationId -> FanInNotificationStore m Bool
  AppendFanInLocalConnectionNotifications :: NonEmpty (UserId, FanInNotification) -> FanInNotificationStore m ()
  GetFanInLocalConnectionNotificationPage :: UserId -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInLocalConnectionNotificationId :: UserId -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInLocalConnectionNotification :: UserId -> NotificationId -> FanInNotificationStore m Bool
  AppendFanInRemoteConnectionNotifications :: NonEmpty ((Domain, UserId), FanInNotification) -> FanInNotificationStore m ()
  GetFanInRemoteConnectionNotificationPage :: Domain -> UserId -> Maybe NotificationId -> Range 1 10000 Int32 -> FanInNotificationStore m FanInNotificationPage
  GetLastFanInRemoteConnectionNotificationId :: Domain -> UserId -> FanInNotificationStore m (Maybe NotificationId)
  HasFanInRemoteConnectionNotification :: Domain -> UserId -> NotificationId -> FanInNotificationStore m Bool

makeSem ''FanInNotificationStore
