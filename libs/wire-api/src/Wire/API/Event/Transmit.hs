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

-- | Dispatcher that applies the 'Transmit' instances of the event family
-- modules to the untyped event objects stored and relayed by gundeck and
-- cannon.  Only the JSON \"type\" strings governed by a family instance are
-- ever decoded; everything else is passed through unchanged.
module Wire.API.Event.Transmit
  ( transmitEvent,
    transmitQueuedNotification,
    transmitInternalNotification,
  )
where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Imports
import Wire.API.Event.Conversation qualified as Conversation
import Wire.API.Event.Meeting qualified as Meeting
import Wire.API.Internal.Notification qualified as InternalNotification
import Wire.API.Notification
import Wire.API.Routes.Version

-- | Highest version for which any gate applies; requests at >= this version
-- are passed through byte-identical without decoding.  Bump when adding a
-- gate above V15.
maxGateVersion :: Version
maxGateVersion = V15

-- | Exact JSON \"type\" strings governed by the family instances above.  Only
-- these are ever decoded; every other event type is passed through unchanged.
conversationMeetingEventTypes, meetingEventTypes :: Set Text
conversationMeetingEventTypes =
  Set.fromList
    [ "conversation.create-meeting",
      "conversation.delete-meeting"
    ]
meetingEventTypes =
  Set.fromList
    [ "meeting.create",
      "meeting.update",
      "meeting.delete",
      "meeting.member-add"
    ]

-- | Adjust a stored event object for delivery to a client that called the API
-- at the given 'Version'.  'Nothing' = do not deliver.
--
-- Unknown event types and objects without a \"type\" key are always delivered
-- (forward compatibility).  A gated type that cannot be decoded below its gate
-- is dropped (fail closed).
transmitEvent :: Version -> Event -> Maybe Event
transmitEvent v o
  | v >= maxGateVersion = Just o
  | otherwise = case KeyMap.lookup "type" o of
      Just (A.String t)
        | t `Set.member` meetingEventTypes -> reTransmit @Meeting.Event
        | t `Set.member` conversationMeetingEventTypes -> reTransmit @Conversation.Event
      _ -> Just o
  where
    reTransmit :: forall e. (Transmit e, Eq e, A.FromJSON e, A.ToJSON e) => Maybe Event
    reTransmit = case A.fromJSON @e (A.Object o) of
      A.Success e -> case transmit e v of
        Nothing -> Nothing
        Just e'
          -- byte-preserve the stored object when the event is unchanged
          | e == e' -> Just o
          | otherwise -> case A.toJSON e' of
              A.Object o' -> Just o'
              _ -> Just o
      -- a gated type we cannot decode must not leak below its gate:
      A.Error _ -> Nothing

-- | 'transmitEvent' over the payload of a 'QueuedNotification'; 'Nothing'
-- when the payload empties.
transmitQueuedNotification :: Version -> QueuedNotification -> Maybe QueuedNotification
transmitQueuedNotification v n = do
  payload <- NonEmpty.nonEmpty (mapMaybe (transmitEvent v) (NonEmpty.toList (n ^. queuedNotificationPayload)))
  pure (n & queuedNotificationPayload .~ payload)

-- | 'transmitEvent' over the payload of an internal 'Notification'; 'Nothing'
-- when the payload empties.
transmitInternalNotification ::
  Version ->
  InternalNotification.Notification ->
  Maybe InternalNotification.Notification
transmitInternalNotification v n = do
  payload <- NonEmpty.nonEmpty (mapMaybe (transmitEvent v) (NonEmpty.toList (InternalNotification.ntfPayload n)))
  pure n {InternalNotification.ntfPayload = payload}
