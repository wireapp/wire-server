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

module Wire.MeetingsStore where

import Data.Bifunctor (Bifunctor (first))
import Data.Id
import Data.Range (Range (fromRange), checkedEither)
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Imports
import Polysemy
import Wire.API.Meeting (Recurrence (..), TimeZone, parseTimeZone, renderTimeZone)
import Wire.API.PostgresMarshall
import Wire.API.User.EmailAddress (emailAddressText, fromEmail)
import Wire.API.User.Identity (EmailAddress)

data StoredMeeting = StoredMeeting
  { -- | unique identifier
    id :: MeetingId,
    -- | title of the meeting
    title :: Range 1 256 Text,
    -- | user who created the meeting
    creator :: UserId,
    -- | start time of the meeting
    startTime :: UTCTime,
    -- | end time of the meeting (the indexed effective-end column)
    endTime :: UTCTime,
    -- | IANA time zone identifier of the meeting (NOT NULL; backfilled to
    -- 'defaultLegacyTimeZone' and always supplied on create)
    tzid :: TimeZone,
    -- | optional recurrence pattern
    recurrence :: Maybe Recurrence,
    -- | conversation where the meeting belongs
    conversationId :: ConvId,
    -- | list of invited participants
    invitedEmails :: [EmailAddress],
    -- | whether it's a trial meeting
    trial :: Bool,
    -- | when the record was created
    createdAt :: UTCTime,
    -- | when the record was last updated
    updatedAt :: UTCTime
  }
  deriving (Show, Eq)

-- | Effective end time of a meeting for expiry and cleanup decisions.
--
-- * No recurrence: end_time.
-- * Bounded recurrence ('until' set): @max end_time until@ -- the meeting is
--   still alive while its recurrence window is open, even if the original
--   time slot has passed.
-- * Open-ended recurrence ('until' = 'Nothing'): 'Nothing' -- the meeting
--   never auto-expires and is never picked up by the cleanup worker.
effectiveEndTime :: StoredMeeting -> Maybe UTCTime
effectiveEndTime sm =
  case sm.recurrence of
    Nothing -> Just sm.endTime
    Just r -> max sm.endTime <$> r.until

type StoredMeetingTuple =
  ( UUID, -- id
    Text, -- title
    UUID, -- creator
    UTCTime, -- start_time
    UTCTime, -- end_time
    Text, -- tzid
    Maybe Text, -- recurrence_frequency
    Maybe Int32, -- recurrence_interval
    Maybe UTCTime, -- recurrence_until
    UUID, -- conversation_id
    Data.Vector.Vector Text, -- invited_emails
    Bool, -- trial
    UTCTime, -- created_at
    UTCTime -- updated_at
  )

instance PostgresMarshall StoredMeetingTuple StoredMeeting where
  postgresMarshall storedMeeting =
    let (rFreq, rInterval, rUntil) = postgresMarshall storedMeeting.recurrence
     in ( toUUID storedMeeting.id,
          fromRange storedMeeting.title,
          toUUID storedMeeting.creator,
          storedMeeting.startTime,
          storedMeeting.endTime,
          renderTimeZone storedMeeting.tzid,
          rFreq,
          rInterval,
          rUntil,
          toUUID storedMeeting.conversationId,
          V.fromList (map fromEmail storedMeeting.invitedEmails),
          storedMeeting.trial,
          storedMeeting.createdAt,
          storedMeeting.updatedAt
        )

instance PostgresUnmarshall StoredMeetingTuple StoredMeeting where
  postgresUnmarshall
    ( id',
      title',
      creator',
      startTime',
      endTime',
      tzid',
      rFreq,
      rInterval,
      rUntil,
      conversationId',
      invitedEmails',
      trial',
      createdAt',
      updateAt'
      ) = do
      rTitle <- first T.pack $ checkedEither title'
      recurrence' <- postgresUnmarshall (rFreq, rInterval, rUntil)
      tzid'' <- maybe (Left "invalid tzid") Right (parseTimeZone tzid')
      pure
        StoredMeeting
          { id = Id id',
            title = rTitle,
            creator = Id creator',
            startTime = startTime',
            endTime = endTime',
            tzid = tzid'',
            recurrence = recurrence',
            conversationId = Id conversationId',
            invitedEmails = mapMaybe emailAddressText (V.toList invitedEmails'),
            trial = trial',
            createdAt = createdAt',
            updatedAt = updateAt'
          }

data MeetingsStore m a where
  CreateMeeting ::
    Range 1 256 Text ->
    UserId ->
    UTCTime ->
    UTCTime ->
    TimeZone ->
    Maybe Recurrence ->
    ConvId ->
    [EmailAddress] ->
    Bool ->
    MeetingsStore m StoredMeeting
  UpdateMeeting ::
    MeetingId ->
    Maybe (Range 1 256 Text) ->
    Maybe UTCTime ->
    Maybe UTCTime ->
    Maybe TimeZone ->
    Maybe (Maybe Recurrence) ->
    MeetingsStore m (Maybe StoredMeeting)
  DeleteMeeting ::
    MeetingId ->
    MeetingsStore m ()
  GetMeeting ::
    MeetingId ->
    MeetingsStore m (Maybe StoredMeeting)
  ListMeetingsByUser ::
    UserId ->
    UTCTime ->
    MeetingsStore m [StoredMeeting]
  ListMeetingsByConversation ::
    ConvId ->
    UTCTime ->
    MeetingsStore m [StoredMeeting]
  AddInvitedEmails ::
    MeetingId ->
    [EmailAddress] ->
    MeetingsStore m ()
  RemoveInvitedEmails ::
    MeetingId ->
    [EmailAddress] ->
    MeetingsStore m ()
  ReplaceInvitedEmails ::
    MeetingId ->
    [EmailAddress] ->
    MeetingsStore m ()
  -- Cleanup operations
  GetOldMeetings ::
    UTCTime ->
    Int ->
    MeetingsStore m [StoredMeeting]

makeSem ''MeetingsStore
