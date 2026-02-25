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
import Wire.API.Meeting (Recurrence (..))
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
    -- | end time of the meeting
    endTime :: UTCTime,
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

type StoredMeetingTuple =
  ( UUID, -- id
    Text, -- title
    UUID, -- creator
    UTCTime, -- start_time
    UTCTime, -- end_time
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
      pure
        StoredMeeting
          { id = Id id',
            title = rTitle,
            creator = Id creator',
            startTime = startTime',
            endTime = endTime',
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
    Maybe Recurrence ->
    ConvId ->
    [EmailAddress] ->
    Bool ->
    MeetingsStore m StoredMeeting
  GetMeeting ::
    MeetingId ->
    MeetingsStore m (Maybe StoredMeeting)

makeSem ''MeetingsStore
