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

import Data.Id
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
    title :: Text,
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
  postgresMarshall sm =
    let (rf, ri, ru) = postgresMarshall sm.recurrence
     in ( toUUID sm.id,
          sm.title,
          toUUID sm.creator,
          sm.startTime,
          sm.endTime,
          rf,
          ri,
          ru,
          toUUID sm.conversationId,
          V.fromList (map fromEmail sm.invitedEmails),
          sm.trial,
          sm.createdAt,
          sm.updatedAt
        )

instance PostgresUnmarshall StoredMeetingTuple StoredMeeting where
  postgresUnmarshall (i, t, c, st, et, rf, ri, ru, ci, ie, tr, ca, ua) = do
    rec' <- postgresUnmarshall (rf, ri, ru)
    pure
      StoredMeeting
        { id = Id i,
          title = t,
          creator = Id c,
          startTime = st,
          endTime = et,
          recurrence = rec',
          conversationId = Id ci,
          invitedEmails = mapMaybe emailAddressText (V.toList ie),
          trial = tr,
          createdAt = ca,
          updatedAt = ua
        }

data MeetingsStore m a where
  CreateMeeting ::
    Text ->
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
