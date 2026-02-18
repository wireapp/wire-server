{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MeetingsStore.Postgres
  ( interpretMeetingsStoreToPostgres,
  )
where

import Data.Id
import Data.Profunctor (dimap)
import Data.Time.Clock
import Data.UUID (UUID)
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Meeting (Recurrence)
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..))
import Wire.API.User.Identity (EmailAddress)
import Wire.MeetingsStore

interpretMeetingsStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  InterpreterFor MeetingsStore r
interpretMeetingsStoreToPostgres =
  interpret $ \case
    CreateMeeting meetingId title creator startTime endTime recurrence convId emails trial ->
      createMeetingImpl meetingId title creator startTime endTime recurrence convId emails trial
    GetMeeting meetingId ->
      getMeetingImpl meetingId

createMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  MeetingId ->
  Text ->
  UserId ->
  UTCTime ->
  UTCTime ->
  Maybe Recurrence ->
  ConvId ->
  [EmailAddress] ->
  Bool ->
  Sem r StoredMeeting
createMeetingImpl meetingId title creator startTime endTime recurrence convId emails trial = do
  pool <- input
  now <- liftIO getCurrentTime
  let sm =
        StoredMeeting
          { id = meetingId,
            title = title,
            creator = creator,
            startTime = startTime,
            endTime = endTime,
            recurrence = recurrence,
            conversationId = convId,
            invitedEmails = emails,
            trial = trial,
            createdAt = now,
            updatedAt = now
          }
  result <- liftIO $ use pool $ statement sm insertStatement
  either throw pure result

insertStatement :: Statement StoredMeeting StoredMeeting
insertStatement =
  dimap (postgresMarshall @StoredMeetingTuple @StoredMeeting) Imports.id $
    refineResult
      (postgresUnmarshall @StoredMeetingTuple @StoredMeeting)
      [singletonStatement|
        INSERT INTO meetings
        (id, title, creator, start_time, end_time,
         recurrence_frequency, recurrence_interval, recurrence_until,
         conversation_id, invited_emails, trial, created_at, updated_at)
        VALUES
        ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: timestamptz, $5 :: timestamptz,
         $6 :: text?, $7 :: int4?, $8 :: timestamptz?,
         $9 :: uuid, $10 :: text[], $11 :: boolean, $12 :: timestamptz, $13 :: timestamptz)
        RETURNING
          id :: uuid, title :: text, creator :: uuid,
          start_time :: timestamptz, end_time :: timestamptz,
          recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
          conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
          created_at :: timestamptz, updated_at :: timestamptz
      |]

getMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  MeetingId ->
  Sem r (Maybe StoredMeeting)
getMeetingImpl meetingId = do
  pool <- input
  result <- liftIO $ use pool $ statement (toUUID meetingId) getMeetingStatement
  either throw pure result

getMeetingStatement :: Statement UUID (Maybe StoredMeeting)
getMeetingStatement =
  refineResult
    (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
    [maybeStatement|
      SELECT
        id :: uuid, title :: text, creator :: uuid,
        start_time :: timestamptz, end_time :: timestamptz,
        recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
        conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
        created_at :: timestamptz, updated_at :: timestamptz
      FROM meetings
      WHERE id = $1 :: uuid
    |]
