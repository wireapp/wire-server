{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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

module Wire.MeetingsStore.Postgres
  ( interpretMeetingsStoreToPostgres,
  )
where

import Data.Id
import Data.Profunctor (dimap)
import Data.Range (Range)
import Data.Time.Clock
import Data.UUID (UUID, nil)
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (throw)
import Polysemy.Input
import Wire.API.Meeting (Recurrence)
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..))
import Wire.API.User.Identity (EmailAddress)
import Wire.MeetingsStore
import Wire.Postgres (PGConstraints)

interpretMeetingsStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor MeetingsStore r
interpretMeetingsStoreToPostgres =
  interpret $ \case
    CreateMeeting title creator startTime endTime recurrence convId emails trial ->
      createMeetingImpl title creator startTime endTime recurrence convId emails trial
    GetMeeting meetingId ->
      getMeetingImpl meetingId

createMeetingImpl ::
  (PGConstraints r) =>
  Range 1 256 Text ->
  UserId ->
  UTCTime ->
  UTCTime ->
  Maybe Recurrence ->
  ConvId ->
  [EmailAddress] ->
  Bool ->
  Sem r StoredMeeting
createMeetingImpl title creator startTime endTime recurrence convId emails trial = do
  pool <- input
  now <- liftIO getCurrentTime
  let sm =
        StoredMeeting
          { id = Id nil,
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
  dimap (tupleWithoutId . postgresMarshall @StoredMeetingTuple @StoredMeeting) Imports.id $
    refineResult
      (postgresUnmarshall @StoredMeetingTuple @StoredMeeting)
      [singletonStatement|
        INSERT INTO meetings
        (title, creator, start_time, end_time,
         recurrence_frequency, recurrence_interval, recurrence_until,
         conversation_id, invited_emails, trial, created_at, updated_at)
        VALUES
        ($1 :: text, $2 :: uuid, $3 :: timestamptz, $4 :: timestamptz,
         $5 :: text? :: recurrence_frequency, $6 :: int4?, $7 :: timestamptz?,
         $8 :: uuid, $9 :: text[], $10 :: boolean, $11 :: timestamptz, $12 :: timestamptz)
        RETURNING
          id :: uuid, title :: text, creator :: uuid,
          start_time :: timestamptz, end_time :: timestamptz,
          recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
          conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
          created_at :: timestamptz, updated_at :: timestamptz
      |]
  where
    tupleWithoutId (_, t, c, st, et, rf, ri, ru, ci, ie, tr, ca, ua) =
      (t, c, st, et, rf, ri, ru, ci, ie, tr, ca, ua)

getMeetingImpl ::
  (PGConstraints r) =>
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
