{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.List qualified as List
import Data.Profunctor (dimap)
import Data.Range (Range, fromRange)
import Data.Time.Clock
import Data.UUID (UUID, nil)
import Data.Vector qualified as V
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Wire.API.Meeting (Recurrence, TimeZone, renderTimeZone)
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..), dimapPG)
import Wire.API.User.Identity (EmailAddress, fromEmail)
import Wire.MeetingsStore
import Wire.Postgres

interpretMeetingsStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor MeetingsStore r
interpretMeetingsStoreToPostgres =
  interpret $ \case
    CreateMeeting title creator startTime endTime tzid recurrence convId emails trial ->
      createMeetingImpl title creator startTime endTime tzid recurrence convId emails trial
    UpdateMeeting meetingId title startDate endTime tzid schedule ->
      updateMeetingImpl meetingId title startDate endTime tzid schedule
    DeleteMeeting meetingId ->
      deleteMeetingImpl meetingId
    GetMeeting meetingId ->
      getMeetingImpl meetingId
    ListMeetingsByUser userId cutoffTime ->
      listMeetingsByUserImpl userId cutoffTime
    ListMeetingsByConversation convId cutoffTime ->
      listMeetingsByConversationImpl convId cutoffTime
    AddInvitedEmails meetingId email ->
      addInvitedEmailsImpl meetingId email
    RemoveInvitedEmails meetingId emails ->
      removeInvitedEmailsImpl meetingId emails
    ReplaceInvitedEmails meetingId emails ->
      replaceInvitedEmailsImpl meetingId emails
    GetOldMeetings cutoffTime batchSize ->
      getOldMeetingsImpl cutoffTime batchSize

-- * Create

createMeetingImpl ::
  (PGConstraints r) =>
  Range 1 256 Text ->
  UserId ->
  UTCTime ->
  UTCTime ->
  TimeZone ->
  Maybe Recurrence ->
  ConvId ->
  [EmailAddress] ->
  Bool ->
  Sem r StoredMeeting
createMeetingImpl title creator startTime endTime tzid recurrence convId emails trial = do
  now <- liftIO getCurrentTime
  let sm =
        StoredMeeting
          { id = Id nil,
            title = title,
            creator = creator,
            startTime = startTime,
            endTime = endTime,
            tzid = tzid,
            recurrence = recurrence,
            conversationId = convId,
            invitedEmails = emails,
            trial = trial,
            createdAt = now,
            updatedAt = now
          }
  runStatement sm insertStatement

insertStatement :: Statement StoredMeeting StoredMeeting
insertStatement =
  dimap (tupleWithoutId . postgresMarshall @StoredMeetingTuple @StoredMeeting) Imports.id $
    refineResult
      (postgresUnmarshall @StoredMeetingTuple @StoredMeeting)
      [singletonStatement|
        INSERT INTO meetings
        (title, creator, start_time, end_time, tzid,
         recurrence_frequency, recurrence_interval, recurrence_until,
         conversation_id, invited_emails, trial, created_at, updated_at)
        VALUES
        ($1 :: text, $2 :: uuid, $3 :: timestamptz, $4 :: timestamptz, $5 :: text,
         $6 :: text? :: recurrence_frequency, $7 :: int4?, $8 :: timestamptz?,
         $9 :: uuid, $10 :: text[], $11 :: boolean, $12 :: timestamptz, $13 :: timestamptz)
        RETURNING
          id :: uuid, title :: text, creator :: uuid,
          start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
          recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
          conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
          created_at :: timestamptz, updated_at :: timestamptz
      |]
  where
    tupleWithoutId (_, t, c, st, et, tz, rf, ri, ru, ci, ie, tr, ca, ua) =
      (t, c, st, et, tz, rf, ri, ru, ci, ie, tr, ca, ua)

-- * Update

type UpdateStoredMeetingWithRecurrenceTuple =
  ( Maybe Text, -- title
    Maybe UTCTime, -- start_time
    Maybe UTCTime, -- end_time
    Maybe Text, -- tzid
    Maybe Text, -- recurrence_frequency
    Maybe Int32, -- recurrence_interval
    Maybe UTCTime, -- recurrence_until
    UUID -- meeting id
  )

type UpdateMeetingWithRecurrenceTuple =
  ( Maybe (Range 1 256 Text), -- title
    Maybe UTCTime, -- start_time
    Maybe UTCTime, -- end_time
    Maybe TimeZone, -- tzid
    Maybe Recurrence, -- recurrence
    MeetingId -- meeting id
  )

instance PostgresMarshall UpdateStoredMeetingWithRecurrenceTuple UpdateMeetingWithRecurrenceTuple where
  postgresMarshall (mTitle, mStartTime, mEndTime, mTzid, recurrence, id') =
    let (rFreq, rInterval, rUntil) = postgresMarshall recurrence
     in ( fromRange <$> mTitle,
          mStartTime,
          mEndTime,
          renderTimeZone <$> mTzid,
          rFreq,
          rInterval,
          rUntil,
          toUUID id'
        )

type UpdateStoredMeetingWithoutRecurrenceTuple =
  ( Maybe Text, -- title
    Maybe UTCTime, -- start_time
    Maybe UTCTime, -- end_time
    Maybe Text, -- tzid
    UUID -- meeting id
  )

type UpdateMeetingWithoutRecurrenceTuple =
  ( Maybe (Range 1 256 Text), -- title
    Maybe UTCTime, -- start_time
    Maybe UTCTime, -- end_time
    Maybe TimeZone, -- tzid
    MeetingId -- meeting id
  )

instance {-# OVERLAPPING #-} PostgresMarshall UpdateStoredMeetingWithoutRecurrenceTuple UpdateMeetingWithoutRecurrenceTuple where
  postgresMarshall (mTitle, mStartTime, mEndTime, mTzid, id') =
    ( fromRange <$> mTitle,
      mStartTime,
      mEndTime,
      renderTimeZone <$> mTzid,
      toUUID id'
    )

updateMeetingImpl ::
  (PGConstraints r) =>
  MeetingId ->
  Maybe (Range 1 256 Text) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe TimeZone ->
  Maybe (Maybe Recurrence) ->
  Sem r (Maybe StoredMeeting)
updateMeetingImpl meetingId mTitle mStartDate mEndTime mTzid mRecurrence = do
  case mRecurrence of
    Nothing ->
      runStatement (mTitle, mStartDate, mEndTime, mTzid, meetingId) updateWithoutRecurrenceStatement
    Just recurrence ->
      runStatement (mTitle, mStartDate, mEndTime, mTzid, recurrence, meetingId) updateWithRecurrenceStatement
  where
    updateWithRecurrenceStatement :: Statement UpdateMeetingWithRecurrenceTuple (Maybe StoredMeeting)
    updateWithRecurrenceStatement =
      dimapPG
        @UpdateStoredMeetingWithRecurrenceTuple
        @UpdateMeetingWithRecurrenceTuple
        @(Maybe StoredMeetingTuple)
        @(Maybe StoredMeeting)
        [maybeStatement|
          UPDATE meetings
          SET title = COALESCE($1 :: text?, title),
              start_time = COALESCE($2 :: timestamptz?, start_time),
              end_time = COALESCE($3 :: timestamptz?, end_time),
              tzid = COALESCE($4 :: text?, tzid),
              recurrence_frequency = $5 :: text? :: recurrence_frequency,
              recurrence_interval = $6 :: int4?,
              recurrence_until = $7 :: timestamptz?,
              updated_at = NOW()
          WHERE id = ($8 :: uuid)
          RETURNING
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
        |]

    updateWithoutRecurrenceStatement :: Statement UpdateMeetingWithoutRecurrenceTuple (Maybe StoredMeeting)
    updateWithoutRecurrenceStatement =
      dimapPG
        @UpdateStoredMeetingWithoutRecurrenceTuple
        @UpdateMeetingWithoutRecurrenceTuple
        @(Maybe StoredMeetingTuple)
        @(Maybe StoredMeeting)
        [maybeStatement|
          UPDATE meetings
          SET title = COALESCE($1 :: text?, title),
              start_time = COALESCE($2 :: timestamptz?, start_time),
              end_time = COALESCE($3 :: timestamptz?, end_time),
              tzid = COALESCE($4 :: text?, tzid),
              updated_at = NOW()
          WHERE id = ($5 :: uuid)
          RETURNING
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
        |]

-- * Delete

deleteMeetingImpl ::
  (PGConstraints r) =>
  MeetingId ->
  Sem r ()
deleteMeetingImpl meetingId = do
  runStatement (toUUID meetingId) deleteStatement
  where
    deleteStatement :: Statement UUID ()
    deleteStatement =
      [resultlessStatement|
        DELETE FROM meetings
        WHERE id = ($1 :: uuid)
      |]

-- * Get

getMeetingImpl ::
  (PGConstraints r) =>
  MeetingId ->
  Sem r (Maybe StoredMeeting)
getMeetingImpl meetingId = do
  runStatement (toUUID meetingId) getMeetingStatement

getMeetingStatement :: Statement UUID (Maybe StoredMeeting)
getMeetingStatement =
  refineResult
    (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
    [maybeStatement|
      SELECT
        id :: uuid, title :: text, creator :: uuid,
        start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
        recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
        conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
        created_at :: timestamptz, updated_at :: timestamptz
      FROM meetings
      WHERE id = $1 :: uuid
    |]

-- * List

listMeetingsByUserImpl ::
  (PGConstraints r) =>
  UserId ->
  UTCTime ->
  Sem r [StoredMeeting]
listMeetingsByUserImpl userId cutoffTime = do
  runStatement (toUUID userId, cutoffTime) $ V.toList <$> listStatement
  where
    listStatement :: Statement (UUID, UTCTime) (V.Vector StoredMeeting)
    listStatement =
      refineResult
        (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
        $ [vectorStatement|
          SELECT
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
          FROM meetings
          WHERE creator = ($1 :: uuid)
            AND (
                 (recurrence_frequency IS NULL AND end_time >= ($2 :: timestamptz))
              OR (recurrence_frequency IS NOT NULL AND recurrence_interval IS NOT NULL
                  AND recurrence_until IS NOT NULL
                  AND GREATEST(end_time, recurrence_until) >= ($2 :: timestamptz))
              OR (recurrence_frequency IS NOT NULL AND recurrence_interval IS NOT NULL
                  AND recurrence_until IS NULL)
            )
          ORDER BY start_time ASC
        |]

listMeetingsByConversationImpl ::
  (PGConstraints r) =>
  ConvId ->
  UTCTime ->
  Sem r [StoredMeeting]
listMeetingsByConversationImpl convId cutoffTime = do
  runStatement (toUUID convId, cutoffTime) $ V.toList <$> listStatement
  where
    listStatement :: Statement (UUID, UTCTime) (V.Vector StoredMeeting)
    listStatement =
      refineResult
        (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
        $ [vectorStatement|
          SELECT
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
          FROM meetings
          WHERE conversation_id = ($1 :: uuid)
            AND (
                 (recurrence_frequency IS NULL AND end_time >= ($2 :: timestamptz))
              OR (recurrence_frequency IS NOT NULL AND recurrence_interval IS NOT NULL
                  AND recurrence_until IS NOT NULL
                  AND GREATEST(end_time, recurrence_until) >= ($2 :: timestamptz))
              OR (recurrence_frequency IS NOT NULL AND recurrence_interval IS NOT NULL
                  AND recurrence_until IS NULL)
            )
          ORDER BY start_time ASC
        |]

addInvitedEmailsImpl ::
  (PGConstraints r) =>
  MeetingId ->
  [EmailAddress] ->
  Sem r ()
addInvitedEmailsImpl meetingId emails = do
  runStatement (V.fromList (fromEmail <$> emails), toUUID meetingId) addEmailStatement
  where
    addEmailStatement :: Statement (V.Vector Text, UUID) ()
    addEmailStatement =
      [resultlessStatement|
        UPDATE meetings
        SET invited_emails = array(SELECT DISTINCT unnest(array_cat(invited_emails, $1 :: text[]))),
            updated_at = NOW()
        WHERE id = ($2 :: uuid)
      |]

removeInvitedEmailsImpl ::
  (PGConstraints r) =>
  MeetingId ->
  [EmailAddress] ->
  Sem r ()
removeInvitedEmailsImpl meetingId emails = do
  runStatement (V.fromList (fromEmail <$> emails), toUUID meetingId) removeEmailStatement
  where
    removeEmailStatement :: Statement (V.Vector Text, UUID) ()
    removeEmailStatement =
      [resultlessStatement|
        UPDATE meetings M
        SET invited_emails = (SELECT array(SELECT unnest(M.invited_emails) EXCEPT SELECT unnest($1 :: text[]))),
            updated_at = NOW()
        WHERE id = ($2 :: uuid)
      |]

replaceInvitedEmailsImpl ::
  (PGConstraints r) =>
  MeetingId ->
  [EmailAddress] ->
  Sem r ()
replaceInvitedEmailsImpl meetingId emails = do
  runStatement (V.fromList (fromEmail <$> emails), toUUID meetingId) replaceEmailStatement
  where
    replaceEmailStatement :: Statement (V.Vector Text, UUID) ()
    replaceEmailStatement =
      [resultlessStatement|
        UPDATE meetings
        SET invited_emails = array(SELECT DISTINCT unnest($1 :: text[])),
            updated_at = NOW()
        WHERE id = ($2 :: uuid)
      |]

getOldMeetingsImpl ::
  (PGConstraints r) =>
  UTCTime ->
  Int ->
  Sem r [StoredMeeting]
getOldMeetingsImpl cutoffTime batchSize = do
  runSession session
  where
    n = fromIntegral batchSize :: Int32
    session :: Session [StoredMeeting]
    session = do
      -- Two separate queries so each branch can use its dedicated partial index:
      --   * non-recurring  -> idx_meetings_end_time_nonrecurring
      --                        (end_time)
      --   * recurring      -> idx_meetings_recurrence_eff_end
      --                        (GREATEST(end_time, recurrence_until))
      -- A single OR query would match neither partial index and force a scan.
      -- Results are merged and re-sorted by 'effectiveEndTime' below.
      nonRecurring <- statement (cutoffTime, n) nonRecurringOldStatement
      recurring <- statement (cutoffTime, n) recurringOldStatement
      pure $
        take batchSize $
          List.sortOn effectiveEndTime (V.toList nonRecurring <> V.toList recurring)
    nonRecurringOldStatement :: Statement (UTCTime, Int32) (V.Vector StoredMeeting)
    nonRecurringOldStatement =
      refineResult
        (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
        $ [vectorStatement|
          SELECT
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
          FROM meetings
          WHERE recurrence_frequency IS NULL
            AND end_time < ($1 :: timestamptz)
          ORDER BY end_time ASC
          LIMIT ($2 :: int4)
        |]
    recurringOldStatement :: Statement (UTCTime, Int32) (V.Vector StoredMeeting)
    recurringOldStatement =
      refineResult
        (traverse (postgresUnmarshall @StoredMeetingTuple @StoredMeeting))
        $ [vectorStatement|
          SELECT
            id :: uuid, title :: text, creator :: uuid,
            start_time :: timestamptz, end_time :: timestamptz, tzid :: text,
            recurrence_frequency :: text?, recurrence_interval :: int4?, recurrence_until :: timestamptz?,
            conversation_id :: uuid, invited_emails :: text[], trial :: boolean,
            created_at :: timestamptz, updated_at :: timestamptz
          FROM meetings
          WHERE recurrence_frequency IS NOT NULL
            AND recurrence_interval IS NOT NULL
            AND recurrence_until IS NOT NULL
            AND GREATEST(end_time, recurrence_until) < ($1 :: timestamptz)
          ORDER BY GREATEST(end_time, recurrence_until) ASC
          LIMIT ($2 :: int4)
        |]
