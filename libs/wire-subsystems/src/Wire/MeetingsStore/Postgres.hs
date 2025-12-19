{-# LANGUAGE RecordWildCards #-}

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

import Data.Aeson (Result (Success), Value, fromJSON, toJSON)
import Data.Domain (Domain (..), _domainText)
import Data.Id
import Data.Profunctor (dimap)
import Data.Qualified
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Meeting qualified as API
import Wire.API.User.Identity (EmailAddress, emailAddressText, fromEmail)
import Wire.MeetingsStore

interpretMeetingsStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  InterpreterFor MeetingsStore r
interpretMeetingsStoreToPostgres =
  interpret $ \case
    CreateMeeting meetingId creator title startDate endDate schedule convId emails trial ->
      createMeetingImpl meetingId creator title startDate endDate schedule convId emails trial
    GetMeeting meetingId ->
      getMeetingImpl meetingId
    ListMeetingsByUser userId cutoffTime ->
      listMeetingsByUserImpl userId cutoffTime
    ListMeetingsByConversation convId cutoffTime ->
      listMeetingsByConversationImpl convId cutoffTime
    UpdateMeeting meetingId title startDate endDate schedule ->
      updateMeetingImpl meetingId title startDate endDate schedule
    DeleteMeeting meetingId ->
      deleteMeetingImpl meetingId
    AddInvitedEmails meetingId email ->
      addInvitedEmailsImpl meetingId email
    RemoveInvitedEmails meetingId emails ->
      removeInvitedEmailsImpl meetingId emails
    GetOldMeetings cutoffTime batchSize ->
      getOldMeetingsImpl cutoffTime batchSize
    DeleteMeetingBatch meetingIds ->
      deleteMeetingBatchImpl meetingIds

createMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  UserId ->
  Text ->
  UTCTime ->
  UTCTime ->
  Maybe API.Recurrence ->
  ConvId ->
  [EmailAddress] ->
  Bool ->
  Sem r ()
createMeetingImpl qMeetingId creator title startDate endDate recurrence convId emails trial = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement params insertStatement

    params =
      ( toUUID (qUnqualified qMeetingId),
        _domainText (qDomain qMeetingId),
        title,
        toUUID creator,
        startDate,
        endDate,
        fmap toJSON recurrence,
        toUUID convId,
        V.fromList (map fromEmail emails),
        trial
      )

    insertStatement :: Statement (UUID.UUID, Text, Text, UUID.UUID, UTCTime, UTCTime, Maybe Value, UUID.UUID, V.Vector Text, Bool) ()
    insertStatement =
      [resultlessStatement|
        INSERT INTO meetings
        (id, domain, title, creator, start_date, end_date, recurrence,
         conversation_id, invited_emails, trial)
        VALUES
        ($1 :: uuid, $2 :: text, $3 :: text, $4 :: uuid, $5 :: timestamptz,
         $6 :: timestamptz, $7 :: jsonb?, $8 :: uuid, $9 :: text[], $10 :: boolean)
      |]

getMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  Sem r (Maybe API.Meeting)
getMeetingImpl qMeetingId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe API.Meeting)
    session = statement (_domainText (qDomain qMeetingId), idToText (qUnqualified qMeetingId)) getMeetingStatement

    getMeetingStatement :: Statement (Text, Text) (Maybe API.Meeting)
    getMeetingStatement =
      dimap
        Imports.id
        (fmap rowToMeeting)
        $ [maybeStatement|
          SELECT id :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, recurrence :: jsonb?,
                 conversation_id :: uuid,
                 invited_emails :: text[], trial :: boolean, updated_at :: timestamptz
          FROM meetings
          WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
        |]

listMeetingsByUserImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  UTCTime ->
  Sem r [API.Meeting]
listMeetingsByUserImpl userId cutoffTime = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session [API.Meeting]
    session = statement (toUUID userId, cutoffTime) listStatement

    listStatement :: Statement (UUID.UUID, UTCTime) [API.Meeting]
    listStatement =
      dimap
        Imports.id
        (V.toList . fmap rowToMeeting)
        $ [vectorStatement|
          SELECT id :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, recurrence :: jsonb?,
                 conversation_id :: uuid,
                 invited_emails :: text[], trial :: boolean, updated_at :: timestamptz
          FROM meetings
          WHERE creator = ($1 :: uuid) AND end_date >= ($2 :: timestamptz)
          ORDER BY start_date ASC
        |]

listMeetingsByConversationImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified ConvId ->
  UTCTime ->
  Sem r [API.Meeting]
listMeetingsByConversationImpl qConvId cutoffTime = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session [API.Meeting]
    session = statement (toUUID (qUnqualified qConvId), _domainText (qDomain qConvId), cutoffTime) listStatement

    listStatement :: Statement (UUID.UUID, Text, UTCTime) [API.Meeting]
    listStatement =
      dimap
        Imports.id
        (V.toList . fmap rowToMeeting)
        $ [vectorStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, recurrence :: jsonb?,
                 conversation_id :: uuid,
                 invited_emails :: text[], trial :: boolean, updated_at :: timestamptz
          FROM meetings
          WHERE conversation_id = ($1 :: uuid) AND domain = ($2 :: text) AND end_date >= ($3 :: timestamptz)
          ORDER BY start_date ASC
        |]

updateMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe API.Recurrence ->
  Sem r (Maybe API.Meeting)
updateMeetingImpl qMeetingId mTitle mStartDate mEndDate mRecurrence = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe API.Meeting)
    session = do
      statement
        ( mTitle,
          mStartDate,
          mEndDate,
          fmap toJSON mRecurrence,
          _domainText (qDomain qMeetingId),
          idToText (qUnqualified qMeetingId)
        )
        updateStatement
      statement (_domainText (qDomain qMeetingId), idToText (qUnqualified qMeetingId)) getMeetingStatement

    updateStatement :: Statement (Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe Value, Text, Text) ()
    updateStatement =
      [resultlessStatement|
        UPDATE meetings
        SET title = COALESCE($1 :: text?, title),
            start_date = COALESCE($2 :: timestamptz?, start_date),
            end_date = COALESCE($3 :: timestamptz?, end_date),
            recurrence = COALESCE($4 :: jsonb?, recurrence),
            updated_at = NOW()
        WHERE domain = ($5 :: text) AND id :: text = ($6 :: text)
      |]

    getMeetingStatement :: Statement (Text, Text) (Maybe API.Meeting)
    getMeetingStatement =
      dimap
        Imports.id
        (fmap rowToMeeting)
        $ [maybeStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, recurrence :: jsonb?,
                 conversation_id :: uuid,
                 invited_emails :: text[], trial :: boolean, updated_at :: timestamptz
          FROM meetings
          WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
        |]

deleteMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  Sem r ()
deleteMeetingImpl qMeetingId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (_domainText (qDomain qMeetingId), idToText (qUnqualified qMeetingId)) deleteStatement

    deleteStatement :: Statement (Text, Text) ()
    deleteStatement =
      [resultlessStatement|
        DELETE FROM meetings
        WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
      |]

addInvitedEmailsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  [EmailAddress] ->
  Sem r ()
addInvitedEmailsImpl qMeetingId emails = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (V.fromList (fromEmail <$> emails), _domainText (qDomain qMeetingId), idToText (qUnqualified qMeetingId)) addEmailStatement

    addEmailStatement :: Statement (V.Vector Text, Text, Text) ()
    addEmailStatement =
      [resultlessStatement|
        UPDATE meetings
        SET invited_emails = array_cat(invited_emails, $1 :: text[]),
            updated_at = NOW()
        WHERE domain = ($2 :: text) AND id :: text = ($3 :: text)
      |]

removeInvitedEmailsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified MeetingId ->
  [EmailAddress] ->
  Sem r ()
removeInvitedEmailsImpl qMeetingId emails = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (V.fromList (fromEmail <$> emails), _domainText (qDomain qMeetingId), idToText (qUnqualified qMeetingId)) removeEmailStatement

    removeEmailStatement :: Statement (V.Vector Text, Text, Text) ()
    removeEmailStatement =
      [resultlessStatement|
        UPDATE meetings M
        SET invited_emails = (SELECT array(SELECT unnest(M.invited_emails) EXCEPT SELECT unnest($1 :: text[]))),
            updated_at = NOW()
        WHERE domain = ($2 :: text) AND id :: text = ($3 :: text)
      |]

getOldMeetingsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UTCTime ->
  Int ->
  Sem r [API.Meeting]
getOldMeetingsImpl cutoffTime batchSize = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session [API.Meeting]
    session = statement (cutoffTime, fromIntegral batchSize) getOldStatement

    getOldStatement :: Statement (UTCTime, Int32) [API.Meeting]
    getOldStatement =
      dimap
        id
        (fmap rowToMeeting . V.toList)
        [vectorStatement|
          SELECT id :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, recurrence :: jsonb?,
                 conversation_id :: uuid,
                 invited_emails :: text[], trial :: bool, updated_at :: timestamptz
          FROM meetings
          WHERE end_date < ($1 :: timestamptz)
          ORDER BY end_date ASC
          LIMIT ($2 :: int4)
        |]

deleteMeetingBatchImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  [Qualified MeetingId] ->
  Sem r Int64
deleteMeetingBatchImpl meetingIds = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session Int64
    session = statement (V.fromList (toUUID . qUnqualified <$> meetingIds), V.fromList (_domainText . qDomain <$> meetingIds)) deleteStatement

    deleteStatement :: Statement (V.Vector UUID, V.Vector Text) Int64
    deleteStatement =
      [rowsAffectedStatement|
        DELETE FROM meetings
        WHERE (id, domain) IN (SELECT * FROM unnest($1::uuid[], $2::text[]))
      |]

-- Helper functions

rowToMeeting :: (UUID, Text, Text, UUID, UTCTime, UTCTime, Maybe Value, UUID, V.Vector Text, Bool, UTCTime) -> API.Meeting
rowToMeeting (meetingIdUUID, domainText_, titleText, creatorUUID, startDate', endDate', recurrenceJSON, convIdUUID, emailsVec, trial', updatedAt') =
  let meetingId' = Id meetingIdUUID
      domain' = Domain domainText_
      qMeetingId = Qualified meetingId' domain'
      creator' = Id creatorUUID
      qCreator = Qualified creator' domain'
      convId' = Id convIdUUID
      qConvId = Qualified convId' domain'
      emails' = mapMaybe emailAddressText (V.toList emailsVec)
      recurrence' =
        recurrenceJSON >>= \v -> case fromJSON v of
          Success r -> Just r
          _ -> Nothing
   in API.Meeting
        { API.id = qMeetingId,
          API.title = titleText,
          API.creator = qCreator,
          API.startDate = startDate',
          API.endDate = endDate',
          API.recurrence = recurrence',
          API.conversationId = qConvId,
          API.invitedEmails = emails',
          API.trial = trial',
          API.updatedAt = updatedAt'
        }
