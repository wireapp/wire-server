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
    ListMeetingsByUser userId ->
      listMeetingsByUserImpl userId
    ListMeetingsByConversation convId ->
      listMeetingsByConversationImpl convId
    UpdateMeeting meetingId title startDate endDate schedule ->
      updateMeetingImpl meetingId title startDate endDate schedule
    DeleteMeeting meetingId ->
      deleteMeetingImpl meetingId
    AddInvitedEmail meetingId email ->
      addInvitedEmailImpl meetingId email
    RemoveInvitedEmail meetingId email ->
      removeInvitedEmailImpl meetingId email
    GetOldMeetings cutoffTime batchSize ->
      getOldMeetingsImpl cutoffTime batchSize
    DeleteMeetingBatch meetingIds ->
      deleteMeetingBatchImpl meetingIds

createMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  UserId ->
  Text ->
  UTCTime ->
  UTCTime ->
  Maybe Text ->
  Qualified ConvId ->
  [EmailAddress] ->
  Bool ->
  Sem r ()
createMeetingImpl qMeetingId creator title startDate endDate schedule qConvId emails trial = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement params insertStatement

    params =
      ( UUID.toText (API.unMeetingId (qUnqualified qMeetingId)),
        _domainText (qDomain qMeetingId),
        title,
        toUUID creator,
        startDate,
        endDate,
        schedule,
        toUUID (qUnqualified qConvId),
        _domainText (qDomain qConvId),
        V.fromList (map fromEmail emails),
        trial
      )

    insertStatement :: Statement (Text, Text, Text, UUID.UUID, UTCTime, UTCTime, Maybe Text, UUID.UUID, Text, V.Vector Text, Bool) ()
    insertStatement =
      [resultlessStatement|
        INSERT INTO meetings
        (id, domain, title, creator, start_date, end_date, schedule,
         conversation_id, conversation_domain, invited_emails, trial)
        VALUES
        ($1 :: text :: uuid, $2 :: text, $3 :: text, $4 :: uuid, $5 :: timestamptz,
         $6 :: timestamptz, $7 :: text?, $8 :: uuid, $9 :: text, $10 :: text[], $11 :: boolean)
      |]

getMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  Sem r (Maybe API.Meeting)
getMeetingImpl qMeetingId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session (Maybe API.Meeting)
    session = statement (_domainText (qDomain qMeetingId), UUID.toText (API.unMeetingId (qUnqualified qMeetingId))) getMeetingStatement

    getMeetingStatement :: Statement (Text, Text) (Maybe API.Meeting)
    getMeetingStatement =
      dimap
        Imports.id
        (fmap rowToMeeting)
        $ [maybeStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, schedule :: text?,
                 conversation_id :: uuid, conversation_domain :: text,
                 invited_emails :: text[], trial :: boolean
          FROM meetings
          WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
        |]

listMeetingsByUserImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  Sem r [API.Meeting]
listMeetingsByUserImpl userId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session [API.Meeting]
    session = statement (toUUID userId) listStatement

    listStatement :: Statement UUID.UUID [API.Meeting]
    listStatement =
      dimap
        Imports.id
        (V.toList . fmap rowToMeeting)
        $ [vectorStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, schedule :: text?,
                 conversation_id :: uuid, conversation_domain :: text,
                 invited_emails :: text[], trial :: boolean
          FROM meetings
          WHERE creator = ($1 :: uuid)
          ORDER BY start_date ASC
        |]

listMeetingsByConversationImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified ConvId ->
  Sem r [API.Meeting]
listMeetingsByConversationImpl qConvId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session [API.Meeting]
    session = statement (toUUID (qUnqualified qConvId), _domainText (qDomain qConvId)) listStatement

    listStatement :: Statement (UUID.UUID, Text) [API.Meeting]
    listStatement =
      dimap
        Imports.id
        (V.toList . fmap rowToMeeting)
        $ [vectorStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, schedule :: text?,
                 conversation_id :: uuid, conversation_domain :: text,
                 invited_emails :: text[], trial :: boolean
          FROM meetings
          WHERE conversation_id = ($1 :: uuid) AND conversation_domain = ($2 :: text)
          ORDER BY start_date ASC
        |]

updateMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Sem r (Maybe API.Meeting)
updateMeetingImpl qMeetingId mTitle mStartDate mEndDate mSchedule = do
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
          mSchedule,
          _domainText (qDomain qMeetingId),
          UUID.toText (API.unMeetingId (qUnqualified qMeetingId))
        )
        updateStatement
      statement (_domainText (qDomain qMeetingId), UUID.toText (API.unMeetingId (qUnqualified qMeetingId))) getMeetingStatement

    updateStatement :: Statement (Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe Text, Text, Text) ()
    updateStatement =
      [resultlessStatement|
        UPDATE meetings
        SET title = COALESCE($1 :: text?, title),
            start_date = COALESCE($2 :: timestamptz?, start_date),
            end_date = COALESCE($3 :: timestamptz?, end_date),
            schedule = COALESCE($4 :: text?, schedule)
        WHERE domain = ($5 :: text) AND id :: text = ($6 :: text)
      |]

    getMeetingStatement :: Statement (Text, Text) (Maybe API.Meeting)
    getMeetingStatement =
      dimap
        Imports.id
        (fmap rowToMeeting)
        $ [maybeStatement|
          SELECT id :: text :: uuid, domain :: text, title :: text, creator :: uuid,
                 start_date :: timestamptz, end_date :: timestamptz, schedule :: text?,
                 conversation_id :: uuid, conversation_domain :: text,
                 invited_emails :: text[], trial :: boolean
          FROM meetings
          WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
        |]

deleteMeetingImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  Sem r ()
deleteMeetingImpl qMeetingId = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (_domainText (qDomain qMeetingId), UUID.toText (API.unMeetingId (qUnqualified qMeetingId))) deleteStatement

    deleteStatement :: Statement (Text, Text) ()
    deleteStatement =
      [resultlessStatement|
        DELETE FROM meetings
        WHERE domain = ($1 :: text) AND id :: text = ($2 :: text)
      |]

addInvitedEmailImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  EmailAddress ->
  Sem r ()
addInvitedEmailImpl qMeetingId email = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (fromEmail email, _domainText (qDomain qMeetingId), UUID.toText (API.unMeetingId (qUnqualified qMeetingId))) addEmailStatement

    addEmailStatement :: Statement (Text, Text, Text) ()
    addEmailStatement =
      [resultlessStatement|
        UPDATE meetings
        SET invited_emails = array_append(invited_emails, $1 :: text)
        WHERE domain = ($2 :: text) AND id :: text = ($3 :: text)
          AND NOT ($1 :: text = ANY(invited_emails))
      |]

removeInvitedEmailImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Qualified API.MeetingId ->
  EmailAddress ->
  Sem r ()
removeInvitedEmailImpl qMeetingId email = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session ()
    session = statement (fromEmail email, _domainText (qDomain qMeetingId), UUID.toText (API.unMeetingId (qUnqualified qMeetingId))) removeEmailStatement

    removeEmailStatement :: Statement (Text, Text, Text) ()
    removeEmailStatement =
      [resultlessStatement|
        UPDATE meetings
        SET invited_emails = array_remove(invited_emails, $1 :: text)
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
                 start_date :: timestamptz, end_date :: timestamptz, schedule :: text?,
                 conversation_id :: uuid, conversation_domain :: text,
                 invited_emails :: text[], trial :: bool
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
  [Qualified API.MeetingId] ->
  Sem r Int64
deleteMeetingBatchImpl meetingIds = do
  pool <- input
  result <- liftIO $ use pool session
  either throw pure result
  where
    session :: Session Int64
    session = foldM deleteSingle 0 meetingIds

    deleteSingle :: Int64 -> Qualified API.MeetingId -> Session Int64
    deleteSingle acc qMeetingId = do
      count <- statement (UUID.toText (API.unMeetingId (qUnqualified qMeetingId)), _domainText (qDomain qMeetingId)) deleteStatement
      pure (acc + count)

    deleteStatement :: Statement (Text, Text) Int64
    deleteStatement =
      [rowsAffectedStatement|
        DELETE FROM meetings
        WHERE id :: text = ($1 :: text) AND domain = ($2 :: text)
      |]

-- Helper functions

rowToMeeting :: (UUID, Text, Text, UUID, UTCTime, UTCTime, Maybe Text, UUID, Text, V.Vector Text, Bool) -> API.Meeting
rowToMeeting (meetingIdUUID, domainText_, titleText, creatorUUID, startDate', endDate', schedule', convIdUUID, convDomainText, emailsVec, trial') =
  let meetingId' = API.MeetingId meetingIdUUID
      domain' = Domain domainText_
      qMeetingId = Qualified meetingId' domain'
      creator' = Id creatorUUID
      convId' = Id convIdUUID
      convDomain' = Domain convDomainText
      qConvId = Qualified convId' convDomain'
      emails' = mapMaybe emailAddressText (V.toList emailsVec)
   in API.Meeting
        { API.id = qMeetingId,
          API.title = titleText,
          API.creator = creator',
          API.startDate = startDate',
          API.endDate = endDate',
          API.schedule = schedule',
          API.conversationId = qConvId,
          API.invitedEmails = emails',
          API.trial = trial'
        }
