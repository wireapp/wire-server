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

module Wire.MeetingsSubsystem.Interpreter where

import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Data.UUID.V4 qualified as UUIDV4
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.CellsState (CellsState (CellsDisabled))
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Meeting
import Wire.API.User (BaseProtocolTag (BaseProtocolMLSTag))
import Wire.API.User.Identity (EmailAddress)
import Wire.ConversationStore qualified as ConvStore
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.StoredConversation
import Wire.UserList

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r,
    Member (Embed IO) r
  ) =>
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem = interpret $ \case
  CreateMeeting zUser newMeeting trial ->
    createMeetingImpl zUser newMeeting trial
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId
  ListMeetings zUser ->
    listMeetingsImpl zUser
  Wire.MeetingsSubsystem.UpdateMeeting zUser meetingId update ->
    updateMeetingImpl zUser meetingId update
  DeleteMeeting zUser meetingId ->
    deleteMeetingImpl zUser meetingId
  AddInvitedEmails zUser meetingId emails ->
    addInvitedEmailsImpl zUser meetingId emails
  RemoveInvitedEmails zUser meetingId emails ->
    removeInvitedEmailsImpl zUser meetingId emails
  CleanupOldMeetings cutoffTime batchSize ->
    cleanupOldMeetingsImpl cutoffTime batchSize

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  NewMeeting ->
  Bool ->
  Sem r (Meeting, StoredConversation)
createMeetingImpl zUser newMeeting trial = do
  -- Generate meeting ID
  meetingId <- liftIO $ MeetingId <$> UUIDV4.nextRandom
  let qMeetingId = tUntagged (qualifyAs zUser meetingId)

  -- Generate new conversation ID
  convId <- liftIO $ randomId
  let lConvId = qualifyAs zUser convId

  -- Create conversation metadata for a meeting
  let metadata =
        ConversationMetadata
          { cnvmType = RegularConv,
            cnvmCreator = Just (tUnqualified zUser),
            cnvmAccess = [],
            cnvmAccessRoles = Set.empty,
            cnvmName = Just newMeeting.title,
            cnvmTeam = Nothing,
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Nothing,
            cnvmGroupConvType = Just MeetingConversation,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsDisabled,
            cnvmParent = Nothing
          }

  -- Create conversation with the meeting creator as the only member (admin role)
  let newConv =
        NewConversation
          { metadata = metadata,
            users = UserList [(tUnqualified zUser, roleNameWireAdmin)] [],
            protocol = BaseProtocolMLSTag,
            groupId = Nothing
          }

  -- Store the conversation
  storedConv <- ConvStore.upsertConversation lConvId newConv
  let qConvId = tUntagged (qualifyAs zUser storedConv.id_)

  -- Store meeting (trial status is provided by caller)
  Store.createMeeting
    qMeetingId
    (tUntagged zUser)
    newMeeting.title
    newMeeting.startDate
    newMeeting.endDate
    newMeeting.recurrence
    qConvId
    newMeeting.invitedEmails
    trial

  -- Return created meeting
  pure
    ( Meeting
        { id = qMeetingId,
          title = newMeeting.title,
          creator = tUntagged zUser,
          startDate = newMeeting.startDate,
          endDate = newMeeting.endDate,
          recurrence = newMeeting.recurrence,
          conversationId = qConvId,
          invitedEmails = newMeeting.invitedEmails,
          trial = trial
        },
      storedConv
    )

getMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  Sem r (Maybe Meeting)
getMeetingImpl zUser meetingId = do
  -- Get meeting from store
  maybeMeeting <- Store.getMeeting meetingId

  case maybeMeeting of
    Nothing -> pure Nothing
    Just meeting -> do
      -- Check authorization: user must be creator OR member of the associated conversation
      let isCreator = meeting.creator == tUntagged zUser
      if isCreator
        then pure (Just meeting)
        else do
          -- Check if user is a member of the conversation
          let convId = qUnqualified meeting.conversationId
          maybeMember <- ConvStore.getLocalMember convId (tUnqualified zUser)
          case maybeMember of
            Just _ -> pure (Just meeting) -- User is a member, authorized
            Nothing -> pure Nothing -- User is not a member, not authorized

listMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  Local UserId ->
  Sem r [Meeting]
listMeetingsImpl zUser = do
  -- List all meetings created by the user
  createdMeetings <- Store.listMeetingsByUser (tUnqualified zUser)

  -- Filter meetings to include only those where user is authorized
  -- (creator or conversation member)
  filterM (isAuthorized zUser) createdMeetings
  where
    isAuthorized :: (Member ConvStore.ConversationStore r) => Local UserId -> Meeting -> Sem r Bool
    isAuthorized lUser meeting = do
      -- User is authorized if they are the creator
      let isCreator = meeting.creator == tUntagged lUser
      if isCreator
        then pure True
        else do
          -- Or if they are a member of the associated conversation
          let convId = qUnqualified meeting.conversationId
          maybeMember <- ConvStore.getLocalMember convId (tUnqualified lUser)
          pure $ isJust maybeMember

updateMeetingImpl ::
  (Member Store.MeetingsStore r) =>
  Local UserId ->
  Qualified MeetingId ->
  UpdateMeeting ->
  Sem r (Maybe Meeting)
updateMeetingImpl zUser meetingId update = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure Nothing
    Just meeting ->
      -- Check authorization (only creator can update)
      if meeting.creator /= tUntagged zUser
        then pure Nothing
        else
          -- Update meeting
          Store.updateMeeting
            meetingId
            update.title
            update.startDate
            update.endDate
            update.recurrence

deleteMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  Sem r Bool
deleteMeetingImpl zUser meetingId = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting ->
      -- Check authorization (only creator can delete)
      if meeting.creator /= tUntagged zUser
        then pure False
        else do
          -- Delete meeting
          Store.deleteMeeting meetingId

          -- Delete associated conversation if it's a meeting conversation
          let convId = qUnqualified meeting.conversationId
          maybeConv <- ConvStore.getConversation convId
          case maybeConv of
            Just conv
              | conv.metadata.cnvmGroupConvType == Just MeetingConversation ->
                  ConvStore.deleteConversation convId
            _ -> pure ()

          pure True

addInvitedEmailsImpl ::
  (Member Store.MeetingsStore r) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  Sem r Bool
addInvitedEmailsImpl zUser meetingId emails = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting ->
      -- Check authorization (only creator can add invitations)
      if meeting.creator /= tUntagged zUser
        then pure False
        else do
          -- Add invited email
          Store.addInvitedEmails meetingId emails
          pure True

removeInvitedEmailsImpl ::
  (Member Store.MeetingsStore r) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  Sem r Bool
removeInvitedEmailsImpl zUser meetingId emails = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting ->
      -- Check authorization (only creator can remove invitations)
      if meeting.creator /= tUntagged zUser
        then pure False
        else do
          -- Remove invited email
          Store.removeInvitedEmails meetingId emails
          pure True

cleanupOldMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  UTCTime ->
  Int ->
  Sem r Int64
cleanupOldMeetingsImpl cutoffTime batchSize = do
  -- 1. Fetch old meetings
  oldMeetings <- Store.getOldMeetings cutoffTime batchSize

  if null oldMeetings
    then pure 0
    else do
      -- 2. Extract meeting IDs and conversation IDs
      let meetingIds = map (.id) oldMeetings
          convIds = map (.conversationId) oldMeetings

      -- 3. Delete meetings from database
      deletedCount <- Store.deleteMeetingBatch meetingIds

      -- 4. Delete associated conversations if they are meeting conversations
      -- We need to check if conversation has GroupConvType = MeetingConversation
      for_ convIds $ \qConvId -> do
        let convId = qUnqualified qConvId
        maybeConv <- ConvStore.getConversation convId
        case maybeConv of
          Just conv
            | conv.metadata.cnvmGroupConvType == Just MeetingConversation ->
                ConvStore.deleteConversation convId
          _ -> pure ()

      pure deletedCount
