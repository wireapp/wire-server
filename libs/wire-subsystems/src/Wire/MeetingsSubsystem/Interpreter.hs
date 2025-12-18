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
import Data.Map qualified as Map
import Data.Qualified
import Data.Range (Range, unsafeRange)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.CellsState (CellsState (CellsDisabled))
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Error (ErrorS)
import Wire.API.Error hiding (DynError, ErrorS)
import Wire.API.Error.Galley
import Wire.API.Meeting qualified as API
import Wire.API.User (BaseProtocolTag (BaseProtocolMLSTag))
import Wire.API.User.Identity (EmailAddress)
import Wire.ConversationStore qualified as ConvStore
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.Sem.Paging.Cassandra (ResultSet (..), ResultSetType (..))
import Wire.StoredConversation
import Wire.TeamStore qualified as TeamStore
import Wire.UserList

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r,
    Member TeamStore.TeamStore r,
    Member (Embed IO) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'InvalidOperation) r
  ) =>
  NominalDiffTime ->
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem validityPeriod = interpret $ \case
  CreateMeeting zUser newMeeting premium ->
    createMeetingImpl zUser newMeeting premium
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId validityPeriod
  ListMeetings zUser ->
    listMeetingsImpl zUser validityPeriod
  UpdateMeeting zUser meetingId update ->
    updateMeetingImpl zUser meetingId update validityPeriod
  DeleteMeeting zUser meetingId ->
    deleteMeetingImpl zUser meetingId validityPeriod
  AddInvitedEmails zUser meetingId emails ->
    addInvitedEmailsImpl zUser meetingId emails validityPeriod
  RemoveInvitedEmails zUser meetingId emails ->
    removeInvitedEmailsImpl zUser meetingId emails validityPeriod

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r,
    Member TeamStore.TeamStore r,
    Member (Embed IO) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'InvalidOperation) r
  ) =>
  Local UserId ->
  API.NewMeeting ->
  Bool ->
  Sem r (API.Meeting, StoredConversation)
createMeetingImpl zUser newMeeting premium = do
  -- Validate that endDate > startDate
  when (newMeeting.endDate <= newMeeting.startDate) $
    throwS @'InvalidOperation

  -- Determine trial status based on team membership and premium feature
  maybeTeamId <- TeamStore.getOneUserTeam (tUnqualified zUser)
  trial <- case maybeTeamId of
    Nothing -> pure True -- Personal users create trial meetings
    Just teamId -> do
      -- Verify user is a team member (not just a collaborator)
      maybeMember <- TeamStore.getTeamMember teamId (tUnqualified zUser)
      case maybeMember of
        Nothing -> throwS @'NotATeamMember -- User not a member
        Just _member -> pure $ not premium

  -- Generate meeting ID
  meetingId <- randomId
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
            cnvmAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
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
    (tUnqualified zUser)
    newMeeting.title
    newMeeting.startDate
    newMeeting.endDate
    newMeeting.recurrence
    storedConv.id_
    newMeeting.invitedEmails
    trial

  now <- liftIO getCurrentTime
  -- Return created meeting
  pure
    ( API.Meeting
        { id = qMeetingId,
          title = newMeeting.title,
          creator = tUntagged zUser,
          startDate = newMeeting.startDate,
          endDate = newMeeting.endDate,
          recurrence = newMeeting.recurrence,
          conversationId = qConvId,
          invitedEmails = newMeeting.invitedEmails,
          trial = trial,
          updatedAt = now
        },
      storedConv
    )

getMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
getMeetingImpl zUser meetingId validityPeriod = do
  -- Get meeting from store
  maybeMeeting <- Store.getMeeting meetingId

  case maybeMeeting of
    Nothing -> pure Nothing
    Just meeting -> do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate validityPeriod) now
      if meeting.endDate < cutoff
        then pure Nothing
        else do
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
    Member ConvStore.ConversationStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  NominalDiffTime ->
  Sem r [API.Meeting]
listMeetingsImpl zUser validityPeriod = do
  now <- liftIO getCurrentTime
  let cutoff = addUTCTime (negate validityPeriod) now

  -- List all meetings created by the user
  createdMeetings <- Store.listMeetingsByUser (tUnqualified zUser) cutoff

  -- Loop over local conversations accessible by the user, then filter to only keep meetings.
  memberMeetings <- getAllMemberMeetings zUser cutoff

  -- Combine and deduplicate
  let allMeetings = createdMeetings <> memberMeetings
      uniqueMeetings = Map.elems $ Map.fromList [(m.id, m) | m <- allMeetings]

  pure uniqueMeetings

getAllMemberMeetings ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  Local UserId ->
  UTCTime ->
  Sem r [API.Meeting]
getAllMemberMeetings zUser cutoff = do
  -- We process conversations in pages
  processPage Nothing
  where
    processPage ::
      ( Member Store.MeetingsStore r,
        Member ConvStore.ConversationStore r
      ) =>
      Maybe (Qualified ConvId) ->
      Sem r [API.Meeting]
    processPage lastId = do
      let range = unsafeRange 1000 :: Range 1 1000 Int32
      resultSet <- ConvStore.getConversationIdsResultSet zUser range lastId

      let qConvIds = resultSet.resultSetResult
          uConvIds = map qUnqualified qConvIds

      if null uConvIds
        then pure []
        else do
          convs <- ConvStore.getConversations uConvIds

          let meetingConvs = filter isMeetingConv convs
              meetingConvIds = Set.fromList $ map (.id_) meetingConvs

          -- Identify which Qualified ConvIds correspond to meeting conversations
          -- We use the original Qualified IDs to query the meeting store
          let targetQConvIds = filter (\qId -> qUnqualified qId `Set.member` meetingConvIds) qConvIds

          -- Fetch meetings for these conversations
          pageMeetings <- forM targetQConvIds $ \qConvId -> do
            Store.listMeetingsByConversation qConvId cutoff

          let currentMeetings = concat pageMeetings

          -- Check if there are more pages
          case resultSet.resultSetType of
            ResultSetTruncated -> do
              -- Recurse with last ID
              rest <- processPage (Just (last qConvIds))
              pure (currentMeetings <> rest)
            ResultSetComplete -> pure currentMeetings

    isMeetingConv :: StoredConversation -> Bool
    isMeetingConv conv = conv.metadata.cnvmGroupConvType == Just MeetingConversation

updateMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  API.UpdateMeeting ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
updateMeetingImpl zUser meetingId update validityPeriod = do
  when (isNothing update.title && isNothing update.startDate && isNothing update.endDate && isNothing update.recurrence) $
    throwS @'InvalidOperation

  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure Nothing
    Just meeting -> do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate validityPeriod) now
      if meeting.endDate < cutoff
        then pure Nothing
        else do
          when (fromMaybe meeting.startDate update.startDate >= fromMaybe meeting.endDate update.endDate) $
            throwS @'InvalidOperation

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
    Member ConvStore.ConversationStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r Bool
deleteMeetingImpl zUser meetingId validityPeriod = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting -> do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate validityPeriod) now
      if meeting.endDate < cutoff
        then pure False
        else
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
  ( Member Store.MeetingsStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  NominalDiffTime ->
  Sem r Bool
addInvitedEmailsImpl zUser meetingId emails validityPeriod = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting -> do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate validityPeriod) now
      if meeting.endDate < cutoff
        then pure False
        else
          -- Check authorization (only creator can add invitations)
          if meeting.creator /= tUntagged zUser
            then pure False
            else do
              -- Add invited email
              Store.addInvitedEmails meetingId emails
              pure True

removeInvitedEmailsImpl ::
  ( Member Store.MeetingsStore r,
    Member (Embed IO) r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  NominalDiffTime ->
  Sem r Bool
removeInvitedEmailsImpl zUser meetingId emails validityPeriod = do
  -- Get existing meeting
  maybeMeeting <- Store.getMeeting meetingId
  case maybeMeeting of
    Nothing -> pure False
    Just meeting -> do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate validityPeriod) now
      if meeting.endDate < cutoff
        then pure False
        else
          -- Check authorization (only creator can remove invitations)
          if meeting.creator /= tUntagged zUser
            then pure False
            else do
              -- Remove invited email
              Store.removeInvitedEmails meetingId emails
              pure True
