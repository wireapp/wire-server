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

module Wire.MeetingsSubsystem.Interpreter
  ( interpretMeetingsSubsystem,
    MeetingError (..),
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Default (def)
import Data.Domain (Domain)
import Data.Id
import Data.Qualified (Local, Qualified (..), tDomain, tUnqualified)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Meeting qualified as API
import Wire.API.Team.Feature (FeatureStatus (..), LockableFeature (..), MeetingsPremiumConfig)
import Wire.API.User (BaseProtocolTag (BaseProtocolMLSTag))
import Wire.ConversationSubsystem (ConversationSubsystem)
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getFeatureForTeam)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

data MeetingError = InvalidTimes | EmptyUpdate
  deriving stock (Eq, Show)

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member Now r,
    Member (Error MeetingError) r
  ) =>
  NominalDiffTime ->
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem validityPeriod = interpret $ \case
  CreateMeeting zUser newMeeting ->
    createMeetingImpl zUser newMeeting
  UpdateMeeting zUser meetingId update ->
    updateMeetingImpl zUser meetingId update validityPeriod
  DeleteMeeting zUser meetingId ->
    deleteMeetingImpl zUser meetingId validityPeriod
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId validityPeriod

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r
  ) =>
  Local UserId ->
  API.NewMeeting ->
  Sem r (API.Meeting, StoredConversation)
createMeetingImpl zUser newMeeting = do
  -- Validate that endTime > startTime
  when (newMeeting.endTime <= newMeeting.startTime) $
    throw InvalidTimes

  -- Determine trial status based on team membership and premium feature
  conversationTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  trial <- case conversationTeamId of
    Nothing -> pure True -- Personal users create trial meetings
    Just teamId -> do
      premiumFeature <- getFeatureForTeam @_ @MeetingsPremiumConfig teamId
      pure $ premiumFeature.status /= FeatureStatusEnabled

  -- Create conversation with the meeting creator as the only member (admin role)
  let newConv =
        NewConv
          { newConvUsers = [],
            newConvQualifiedUsers = [],
            newConvName = Just newMeeting.title,
            newConvAccess = Set.singleton PrivateAccess,
            newConvAccessRoles = Nothing,
            newConvTeam = ConvTeamInfo <$> conversationTeamId,
            newConvMessageTimer = Nothing,
            newConvReceiptMode = Nothing,
            newConvUsersRole = roleNameWireAdmin,
            newConvProtocol = BaseProtocolMLSTag,
            newConvGroupConvType = MeetingConversation,
            newConvCells = True,
            newConvChannelAddPermission = Nothing,
            newConvSkipCreator = False,
            newConvParent = Nothing,
            newConvHistory = def
          }

  -- Create and store the conversation via ConversationSubsystem
  storedConv <- ConversationSubsystem.createGroupConversation zUser Nothing newConv

  -- Store meeting (trial status is provided by caller)
  storedMeeting <-
    Store.createMeeting
      newMeeting.title
      (tUnqualified zUser)
      newMeeting.startTime
      newMeeting.endTime
      newMeeting.recurrence
      storedConv.id_
      newMeeting.invitedEmails
      trial

  -- Return created meeting
  pure
    ( storedMeetingToMeeting (tDomain zUser) storedMeeting,
      storedConv
    )

updateMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  API.UpdateMeeting ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
updateMeetingImpl zUser meetingId update validityPeriod = do
  when (isNothing update.title && isNothing update.startTime && isNothing update.endTime && isNothing update.recurrence) $
    throw EmptyUpdate

  runMaybeT $ do
    meeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
    now <- lift Now.get
    let cutoff = addUTCTime (negate validityPeriod) now
    guard $ meeting.endTime >= cutoff
    when (fromMaybe meeting.startTime update.startTime >= fromMaybe meeting.endTime update.endTime) $
      lift $
        throw InvalidTimes

    guard $ meeting.creator == tUnqualified zUser
    updatedMeeting <-
      MaybeT $
        Store.updateMeeting
          (qUnqualified meetingId)
          update.title
          update.startTime
          update.endTime
          update.recurrence
    pure $ storedMeetingToMeeting (tDomain zUser) updatedMeeting

deleteMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r Bool
deleteMeetingImpl zUser meetingId validityPeriod = do
  -- Get existing meeting
  result <-
    runMaybeT $ do
      meeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ meeting.endTime >= cutoff
      -- Check authorization (only creator can delete)
      guard $ meeting.creator == tUnqualified zUser
      -- Delete meeting
      lift $ Store.deleteMeeting (qUnqualified meetingId)
      -- Delete associated conversation if it's a meeting conversation
      let convId = meeting.conversationId
      maybeConv <- lift $ ConversationSubsystem.getConversation convId
      case maybeConv of
        Just conv
          | conv.metadata.cnvmGroupConvType == Just MeetingConversation ->
              lift $ ConversationSubsystem.deleteConversation convId
        _ -> pure ()
      pure ()
  pure $ isJust result

getMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
getMeetingImpl zUser meetingId validityPeriod = do
  -- Get meeting from store
  runMaybeT $ do
    storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
    now <- lift Now.get
    let cutoff = addUTCTime (negate validityPeriod) now
    guard $ storedMeeting.endTime >= cutoff
    -- Check authorization: user must be creator OR member of the associated conversation
    let isCreator = storedMeeting.creator == tUnqualified zUser
    if isCreator
      then pure $ storedMeetingToMeeting (tDomain zUser) storedMeeting
      else do
        -- Check if user is a member of the conversation
        let convId = storedMeeting.conversationId
        void $ MaybeT $ ConversationSubsystem.internalGetLocalMember convId (tUnqualified zUser)
        pure $ storedMeetingToMeeting (tDomain zUser) storedMeeting -- User is a member, authorized

-- Helper function to convert StoredMeeting to API.Meeting
storedMeetingToMeeting :: Domain -> Store.StoredMeeting -> API.Meeting
storedMeetingToMeeting domain sm =
  API.Meeting
    { API.id = Qualified sm.id domain,
      API.title = sm.title,
      API.creator = Qualified sm.creator domain,
      API.startTime = sm.startTime,
      API.endTime = sm.endTime,
      API.recurrence = sm.recurrence,
      API.conversationId = Qualified sm.conversationId domain,
      API.invitedEmails = sm.invitedEmails,
      API.trial = sm.trial,
      API.createdAt = sm.createdAt,
      API.updatedAt = sm.updatedAt
    }
