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

import Data.Default (def)
import Data.Domain (Domain)
import Data.Id
import Data.Qualified (Local, Qualified (..), tDomain, tUnqualified)
import Data.Range (checked)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Error (ErrorS)
import Wire.API.Error hiding (DynError, ErrorS)
import Wire.API.Error.Galley
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

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member Now r,
    Member (ErrorS 'InvalidOperation) r
  ) =>
  NominalDiffTime ->
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem validityPeriod = interpret $ \case
  CreateMeeting zUser newMeeting ->
    createMeetingImpl zUser newMeeting
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId validityPeriod

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (ErrorS 'InvalidOperation) r
  ) =>
  Local UserId ->
  API.NewMeeting ->
  Sem r (API.Meeting, StoredConversation)
createMeetingImpl zUser newMeeting = do
  -- Validate that endTime > startTime
  when (newMeeting.endTime <= newMeeting.startTime) $
    throwS @'InvalidOperation

  -- Determine trial status based on team membership and premium feature
  conversationTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  trial <- case conversationTeamId of
    Nothing -> pure True -- Personal users create trial meetings
    Just teamId -> do
      premiumFeature <- getFeatureForTeam @_ @MeetingsPremiumConfig teamId
      pure $  premiumFeature.status == FeatureStatusEnabled

  -- Create conversation with the meeting creator as the only member (admin role)
  let nameChecked = checked newMeeting.title
      newConv =
        NewConv
          { newConvUsers = [],
            newConvQualifiedUsers = [],
            newConvName = Just $ fromJust nameChecked,
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
  maybeStoredMeeting <- Store.getMeeting (qUnqualified meetingId)

  case maybeStoredMeeting of
    Nothing -> pure Nothing
    Just storedMeeting -> do
      now <- Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      if storedMeeting.endTime < cutoff
        then pure Nothing
        else do
          -- Check authorization: user must be creator OR member of the associated conversation
          let isCreator = storedMeeting.creator == tUnqualified zUser
          if isCreator
            then pure (Just (storedMeetingToMeeting (tDomain zUser) storedMeeting))
            else do
              -- Check if user is a member of the conversation
              let convId = storedMeeting.conversationId
              maybeMember <- ConversationSubsystem.internalGetLocalMember convId (tUnqualified zUser)
              case maybeMember of
                Just _ -> pure (Just (storedMeetingToMeeting (tDomain zUser) storedMeeting)) -- User is a member, authorized
                Nothing -> pure Nothing -- User is not a member, not authorized

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
