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

module Galley.API.Meetings
  ( createMeeting,
    getMeeting,
    listMeetings,
    updateMeeting,
    deleteMeeting,
    addMeetingInvitation,
    removeMeetingInvitation,
  )
where

import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import Galley.API.Error
import Galley.API.Teams.Features.Get (getFeatureForTeam)
import Galley.Effects.TeamFeatureStore
import Galley.Options (Opts)
import Imports
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation (JoinType (InternalAdd))
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Meeting
import Wire.API.Team.Feature (FeatureStatus (..), LockableFeature (..), MeetingConfig, MeetingPremiumConfig)
import Wire.BackendNotificationQueueAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationSubsystem.Notification (notifyCreatedConversation)
import Wire.ConversationSubsystem.View qualified as ViewError
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.MeetingsSubsystem qualified as Meetings
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.TeamStore qualified as TeamStore

-- | Check if meetings feature is enabled for the user (if they're in a team)
checkMeetingsEnabled ::
  ( Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Input Opts) r
  ) =>
  UserId ->
  Sem r ()
checkMeetingsEnabled userId = do
  maybeTeamId <- TeamStore.getOneUserTeam userId
  case maybeTeamId of
    Nothing -> pure () -- Personal users can use meetings
    Just teamId -> do
      meetingFeature <- getFeatureForTeam @MeetingConfig teamId
      unless (meetingFeature.status == FeatureStatusEnabled) $
        throwS @'InvalidOperation

createMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'InvalidOperation) r,
    Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member Now r,
    Member P.TinyLog r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  NewMeeting ->
  Sem r Meeting
createMeeting lUser newMeeting = do
  -- Check if meetings feature is enabled
  checkMeetingsEnabled (tUnqualified lUser)

  maybeTeamId <- TeamStore.getOneUserTeam (tUnqualified lUser)
  premium <- case maybeTeamId of
    Nothing -> pure True -- Personal users create trial meetings
    Just teamId -> do
      premiumFeature <- getFeatureForTeam @MeetingPremiumConfig teamId
      pure $ case premiumFeature of
        LockableFeature {status = FeatureStatusEnabled} -> True
        _ -> False

  (meeting, conversation) <- Meetings.createMeeting lUser newMeeting premium
  res <- runError @ViewError.ViewError $ notifyCreatedConversation lUser Nothing conversation InternalAdd
  case res of
    Left ViewError.BadMemberState -> throw (InternalErrorWithDescription "Internal error: Member state inconsistent")
    Right () -> pure ()

  pure meeting

getMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  Sem r Meeting
getMeeting zUser domain meetingId = do
  checkMeetingsEnabled (tUnqualified zUser)
  let qMeetingId = Qualified meetingId domain
  maybeMeeting <- Meetings.getMeeting zUser qMeetingId
  case maybeMeeting of
    Nothing -> throwS @'MeetingNotFound
    Just meeting -> pure meeting

listMeetings ::
  ( Member Meetings.MeetingsSubsystem r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Sem r [Meeting]
listMeetings lUser = do
  checkMeetingsEnabled (tUnqualified lUser)
  Meetings.listMeetings lUser

updateMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  UpdateMeeting ->
  Sem r Meeting
updateMeeting zUser domain meetingId update = do
  checkMeetingsEnabled (tUnqualified zUser)
  -- Validate that at least one field is being updated
  when (isNothing update.title && isNothing update.startDate && isNothing update.endDate && isNothing update.recurrence) $
    throwS @'InvalidOperation
  -- Validate dates if both are provided
  case (update.startDate, update.endDate) of
    (Just start, Just end) -> when (end <= start) $ throwS @'InvalidOperation
    _ -> pure ()
  let qMeetingId = Qualified meetingId domain
  maybeMeeting <- Meetings.updateMeeting zUser qMeetingId update
  case maybeMeeting of
    Nothing -> throwS @'MeetingNotFound
    Just meeting -> pure meeting

deleteMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  Sem r ()
deleteMeeting zUser domain meetingId = do
  checkMeetingsEnabled (tUnqualified zUser)
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.deleteMeeting zUser qMeetingId
  unless success $ throwS @'MeetingNotFound

addMeetingInvitation ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  MeetingEmailsInvitation ->
  Sem r ()
addMeetingInvitation zUser domain meetingId (MeetingEmailsInvitation emails) = do
  checkMeetingsEnabled (tUnqualified zUser)
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.addInvitedEmails zUser qMeetingId emails
  unless success $ throwS @'MeetingNotFound

removeMeetingInvitation ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  MeetingEmailsInvitation ->
  Sem r ()
removeMeetingInvitation zUser domain meetingId (MeetingEmailsInvitation emails) = do
  checkMeetingsEnabled (tUnqualified zUser)
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.removeInvitedEmails zUser qMeetingId emails
  unless success $ throwS @'MeetingNotFound
