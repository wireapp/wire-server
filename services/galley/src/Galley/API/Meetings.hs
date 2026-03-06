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
    updateMeeting,
    getMeeting,
  )
where

import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Meeting
import Wire.API.Team.Feature (FeatureStatus (..), LockableFeature (..), MeetingsConfig)
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getFeatureForTeam)
import Wire.MeetingsSubsystem qualified as Meetings
import Wire.TeamStore qualified as TeamStore

-- | Check if meetings feature is enabled for the user (if they're in a team)
checkMeetingsEnabled ::
  ( Member TeamStore.TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member (ErrorS 'InvalidOperation) r
  ) =>
  UserId ->
  Sem r ()
checkMeetingsEnabled userId = do
  maybeTeamId <- TeamStore.getOneUserTeam userId
  case maybeTeamId of
    Nothing -> pure () -- Personal users can use meetings
    Just teamId -> do
      meetingFeature <- getFeatureForTeam @_ @MeetingsConfig teamId
      unless (meetingFeature.status == FeatureStatusEnabled) $
        throwS @'InvalidOperation

createMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member FeaturesConfigSubsystem r
  ) =>
  Local UserId ->
  NewMeeting ->
  Sem r Meeting
createMeeting lUser newMeeting = do
  checkMeetingsEnabled (tUnqualified lUser)
  (meeting, _conversation) <- Meetings.createMeeting lUser newMeeting
  pure meeting

updateMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member TeamStore.TeamStore r,
    Member FeaturesConfigSubsystem r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  UpdateMeeting ->
  Sem r Meeting
updateMeeting zUser domain meetingId update = do
  checkMeetingsEnabled (tUnqualified zUser)
  let qMeetingId = Qualified meetingId domain
  maybeMeeting <- Meetings.updateMeeting zUser qMeetingId update
  case maybeMeeting of
    Nothing -> throwS @'MeetingNotFound
    Just meeting -> pure meeting

getMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r,
    Member TeamStore.TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member (ErrorS 'InvalidOperation) r
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
