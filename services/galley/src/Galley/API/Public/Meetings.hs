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

module Galley.API.Public.Meetings where

import Galley.API.Meetings qualified as Meetings
import Galley.App
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Meetings

meetingsAPI :: API MeetingsAPI GalleyEffects
meetingsAPI =
  mkNamedAPI @"create-meeting@v15" Meetings.createMeetingV16
    <@> mkNamedAPI @"create-meeting@v17" Meetings.createMeetingV18
    <@> mkNamedAPI @"create-meeting" Meetings.createMeeting
    <@> mkNamedAPI @"update-meeting@v15" Meetings.updateMeetingV16
    <@> mkNamedAPI @"update-meeting@v17" Meetings.updateMeetingV18
    <@> mkNamedAPI @"update-meeting" Meetings.updateMeeting
    <@> mkNamedAPI @"delete-meeting" Meetings.deleteMeeting
    <@> mkNamedAPI @"get-meeting@v15" Meetings.getMeetingV16
    <@> mkNamedAPI @"get-meeting@v17" Meetings.getMeetingV18
    <@> mkNamedAPI @"get-meeting" Meetings.getMeeting
    <@> mkNamedAPI @"list-meetings@v16" Meetings.listMeetingsV16
    <@> mkNamedAPI @"list-meetings@v17" Meetings.listMeetingsV18
    <@> mkNamedAPI @"list-meetings" Meetings.listMeetings
    <@> mkNamedAPI @"add-meeting-invitation" Meetings.addMeetingInvitation
    <@> mkNamedAPI @"remove-meeting-invitation" Meetings.removeMeetingInvitation
    <@> mkNamedAPI @"replace-meeting-invitation" Meetings.replaceMeetingInvitation
    <@> mkNamedAPI @"refresh-meeting-link" Meetings.refreshMeetingLink
    <@> mkNamedAPI @"get-meeting-by-link" Meetings.getMeetingByLink
    <@> mkNamedAPI @"join-meeting" Meetings.joinMeeting
