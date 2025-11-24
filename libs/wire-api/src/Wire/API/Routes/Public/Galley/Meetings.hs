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

module Wire.API.Routes.Public.Galley.Meetings where

import Data.Domain (Domain)
import Servant
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Meeting
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public

type MeetingsAPI =
  Named
    "create-meeting"
    ( Summary "Create a new meeting"
        :> ZLocalUser
        :> "meetings"
        :> ReqBody '[JSON] NewMeeting
        :> CanThrow 'InvalidOperation
        :> CanThrow UnreachableBackends
        :> MultiVerb
             'POST
             '[JSON]
             '[Respond 201 "Meeting created" Meeting]
             Meeting
    )
    :<|> Named
           "list-meetings"
           ( Summary "List all meetings for the authenticated user"
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> Get '[JSON] [Meeting]
           )
    :<|> Named
           "get-meeting"
           ( Summary "Get a single meeting by ID"
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> Get '[JSON] Meeting
           )
    :<|> Named
           "update-meeting"
           ( Summary "Update an existing meeting"
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidOperation
               :> ReqBody '[JSON] UpdateMeeting
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[Respond 200 "Meeting updated" Meeting]
                    Meeting
           )
    :<|> Named
           "delete-meeting"
           ( Summary "Delete a meeting"
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Meeting deleted"]
                    ()
           )
    :<|> Named
           "add-meeting-invitation"
           ( Summary "Add an email to the invited emails"
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> ReqBody '[JSON] MeetingEmailsInvitation
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[RespondEmpty 200 "Invitation added"]
                    ()
           )
    :<|> Named
           "remove-meeting-invitation"
           ( Summary "Remove emails from the invited emails"
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> "delete"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> ReqBody '[JSON] MeetingEmailsInvitation
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[RespondEmpty 200 "Invitations removed"]
                    ()
           )
