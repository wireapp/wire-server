-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Id (MeetingId)
import Servant
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Meeting
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type MeetingsAPI =
  Named
    "create-meeting@v17"
    ( Summary "Create a new meeting"
        :> From 'V15
        :> Until 'V18
        :> ZLocalUser
        :> "meetings"
        :> ReqBody '[JSON] NewMeeting
        :> CanThrow 'InvalidOperation
        :> CanThrow UnreachableBackends
        :> MultiVerb
             'POST
             '[JSON]
             '[Respond 201 "Meeting created" MeetingWithConversationLegacy]
             MeetingWithConversationLegacy
    )
    :<|> Named
           "create-meeting"
           ( Summary "Create a new meeting"
               :> From 'V18
               :> ZLocalUser
               :> "meetings"
               :> ReqBody '[JSON] NewMeeting
               :> CanThrow 'InvalidOperation
               :> CanThrow UnreachableBackends
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[Respond 201 "Meeting created" MeetingWithConversation]
                    MeetingWithConversation
           )
    :<|> Named
           "update-meeting@v17"
           ( Summary "Update an existing meeting"
               :> From 'V15
               :> Until 'V18
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
                    '[Respond 200 "Meeting updated" MeetingWithConversationLegacy]
                    MeetingWithConversationLegacy
           )
    :<|> Named
           "update-meeting"
           ( Summary "Update an existing meeting"
               :> From 'V18
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
                    '[Respond 200 "Meeting updated" MeetingWithConversation]
                    MeetingWithConversation
           )
    :<|> Named
           "delete-meeting"
           ( Summary "Delete a meeting"
               :> From 'V15
               :> ZLocalUser
               :> ZConn
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
           "get-meeting@v17"
           ( Summary "Get a single meeting by ID"
               :> From 'V15
               :> Until 'V18
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> Get '[JSON] MeetingLegacy
           )
    :<|> Named
           "get-meeting"
           ( Summary "Get a single meeting by ID"
               :> From 'V18
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> Get '[JSON] Meeting
           )
    :<|> Named
           "list-meetings@v17"
           ( Summary "List all meetings for the authenticated user"
               :> From 'V16
               :> Until 'V18
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> Get '[JSON] [MeetingLegacy]
           )
    :<|> Named
           "list-meetings"
           ( Summary "List all meetings for the authenticated user"
               :> From 'V18
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> Get '[JSON] [Meeting]
           )
    :<|> Named
           "add-meeting-invitation"
           ( Summary "Add an email to the invited emails"
               :> From 'V16
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
               :> From 'V16
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
    :<|> Named
           "replace-meeting-invitation"
           ( Summary "Replace the invited emails"
               :> From 'V17
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> ReqBody '[JSON] MeetingEmailsInvitation
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Invitations replaced"]
                    ()
           )
