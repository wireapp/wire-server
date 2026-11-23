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

import Data.Code qualified as Code
import Data.Domain (Domain)
import Data.Id (MeetingId)
import Data.Misc (IpAddr, PlainTextPassword8)
import Servant
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Meeting
import Wire.API.OAuth
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

-- | Pairing requirements shared by the seven V19 meeting mutation routes.
type CalendarPairingDocs =
  Description
    "Externally-managed meetings (externally_managed_by (e.g. google_calendar or outlook)) must be created and mutated by the calendar integration: an OAuth bearer caller whose OAuth client is listed in the deployment's calendar OAuth clients (galley setting settings.meetings.calendarOAuthClients). The calendar integration may only create and mutate externally-managed meetings. Violations of the externally_managed_by/caller/calendar-field pairing return 400 invalid-op; mutating an externally-managed meeting with a Z-Connection returns 403 externally-managed-by-calendar. Creating an externally-managed meeting returns 403 id-feature-error when the deployment's license does not cover calendar integration."

type MeetingsAPI =
  Named
    "create-meeting@v15"
    ( Summary "Create a new meeting"
        :> From 'V15
        :> Until 'V17
        :> ZLocalUser
        :> ZConn
        :> "meetings"
        :> ReqBody '[JSON] NewMeetingV16
        :> CanThrow MeetingError
        :> CanThrow UnreachableBackends
        :> MultiVerb
             'POST
             '[JSON]
             '[Respond 201 "Meeting created" MeetingWithConversationV16]
             MeetingWithConversationV16
    )
    :<|> Named
           "create-meeting@v17"
           ( Summary "Create a new meeting"
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V17
               :> Until 'V19
               :> ZLocalUser
               :> ZConn
               :> "meetings"
               :> ReqBody '[JSON] NewMeetingV18
               :> CanThrow MeetingError
               :> CanThrow UnreachableBackends
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[Respond 201 "Meeting created" MeetingWithConversationV18]
                    MeetingWithConversationV18
           )
    :<|> Named
           "create-meeting"
           ( Summary "Create a new meeting"
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V19
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> ReqBody '[JSON] NewMeeting
               :> CanThrow MeetingError
               :> CanThrow UnreachableBackends
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[Respond 201 "Meeting created" MeetingWithConversation]
                    MeetingWithConversation
           )
    :<|> Named
           "update-meeting@v15"
           ( Summary "Update an existing meeting"
               :> From 'V15
               :> Until 'V17
               :> ZLocalUser
               :> ZConn
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
               :> ReqBody '[JSON] UpdateMeeting
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[Respond 200 "Meeting updated" MeetingWithConversationV16]
                    MeetingWithConversationV16
           )
    :<|> Named
           "update-meeting@v17"
           ( Summary "Update an existing meeting"
               :> From 'V17
               :> Until 'V19
               :> ZLocalUser
               :> ZConn
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
               :> ReqBody '[JSON] UpdateMeeting
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[Respond 200 "Meeting updated" MeetingWithConversationV18]
                    MeetingWithConversationV18
           )
    :<|> Named
           "update-meeting"
           ( Summary "Update an existing meeting"
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V19
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
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
               :> DescriptionOAuthScope 'DeleteOnlyMeetings
               :> From 'V15
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Meeting deleted"]
                    ()
           )
    :<|> Named
           "get-meeting@v15"
           ( Summary "Get a single meeting by ID"
               :> From 'V15
               :> Until 'V17
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow MeetingError
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "A single meeting by ID" MeetingV16)
           )
    :<|> Named
           "get-meeting@v17"
           ( Summary "Get a single meeting by ID"
               :> From 'V17
               :> Until 'V19
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow MeetingError
               :> Get '[JSON] MeetingV18
           )
    :<|> Named
           "get-meeting"
           ( Summary "Get a single meeting by ID"
               :> Description
                    "Only the creator and members of the meeting's conversation can get a meeting. Returns 404 meeting-not-found for unknown or expired meetings, and for meetings of a team with the meetings feature disabled (for everyone, including the creator). The caller's own team feature status does not matter."
               :> From 'V19
               :> ZLocalUser
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> CanThrow 'MeetingNotFound
               :> CanThrow MeetingError
               :> Get '[JSON] Meeting
           )
    :<|> Named
           "list-meetings@v16"
           ( Summary "List all meetings for the authenticated user"
               :> From 'V16
               :> Until 'V17
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> CanThrow MeetingError
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "List of meetings for the authenticated user" [MeetingV16])
           )
    :<|> Named
           "list-meetings@v17"
           ( Summary "List all meetings for the authenticated user"
               :> From 'V17
               :> Until 'V19
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> CanThrow MeetingError
               :> Get '[JSON] [MeetingV18]
           )
    :<|> Named
           "list-meetings"
           ( Summary "List all meetings for the authenticated user"
               :> From 'V19
               :> ZLocalUser
               :> "meetings"
               :> "list"
               :> CanThrow MeetingError
               :> Get '[JSON] [Meeting]
           )
    :<|> Named
           "add-meeting-invitation"
           ( Summary "Add an email to the invited emails"
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V16
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
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
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V16
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> "delete"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
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
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V17
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "invitations"
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
               :> ReqBody '[JSON] MeetingEmailsInvitation
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Invitations replaced"]
                    ()
           )
    :<|> Named
           "refresh-meeting-link"
           ( Summary "Refresh the join link of a meeting"
               :> DescriptionOAuthScope 'WriteOnlyMeetings
               :> From 'V19
               :> ZLocalUser
               :> ZOptConn
               :> ZOptOAuthClient
               :> CalendarPairingDocs
               :> "meetings"
               :> Capture "domain" Domain
               :> Capture "id" MeetingId
               :> "link"
               :> "refresh"
               :> ReqBody '[JSON] RefreshMeetingLinkRequest
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'AccessDenied
               :> CanThrow MeetingError
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[Respond 200 "Meeting link refreshed" MeetingWithConversation]
                    MeetingWithConversation
           )
    :<|> Named
           "code-check-meeting-link"
           ( Summary "Check the meeting behind a join link"
               :> Description
                    "Unauthenticated: no cookie or token needed. Returns the metadata (title, time, recurrence) of the meeting a join link resolves to. Unlike `GET /meetings/{domain}/{id}` no creator or conversation membership is required. Returns 404 meeting-not-found for unknown code keys, stale code values (e.g. after a link refresh), expired meetings, meetings of a team with the meetings feature disabled, or keys addressing a conversation code; 403 invalid-meeting-password when the link is password-protected and the `password` query param is missing or wrong. Returns 403 id-feature-error when the deployment's license does not cover invitation links."
               :> From 'V19
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'InvalidMeetingPassword
               :> CanThrow 'MeetingNotLicensed
               :> Header' [Required, Strict] "X-Forwarded-For" IpAddr
               :> "meeting"
               :> Capture "domain" Domain
               :> Capture "key" Code.Key
               :> Capture "code" Code.Value
               :> "code-check"
               :> QueryParam' '[Optional, Strict] "password" PlainTextPassword8
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "The meeting behind the join link" MeetingCodeCheck)
           )
    :<|> Named
           "join-meeting"
           ( Summary "Join a meeting through its join link"
               :> Description
                    "Authenticated. Resolves the join link and adds the user to the meeting's underlying conversation (as with `POST /conversations/join` via the link's code access), returning the meeting with its conversation view. Re-joining is an idempotent no-op. A password-protected link requires the matching `password` query param (403 invalid-meeting-password otherwise); a stale (pre-refresh) code value, or a meeting of a team with the meetings feature disabled, yields 404 meeting-not-found. Returns 403 id-feature-error when the deployment's license does not cover invitation links."
               :> From 'V19
               :> ZLocalUser
               :> ZConn
               :> "meeting"
               :> Capture "domain" Domain
               :> Capture "key" Code.Key
               :> Capture "code" Code.Value
               :> "join"
               :> QueryParam' '[Optional, Strict] "password" PlainTextPassword8
               :> CanThrow 'MeetingNotFound
               :> CanThrow 'InvalidMeetingPassword
               :> CanThrow 'MeetingNotLicensed
               :> CanThrow 'LinkUnavailable
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TooManyMembers
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[Respond 200 "Meeting joined" MeetingWithConversation]
                    MeetingWithConversation
           )
