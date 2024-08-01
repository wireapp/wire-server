-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley.TeamMember where

import Data.Id
import Data.Int
import Data.Range
import GHC.Generics
import Generics.SOP qualified as GSOP
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.CSV
import Wire.API.Routes.LowLevelStream
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.Team.Member
import Wire.API.User qualified as User

type TeamMemberAPI =
  Named
    "get-team-members"
    ( Summary "Get team members"
        :> CanThrow 'NotATeamMember
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "members"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum results to be returned"
             ]
             "maxResults"
             (Range 1 HardTruncationLimit Int32)
        :> QueryParam'
             [ Optional,
               Strict,
               Description
                 "Optional, when not specified, the first page will be returned.\
                 \Every returned page contains a `pagingState`, this should be supplied to retrieve the next page."
             ]
             "pagingState"
             TeamMembersPagingState
        :> Get '[JSON] TeamMembersPage
    )
    :<|> Named
           "get-team-member"
           ( Summary "Get single team member"
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TeamMemberNotFound
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> Get '[JSON] TeamMemberOptPerms
           )
    :<|> Named
           "get-team-members-by-ids"
           ( Summary "Get team members by user id list"
               :> Description "The `has_more` field in the response body is always `false`."
               :> CanThrow 'NotATeamMember
               :> CanThrow 'BulkGetMemberLimitExceeded
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "get-members-by-ids-using-post"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum results to be returned"
                    ]
                    "maxResults"
                    (Range 1 HardTruncationLimit Int32)
               :> ReqBody '[JSON] User.UserIdList
               :> Post '[JSON] TeamMemberListOptPerms
           )
    :<|> Named
           "add-team-member"
           ( Summary "Add a new team member"
               :> Until 'V4
               :> CanThrow 'InvalidPermissions
               :> CanThrow 'NoAddToBinding
               :> CanThrow 'NotATeamMember
               :> CanThrow 'NotConnected
               :> CanThrow OperationDenied
               :> CanThrow 'TeamNotFound
               :> CanThrow 'TooManyTeamMembers
               :> CanThrow 'TooManyTeamAdmins
               :> CanThrow 'UserBindingExists
               :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> ReqBody '[JSON] NewTeamMember
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (RespondEmpty 200 "")
           )
    :<|> Named
           "delete-team-member"
           ( Summary "Remove an existing team member"
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'TeamNotFound
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> ReqBody '[JSON] TeamMemberDeleteData
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    TeamMemberDeleteResultResponseType
                    TeamMemberDeleteResult
           )
    :<|> Named
           "delete-non-binding-team-member"
           ( Summary "Remove an existing team member"
               :> Until 'V4
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'TeamNotFound
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> Capture "uid" UserId
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    TeamMemberDeleteResultResponseType
                    TeamMemberDeleteResult
           )
    :<|> Named
           "update-team-member"
           ( Summary "Update an existing team member"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidPermissions
               :> CanThrow 'TeamNotFound
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'TooManyTeamAdmins
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> ReqBody '[JSON] NewTeamMember
               :> MultiVerb1
                    'PUT
                    '[JSON]
                    (RespondEmpty 200 "")
           )
    :<|> Named
           "get-team-members-csv"
           ( Summary "Get all members of the team as a CSV file"
               :> CanThrow 'AccessDenied
               :> Description
                    "The endpoint returns data in chunked transfer encoding.\
                    \ Internal server errors might result in a failed transfer\
                    \ instead of a 500 response."
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "members"
               :> "csv"
               :> LowLevelStream
                    'GET
                    200
                    '[ '( "Content-Disposition",
                          "attachment; filename=\"wire_team_members.csv\""
                        )
                     ]
                    "CSV of team members"
                    CSV
           )

type TeamMemberDeleteResultResponseType =
  '[ RespondEmpty 202 "Team member scheduled for deletion",
     RespondEmpty 200 ""
   ]

data TeamMemberDeleteResult
  = TeamMemberDeleteAccepted
  | TeamMemberDeleteCompleted
  deriving (Generic)
  deriving (AsUnion TeamMemberDeleteResultResponseType) via GenericAsUnion TeamMemberDeleteResultResponseType TeamMemberDeleteResult

instance GSOP.Generic TeamMemberDeleteResult
