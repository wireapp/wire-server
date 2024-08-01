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

module Wire.API.Routes.Public.Galley.Team where

import Data.Id
import Imports
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.Team
import Wire.API.Team.Permission

type TeamAPI =
  Named
    "create-non-binding-team"
    ( Summary "Create a new non binding team"
        :> Until 'V4
        :> ZUser
        :> ZConn
        :> CanThrow 'NotConnected
        :> CanThrow 'UserBindingExists
        :> "teams"
        :> ReqBody '[Servant.JSON] NonBindingNewTeam
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  '[DescHeader "Location" "Team ID" TeamId]
                  TeamId
                  (RespondEmpty 201 "Team ID as `Location` header value")
              ]
             TeamId
    )
    :<|> Named
           "update-team"
           ( Summary "Update team properties"
               :> ZUser
               :> ZConn
               :> CanThrow 'NotATeamMember
               :> CanThrow ('MissingPermission ('Just 'SetTeamData))
               :> "teams"
               :> Capture "tid" TeamId
               :> ReqBody '[JSON] TeamUpdateData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Team updated"]
                    ()
           )
    :<|> Named
           "get-teams"
           ( Summary "Get teams (deprecated); use `GET /teams/:tid`"
               :> Until 'V4
               :> ZUser
               :> "teams"
               :> Get '[JSON] TeamList
           )
    :<|> Named
           "get-team"
           ( Summary "Get a team by ID"
               :> ZUser
               :> CanThrow 'TeamNotFound
               :> "teams"
               :> Capture "tid" TeamId
               :> Get '[JSON] Team
           )
    :<|> Named
           "delete-team"
           ( Summary "Delete a team"
               :> ZUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow ('MissingPermission ('Just 'DeleteTeam))
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'DeleteQueueFull
               :> CanThrow AuthenticationError
               :> "teams"
               :> Capture "tid" TeamId
               :> ReqBody '[Servant.JSON] TeamDeleteData
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 202 "Team is scheduled for removal"] ()
           )
