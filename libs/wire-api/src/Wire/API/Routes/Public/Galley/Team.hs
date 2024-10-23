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

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.OpenApi.Schema qualified as S
import Data.Range
import Data.Schema
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
import Wire.API.Team.Member
import Wire.API.Team.Permission

-- | FUTUREWORK: remove when the create-non-binding-team endpoint is deleted
data NonBindingNewTeam = NonBindingNewTeam
  { teamName :: Range 1 256 Text,
    teamIcon :: Icon,
    teamIconKey :: Maybe (Range 1 256 Text),
    teamMembers :: Maybe (Range 1 127 [TeamMember])
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NonBindingNewTeam)

instance ToSchema NonBindingNewTeam where
  schema =
    object "NonBindingNewTeam" $
      NonBindingNewTeam
        <$> (.teamName) .= fieldWithDocModifier "name" (description ?~ "team name") schema
        <*> (.teamIcon) .= fieldWithDocModifier "icon" (description ?~ "team icon (asset ID)") schema
        <*> (.teamIconKey) .= maybe_ (optFieldWithDocModifier "icon_key" (description ?~ "team icon asset key") schema)
        <*> (.teamMembers)
          .= maybe_
            ( optFieldWithDocModifier
                "members"
                (description ?~ "initial team member ids (between 1 and 127)")
                sch
            )
    where
      sch :: ValueSchema SwaggerDoc (Range 1 127 [TeamMember])
      sch = fromRange .= rangedSchema (array schema)

type TeamAPI =
  Named
    "create-non-binding-team"
    ( Summary "Create a new non binding team"
        :> Until 'V4
        :> ZUser
        :> ZConn
        :> CanThrow InvalidAction
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
