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

module Wire.TeamSubsystem.Util where

import Control.Lens ((^..))
import Data.Default
import Data.Id
import Data.Json.Util
import Imports
import Polysemy
import Wire.API.Event.Team
import Wire.API.Push.V2 (RecipientClients (..))
import Wire.API.Team.Member qualified as TM
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.TeamSubsystem

-- | Generate a team event and send it to all team admins
generateTeamEvent ::
  ( Member Now r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  TeamId ->
  EventData ->
  Sem r ()
generateTeamEvent uid tid edata = do
  now <- get
  let event = newEvent tid now edata
  admins <- internalGetTeamAdmins tid
  pushNotifications
    [ def
        { origin = Just uid,
          json = toJSONObject $ event,
          recipients =
            [ Recipient
                { recipientUserId = u,
                  recipientClients = RecipientClientsAll
                }
              | u <- admins ^.. TM.teamMembers . traverse . TM.userId
            ],
          transient = False
        }
    ]
