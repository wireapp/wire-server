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
