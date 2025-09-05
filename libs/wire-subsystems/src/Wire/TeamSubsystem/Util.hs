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

-- | Generate team events and send them to all team admins
generateTeamEvents ::
  ( Member Now r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  TeamId ->
  [EventData] ->
  Sem r ()
generateTeamEvents uid tid eventsData = do
  now <- get
  admins <- internalGetTeamAdmins tid
  pushNotifications $
    eventsData <&> \eData ->
      def
        { origin = Just uid,
          json = toJSONObject $ newEvent tid now eData,
          recipients =
            [ Recipient
                { recipientUserId = u,
                  recipientClients = RecipientClientsAll
                }
              | u <- admins ^.. TM.teamMembers . traverse . TM.userId
            ],
          transient = False
        }
