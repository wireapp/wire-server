module Wire.API.Routes.Public.Galley.TeamNotification where

import Servant
import Wire.API.Routes.Public
import Wire.API.Routes.Named
import Imports
import Wire.API.Notification
import Data.Range
import Wire.API.Error
import Wire.API.Error.Galley

type TeamNotificationAPI =
  Named
    "get-team-notifications"
    ( "team"
        :> "notifications"
        :> ZUser
        :> CanThrow 'TeamNotFound
        :> CanThrow 'InvalidTeamNotificationId
        -- :> CanThrow InvalidInput
        -- TODO: Ensure/check this is a V1 UUID
        :> QueryParam "since" NotificationId
        :> QueryParam "size" (Range 1 10000 Int32)
        :> Get '[Servant.JSON] QueuedNotificationList
    )
