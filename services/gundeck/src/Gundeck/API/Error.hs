module Gundeck.API.Error where

import Data.Text.Lazy (Text)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error (Error (..))

notificationNotFound :: Error
notificationNotFound = Error status404 "not-found" "Some notifications not found."

clientError :: Text -> Error
clientError = Error status400 "client-error"

invalidNotificationId :: Error
invalidNotificationId = clientError "Notification ID must be a version 1 UUID"
