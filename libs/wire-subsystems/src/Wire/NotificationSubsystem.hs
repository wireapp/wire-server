module Wire.NotificationSubsystem
  ( module Wire.NotificationSubsystem.Internal,
    newPush1,
    newPush,
    newPushLocal,
    newPushLocal1,
  )
where

import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Gundeck.Types hiding (Push (..), Recipient, newPush)
import Imports
-- Importing like this hides only the constructors for NotificationSubsystem,
-- which are not very useful but have names which conflict with other
-- constructors
import Wire.NotificationSubsystem.Internal (NotificationSubsystem)
import Wire.NotificationSubsystem.Internal hiding (NotificationSubsystem (..))

newPush1 :: Maybe UserId -> Object -> NonEmpty Recipient -> Push
newPush1 from e rr =
  Push
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = RouteAny,
      _pushNativePriority = Nothing,
      _pushApsData = Nothing,
      pushJson = e,
      pushOrigin = from,
      _pushRecipients = rr
    }

newPush :: Maybe UserId -> Object -> [Recipient] -> Maybe Push
newPush _ _ [] = Nothing
newPush u e (r : rr) = Just $ newPush1 u e (r :| rr)

newPushLocal :: UserId -> Object -> [Recipient] -> Maybe Push
newPushLocal uid = newPush (Just uid)

newPushLocal1 :: UserId -> Object -> NonEmpty Recipient -> Push
newPushLocal1 uid = newPush1 (Just uid)
