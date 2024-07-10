module Galley.API.Public.TeamNotification where

import Data.Id
import Data.Range
import Data.UUID.Util qualified as UUID
import Galley.API.Teams.Notifications qualified as APITeamQueue
import Galley.App
import Galley.Effects
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Internal.Notification
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.TeamNotification

teamNotificationAPI :: API TeamNotificationAPI GalleyEffects
teamNotificationAPI =
  mkNamedAPI @"get-team-notifications" getTeamNotifications

type SizeRange = Range 1 10000 Int32

-- | See also: 'Gundeck.API.Public.paginateH', but the semantics of this end-point is slightly
-- less warped.  This is a work-around because we cannot send events to all of a large team.
-- See haddocks of module "Galley.API.TeamNotifications" for details.
getTeamNotifications ::
  ( Member BrigAccess r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidTeamNotificationId) r,
    Member TeamNotificationStore r
  ) =>
  UserId ->
  Maybe NotificationId ->
  Maybe SizeRange ->
  Sem r QueuedNotificationList
getTeamNotifications uid since size = do
  since' <- checkSince since
  APITeamQueue.getTeamNotifications
    uid
    since'
    (fromMaybe defaultSize size)
  where
    checkSince ::
      (Member (ErrorS 'InvalidTeamNotificationId) r) =>
      Maybe NotificationId ->
      Sem r (Maybe NotificationId)
    checkSince Nothing = pure Nothing
    checkSince (Just (parseIdFromText -> (Right nid)))
      | (UUID.version . toUUID) nid == 1 =
          (pure . Just . idToText) nid
    checkSince (Just _) = throwS @'InvalidTeamNotificationId

    defaultSize :: SizeRange
    defaultSize = unsafeRange 1000
