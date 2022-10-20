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

module Gundeck.API.Public
  ( servantSitemap,
  )
where

import Data.Id
import Data.Range
import Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Gundeck.Monad
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Push as Push
import Imports
import Servant (HasServer (..), (:<|>) (..))
import qualified Wire.API.Notification as Public
import qualified Wire.API.Push.Token as Public
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Gundeck

-------------------------------------------------------------------------------
-- Servant API

servantSitemap :: ServerT GundeckAPI Gundeck
servantSitemap = pushAPI :<|> notificationAPI
  where
    pushAPI =
      Named @"register-push-token" addToken
        :<|> Named @"delete-push-token" deleteToken
        :<|> Named @"get-push-tokens" listTokens

    notificationAPI =
      Named @"get-notification-by-id" getById
        :<|> Named @"get-last-notification" getLastNotification
        :<|> Named @"get-notifications" paginate

addToken :: UserId -> ConnId -> Public.PushToken -> Gundeck (Either Public.AddTokenError Public.AddTokenSuccess)
addToken uid cid newTok =
  Push.addToken uid cid newTok <&> \case
    Push.AddTokenSuccess t -> Right (Public.AddTokenSuccess t)
    Push.AddTokenNoBudget -> Left Public.AddTokenErrorNoBudget
    Push.AddTokenNotFound -> Left Public.AddTokenErrorNotFound
    Push.AddTokenInvalid -> Left Public.AddTokenErrorInvalid
    Push.AddTokenTooLong -> Left Public.AddTokenErrorTooLong
    Push.AddTokenMetadataTooLong -> Left Public.AddTokenErrorMetadataTooLong

deleteToken :: UserId -> Public.Token -> Gundeck (Maybe ())
deleteToken = Push.deleteToken

listTokens :: UserId -> Gundeck Public.PushTokenList
listTokens = Push.listTokens

-- | Returns a list of notifications for given 'uid'
--
--
-- Takes an optional parameter 'since' which is a V1 UUID, (which includes a
-- timestamp).
--
-- If the parameter 'since' is omitted, all notifications of the user are
-- returned. This is not recommended. (TODO: Ask client teams if they ever use
-- this)
--
-- If the parameter 'since' fails to parse, all notifications of the user are
-- returned but the status code is set to 404.
-- FUTUREWORK: We should change this behaviour as it's extremely confusing. We
-- could kindly reject with a 400, and not event hit the database at all.
-- This was introduced in
-- https://github.com/zinfra/orlop/pull/30/commits/a358dfc1cb225c92066ea79db28c8824531ae231
--
-- If the 'since' parameter is present, and a notification 'since' is actually
-- found in the database, this returns all the notifications since 'since'
-- (exclusive of 'since' itself) and returns a status code 200.
--
-- If the 'since' parameter is present, and a notification 'since' is not found
-- in the database, then due to the fact that 'since' is a V1 UUID (which
-- contains a timestamp) we can still return all the notifications that
-- happened after it eventhough it is not present in the database. This can
-- happen for example because a client hasn't been online for 30 days and we
-- have deleted the notification in the backend in the meantime.
-- We will return all the notifications that we have that happened after 'since'
-- but return status code 404 to signal that 'since' itself was missing.
--
-- (arianvp): I am not sure why it is convenient for clients to distinct
-- between these two cases.
paginate ::
  UserId ->
  Maybe Public.RawNotificationId ->
  Maybe ClientId ->
  Maybe (Range 100 10000 Int32) ->
  Gundeck Public.GetNotificationsResponse
paginate uid mbSince mbClient mbSize = do
  let size = fromMaybe (unsafeRange 1000) mbSize
  Notification.PaginateResult gap page <- Notification.paginate uid (join since) mbClient size
  pure $
    if gap
      then Public.GetNotificationsWithStatusNotFound page
      else case since of
        Just (Just _) -> Public.GetNotificationsSuccess page
        Nothing -> Public.GetNotificationsSuccess page
        Just Nothing -> Public.GetNotificationsWithStatusNotFound page
  where
    since :: Maybe (Maybe Public.NotificationId)
    since = parseUUID <$> mbSince

    parseUUID :: Public.RawNotificationId -> Maybe Public.NotificationId
    parseUUID = (UUID.fromASCIIBytes . Public.unRawNotificationId) >=> isV1UUID >=> pure . Id

    isV1UUID :: UUID -> Maybe UUID
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing

getById :: UserId -> Public.NotificationId -> Maybe ClientId -> Gundeck (Maybe Public.QueuedNotification)
getById = Notification.getById

getLastNotification :: UserId -> Maybe ClientId -> Gundeck (Maybe Public.QueuedNotification)
getLastNotification = Notification.getLast
