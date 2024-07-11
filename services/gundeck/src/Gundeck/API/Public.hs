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
import Data.Text.Encoding qualified as Text
import Gundeck.Monad
import Gundeck.Notification qualified as Notification
import Gundeck.Notification.Data qualified as Data
import Gundeck.Push qualified as Push
import Imports
import Servant (HasServer (..), (:<|>) (..))
import Wire.API.Notification qualified as Public
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Gundeck

-------------------------------------------------------------------------------
-- Servant API

servantSitemap :: ServerT GundeckAPI Gundeck
servantSitemap = pushAPI :<|> notificationAPI
  where
    pushAPI =
      Named @"register-push-token" Push.addToken
        :<|> Named @"delete-push-token" Push.deleteToken
        :<|> Named @"get-push-tokens" Push.listTokens

    notificationAPI =
      Named @"get-last-notification" Data.fetchLast
        :<|> Named @"get-notification-by-id" Data.fetchId
        :<|> Named @"get-notifications@v2" paginateUntilV2
        :<|> Named @"get-notifications" paginate

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
-- (arianvp): I am not sure why it is convenient for clients to distinguish
-- between these two cases.
paginateUntilV2 ::
  UserId ->
  Maybe Public.RawNotificationId ->
  Maybe ClientId ->
  Maybe (Range 100 10000 Int32) ->
  Gundeck Public.GetNotificationsResponse
paginateUntilV2 uid mbSince mbClient mbSize = do
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
    parseUUID = pure . Text.decodeUtf8 . Public.unRawNotificationId

paginate ::
  UserId ->
  Maybe Public.NotificationId ->
  Maybe ClientId ->
  Maybe (Range 100 10000 Int32) ->
  Gundeck (Maybe Public.QueuedNotificationList)
paginate uid mbSince mbClient mbSize = do
  let size = fromMaybe (unsafeRange 1000) mbSize
  Notification.PaginateResult gap page <- Notification.paginate uid mbSince mbClient size
  pure $ if gap then Nothing else Just page
