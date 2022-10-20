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
  ( sitemap,
    apiDocs,
    servantSitemap,
  )
where

import Data.Id
import Data.Range
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Data.Swagger.Build.Api as Swagger
import Data.Text.Encoding (decodeLatin1)
import Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Gundeck.API.Error
import Gundeck.Monad
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Push as Push
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities as Wai
import Network.Wai.Utilities.Swagger
import Servant (HasServer (..), (:<|>) (..))
import Wire.API.Notification (NotificationId)
import qualified Wire.API.Notification as Public
import qualified Wire.API.Push.Token as Public
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Gundeck
import qualified Wire.API.Swagger as Public.Swagger

-------------------------------------------------------------------------------
-- Servant API

servantSitemap :: ServerT GundeckAPI Gundeck
servantSitemap = pushAPI :<|> notificationAPI
  where
    pushAPI =
      Named @"register-push-token" addToken
        :<|> Named @"delete-push-token" deleteToken

    notificationAPI = Named @"get-notification-by-id" getById

--------------------------------------------------------------------------------
-- Wai Routes API

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do
  -- Push API -----------------------------------------------------------

  get "/push/tokens" (continue listTokensH) $
    header "Z-User"
      .&. accept "application" "json"
  document "GET" "getPushTokens" $ do
    summary "List the user's registered push tokens."
    returns (ref Public.modelPushTokenList)
    response 200 "Object containing list of push tokens" end

  -- Notification API --------------------------------------------------------

  get "/notifications" (continue paginateH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. opt (query "since")
      .&. opt (query "client")
      .&. def (unsafeRange 1000) (query "size")
  document "GET" "fetchNotifications" $ do
    summary "Fetch notifications"
    parameter Query "since" bytes' $ do
      optional
      description "Only return notifications more recent than this."
    parameter Query "client" bytes' $ do
      optional
      description "Only return notifications targeted at the given client."
    parameter Query "size" (int32 (Swagger.def 1000)) $ do
      optional
      description "Maximum number of notifications to return."
    returns (ref Public.modelNotificationList)
    response 200 "Notification list" end
    errorResponse' notificationNotFound Public.modelNotificationList

  get "/notifications/last" (continue getLastH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. opt (query "client")
  document "GET" "getLastNotification" $ do
    summary "Fetch the last notification."
    parameter Query "client" bytes' $ do
      optional
      description "Only return the last notification targeted at the given client."
    returns (ref Public.modelNotification)
    response 200 "Notification found" end
    errorResponse notificationNotFound

apiDocs :: Routes ApiBuilder Gundeck ()
apiDocs = do
  get "/push/api-docs" (continue docsH) $
    query "base_url" .&. accept "application" "json"

type JSON = Media "application" "json"

docsH :: ByteString ::: JSON -> Gundeck Response
docsH (url ::: _) =
  let doc = mkSwaggerApi (decodeLatin1 url) Public.Swagger.models sitemap
   in pure $ json doc

addToken :: UserId -> ConnId -> Public.PushToken -> Gundeck (Either Public.AddTokenError Public.AddTokenSuccess)
addToken uid cid newtok =
  Push.addToken uid cid newtok <&> \case
    Push.AddTokenSuccess t -> Right (Public.AddTokenSuccess t)
    Push.AddTokenNoBudget -> Left Public.AddTokenErrorNoBudget
    Push.AddTokenNotFound -> Left Public.AddTokenErrorNotFound
    Push.AddTokenInvalid -> Left Public.AddTokenErrorInvalid
    Push.AddTokenTooLong -> Left Public.AddTokenErrorTooLong
    Push.AddTokenMetadataTooLong -> Left Public.AddTokenErrorMetadataTooLong

deleteToken :: UserId -> Public.Token -> Gundeck (Maybe ())
deleteToken = Push.deleteToken

listTokensH :: UserId ::: JSON -> Gundeck Response
listTokensH (uid ::: _) =
  setStatus status200 . json @Public.PushTokenList <$> Push.listTokens uid

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
paginateH :: JSON ::: UserId ::: Maybe ByteString ::: Maybe ClientId ::: Range 100 10000 Int32 -> Gundeck Response
paginateH (_ ::: uid ::: sinceRaw ::: clt ::: size) = do
  Notification.PaginateResult gap page <- Notification.paginate uid (join since) clt size
  pure . updStatus gap . json $ (page :: Public.QueuedNotificationList)
  where
    since :: Maybe (Maybe NotificationId)
    since = parseUUID <$> sinceRaw
    parseUUID :: ByteString -> Maybe NotificationId
    parseUUID = UUID.fromASCIIBytes >=> isV1UUID >=> pure . Id
    isV1UUID :: UUID -> Maybe UUID
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing
    updStatus :: Bool -> Response -> Response
    updStatus True = setStatus status404
    updStatus False = case since of
      Just (Just _) -> id
      Nothing -> id
      Just Nothing -> setStatus status404

getById :: UserId -> NotificationId -> Maybe ClientId -> Gundeck (Maybe Public.QueuedNotification)
getById = Notification.getById

getLastH :: JSON ::: UserId ::: Maybe ClientId -> Gundeck Response
getLastH (_ ::: uid ::: cid) =
  json @Public.QueuedNotification <$> Notification.getLast uid cid
