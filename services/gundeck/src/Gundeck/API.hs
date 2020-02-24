module Gundeck.API (sitemap) where

import Data.ByteString.Conversion (List)
import Data.Id
import Data.Range
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Data.Swagger.Build.Api as Swagger
import Data.Text.Encoding (decodeLatin1)
import Gundeck.API.Error
import qualified Gundeck.Client as Client
import Gundeck.Monad
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Presence as Presence
import qualified Gundeck.Push as Push
import Gundeck.Types
import qualified Gundeck.Types.Swagger as Model
import Imports hiding (head)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do
  -- Push API -----------------------------------------------------------

  post "/push/tokens" (continue addTokenH) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @PushToken
      .&. accept "application" "json"
  document "POST" "registerPushToken" $ do
    summary "Register a native push token"
    body (ref Model.pushToken) $
      description "JSON body"
    returns (ref Model.pushToken)
    response 201 "Push token registered" end
    response 404 "App does not exist" end
  delete "/push/tokens/:pid" (continue deleteTokenH) $
    header "Z-User"
      .&. param "pid"
      .&. accept "application" "json"
  document "DELETE" "unregisterPushToken" $ do
    summary "Unregister a native push token"
    parameter Path "pid" bytes' $
      description "The push token to delete"
    response 204 "Push token unregistered" end
    response 404 "Push token does not exist" end
  get "/push/tokens" (continue listTokensH) $
    header "Z-User"
      .&. accept "application" "json"
  document "GET" "getPushTokens" $ do
    summary "List the user's registered push tokens."
    returns (ref Model.pushTokenList)
    response 200 "Object containing list of push tokens" end
  post "/i/push/v2" (continue pushH) $
    request .&. accept "application" "json"
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
    returns (ref Model.notificationList)
    response 200 "Notification list" end
    errorResponse' notificationNotFound Model.notificationList
  get "/notifications/:id" (continue getByIdH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "id"
      .&. opt (query "client")
  document "GET" "getNotification" $ do
    summary "Fetch a notification by ID."
    parameter Query "id" bytes' $
      description "Notification ID"
    parameter Query "client" bytes' $ do
      optional
      description "Only return notifications targeted at the given client."
    returns (ref Model.notification)
    response 200 "Notification found" end
    errorResponse notificationNotFound
  get "/notifications/last" (continue getLastH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. opt (query "client")
  document "GET" "getLastNotification" $ do
    summary "Fetch the last notification."
    parameter Query "client" bytes' $ do
      optional
      description "Only return the last notification targeted at the given client."
    returns (ref Model.notification)
    response 200 "Notification found" end
    errorResponse notificationNotFound
  -- Presence API ----------------------------------------------------------

  get "/i/presences/:uid" (continue listPresencesH) $
    param "uid" .&. accept "application" "json"
  get "/i/presences" (continue listAllPresencesH) $
    param "ids" .&. accept "application" "json"
  post "/i/presences" (continue addPresenceH) $
    request .&. accept "application" "json"
  delete "/i/presences/:uid/devices/:did/cannons/:cannon" (continue removePresenceH) $
    param "uid" .&. param "did" .&. param "cannon"
  -- User-Client API -------------------------------------------------------

  delete "/i/clients/:cid" (continue unregisterClientH) $
    header "Z-User" .&. param "cid"
  delete "/i/user" (continue removeUserH) $
    header "Z-User"
  -- Docs ------------------------------------------------------------------

  get "/push/api-docs" (continue docsH) $
    query "base_url" .&. accept "application" "json"
  -- Status & Monitoring ---------------------------------------------------

  head "/i/status" (continue $ const (return empty)) true
  get "/i/status" (continue $ const (return empty)) true

type JSON = Media "application" "json"

docsH :: ByteString ::: JSON -> Gundeck Response
docsH (url ::: _) =
  let doc = mkSwaggerApi (decodeLatin1 url) Model.gundeckModels sitemap
   in return $ json doc

addTokenH :: UserId ::: ConnId ::: JsonRequest PushToken ::: JSON -> Gundeck Response
addTokenH = Push.addToken

deleteTokenH :: UserId ::: Token ::: JSON -> Gundeck Response
deleteTokenH = Push.deleteToken

listTokensH :: UserId ::: JSON -> Gundeck Response
listTokensH = Push.listTokens

pushH :: Request ::: JSON -> Gundeck Response
pushH = Push.push

paginateH :: JSON ::: UserId ::: Maybe ByteString ::: Maybe ClientId ::: Range 100 10000 Int32 -> Gundeck Response
paginateH = Notification.paginate

getByIdH :: JSON ::: UserId ::: NotificationId ::: Maybe ClientId -> Gundeck Response
getByIdH = Notification.getById

getLastH :: JSON ::: UserId ::: Maybe ClientId -> Gundeck Response
getLastH = Notification.getLast

listPresencesH :: UserId ::: JSON -> Gundeck Response
listPresencesH = Presence.list

listAllPresencesH :: List UserId ::: JSON -> Gundeck Response
listAllPresencesH = Presence.listAll

addPresenceH :: Request ::: JSON -> Gundeck Response
addPresenceH = Presence.add

removePresenceH :: UserId ::: ConnId ::: CannonId -> Gundeck Response
removePresenceH = Presence.remove

unregisterClientH :: UserId ::: ClientId -> Gundeck Response
unregisterClientH = Client.unregister

removeUserH :: UserId -> Gundeck Response
removeUserH = Client.removeUser
