module Gundeck.API (sitemap) where

import Control.Lens ((^.))
import Data.Id
import Data.Range
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Data.Swagger.Build.Api as Swagger
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.Encoding as Text
import Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Gundeck.API.Error
import qualified Gundeck.Client as Client
import Gundeck.Monad
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Presence as Presence
import qualified Gundeck.Push as Push
import Gundeck.Types
import qualified Gundeck.Types.Swagger as Model
import Imports hiding (getLast, head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Response (json, setStatus)
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

  get "/i/presences/:uid" (continue Presence.list) $
    param "uid" .&. accept "application" "json"
  get "/i/presences" (continue Presence.listAll) $
    param "ids" .&. accept "application" "json"
  post "/i/presences" (continue Presence.add) $
    request .&. accept "application" "json"
  delete "/i/presences/:uid/devices/:did/cannons/:cannon" (continue Presence.remove) $
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
addTokenH (uid ::: cid ::: req ::: _) = do
  newtok <- fromJsonBody req
  handleAddTokenResponse <$> Push.addToken uid cid newtok

handleAddTokenResponse :: Push.AddTokenResponse -> Response
handleAddTokenResponse = \case
  Push.AddTokenSuccess newtok -> success newtok
  Push.AddTokenNoBudget -> snsThreadBudgetReached
  Push.AddTokenNotFound -> notFound
  Push.AddTokenInvalid -> invalidToken
  Push.AddTokenTooLong -> tokenTooLong
  Push.AddTokenMetadataTooLong -> metadataTooLong

success :: PushToken -> Response
success t =
  let loc = Text.encodeUtf8 . tokenText $ t ^. token
   in json t & setStatus status201 & addHeader hLocation loc

invalidToken :: Response
invalidToken =
  json (Error status400 "invalid-token" "Invalid push token")
    & setStatus status404

snsThreadBudgetReached :: Response
snsThreadBudgetReached =
  json (Error status400 "sns-thread-budget-reached" "Too many concurrent calls to SNS; is SNS down?")
    & setStatus status413

tokenTooLong :: Response
tokenTooLong =
  json (Error status400 "token-too-long" "Push token length must be < 8192 for GCM or 400 for APNS")
    & setStatus status413

metadataTooLong :: Response
metadataTooLong =
  json (Error status400 "metadata-too-long" "Tried to add token to endpoint resulting in metadata length > 2048")
    & setStatus status413

notFound :: Response
notFound = empty & setStatus status404

deleteTokenH :: UserId ::: Token ::: JSON -> Gundeck Response
deleteTokenH (uid ::: tok ::: _) = setStatus status204 empty <$ Push.deleteToken uid tok

listTokensH :: UserId ::: JSON -> Gundeck Response
listTokensH (uid ::: _) = setStatus status200 . json <$> Push.listTokens uid

pushH :: Request ::: JSON -> Gundeck Response
pushH (req ::: _) = do
  ps <- fromJsonBody (JsonRequest req)
  empty <$ Push.push ps

paginateH :: JSON ::: UserId ::: Maybe ByteString ::: Maybe ClientId ::: Range 100 10000 Int32 -> Gundeck Response
paginateH (_ ::: uid ::: sinceRaw ::: clt ::: size) = do
  Notification.PaginateResult gap page <- Notification.paginate uid (join since) clt size
  pure . updStatus gap . json $ page
  where
    since :: Maybe (Maybe NotificationId)
    since = parseUUID <$> sinceRaw
    --
    parseUUID :: ByteString -> Maybe NotificationId
    parseUUID = UUID.fromASCIIBytes >=> isV1UUID >=> return . Id
    --
    isV1UUID :: UUID -> Maybe UUID
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing
    --
    -- the motivation of updStatus is far from clear to me, but this is how it's always been done.
    updStatus :: Bool -> Response -> Response
    updStatus True = setStatus status404
    updStatus False = case since of
      Just (Just _) -> id
      Nothing -> id
      Just Nothing -> setStatus status404

getByIdH :: JSON ::: UserId ::: NotificationId ::: Maybe ClientId -> Gundeck Response
getByIdH (_ ::: uid ::: nid ::: cid) = json <$> Notification.getById uid nid cid

getLastH :: JSON ::: UserId ::: Maybe ClientId -> Gundeck Response
getLastH (_ ::: uid ::: cid) = json <$> Notification.getLast uid cid

unregisterClientH :: UserId ::: ClientId -> Gundeck Response
unregisterClientH (uid ::: cid) = empty <$ Client.unregister uid cid

removeUserH :: UserId -> Gundeck Response
removeUserH uid = empty <$ Client.removeUser uid
