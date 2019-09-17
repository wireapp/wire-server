module Gundeck.API (sitemap) where

import Imports hiding (head)
import Control.Lens (view)
import Data.Metrics.Middleware
import Data.Range
import Data.Swagger.Build.Api hiding (def, min, Response)
import Data.Text.Encoding (decodeLatin1)
import Gundeck.API.Error
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger

import qualified Data.Swagger.Build.Api as Swagger
import qualified Gundeck.Client as Client
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Push as Push
import qualified Gundeck.Presence as Presence
import qualified Gundeck.Types.Swagger as Model

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do

    -- Push API -----------------------------------------------------------

    post "/push/tokens" (continue Push.addToken) $
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

    delete "/push/tokens/:pid" (continue Push.deleteToken) $
        header "Z-User"
        .&. param "pid"
        .&. accept "application" "json"

    document "DELETE" "unregisterPushToken" $ do
        summary "Unregister a native push token"
        parameter Path "pid" bytes' $
            description "The push token to delete"
        response 204 "Push token unregistered" end
        response 404 "Push token does not exist" end

    get "/push/tokens" (continue Push.listTokens) $
        header "Z-User"
        .&. accept "application" "json"

    document "GET" "getPushTokens" $ do
        summary "List the user's registered push tokens."
        returns (ref Model.pushTokenList)
        response 200 "Object containing list of push tokens" end

    post "/i/push" (continue Push.push) $
        request .&. accept "application" "json"
        -- TODO: REFACTOR: this end-point is probably noise, and should be dropped.  @/i/push/v2@ does exactly
        -- the same thing.

    post "/i/push/v2" (continue Push.push) $
        request .&. accept "application" "json"

    -- Notification API --------------------------------------------------------

    get "/notifications" (continue Notification.paginate) $
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

    get "/notifications/:id" (continue Notification.getById) $
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

    get "/notifications/last" (continue Notification.getLast) $
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

    -- DEPRECATED: this is deprecated as of https://github.com/wireapp/wire-server/pull/549 (can be
    -- removed once brig is deployed everywhere and won't trip over this missing any more.)
    put "/i/clients/:cid" (continue Client.register) $
        header "Z-User"
        .&. param "cid"
        .&. request
        .&. contentType "application" "json"
        .&. accept "application" "json"

    delete "/i/clients/:cid" (continue Client.unregister) $
        header "Z-User" .&. param "cid"

    delete "/i/user" (continue Client.removeUser) $
        header "Z-User"

    -- Docs ------------------------------------------------------------------

    get "/push/api-docs" (continue docs) $
        query "base_url" .&. accept "application" "json"

    -- Status & Monitoring ---------------------------------------------------

    head "/i/status" (continue $ const (return empty)) true

    get  "/i/status" (continue $ const (return empty)) true

    get "/i/monitoring" (continue monitoring) $
        accept "application" "json"

type JSON = Media "application" "json"

docs :: ByteString ::: JSON -> Gundeck Response
docs (url ::: _) =
    let doc = mkSwaggerApi (decodeLatin1 url) Model.gundeckModels sitemap in
    return $ json doc

monitoring :: JSON -> Gundeck Response
monitoring _ = json <$> (render =<< view monitor)
