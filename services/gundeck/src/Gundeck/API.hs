{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Gundeck.API where

import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import Control.Exception (finally)
import Control.Lens hiding (enum)
import Control.Monad
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Metrics.Middleware
import Data.Range
import Data.Swagger.Build.Api hiding (def, min, Response)
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import Gundeck.API.Error
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options
import Gundeck.React
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Swagger
import Network.Wai.Utilities.Server hiding (serverPort)
import Prelude hiding (head)
import Util.Options

import qualified Control.Concurrent.Async as Async
import qualified Data.Metrics as Metrics
import qualified Data.Swagger.Build.Api as Swagger
import qualified Gundeck.Aws as Aws
import qualified Gundeck.Client as Client
import qualified Gundeck.Notification as Notification
import qualified Gundeck.Push as Push
import qualified Gundeck.Push.Native.Fallback.Queue as Fallback
import qualified Gundeck.Presence as Presence
import qualified Gundeck.Types.Swagger as Model
import qualified Network.Wai.Middleware.Gzip as GZip
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified System.Logger as Log

runServer :: Opts -> IO ()
runServer o = do
    m <- metrics
    e <- createEnv m o
    runClient (e^.cstate) $
        versionCheck schemaVersion
    let l = e^.applog
    s <- newSettings $ defaultServer (unpack $ o^.gundeck.epHost) (o^.gundeck.epPort) l m
    app <- pipeline e
    lst <- Async.async $ Aws.execute (e^.awsEnv) (Aws.listen (runDirect e . onEvent))
    runSettingsWithShutdown s app 5 `finally` do
        Log.info l $ Log.msg (Log.val "Draining fallback queue ...")
        Fallback.drainQueue (e^.fbQueue) (fromIntegral (o^.fallback.fbQueueDelay) + 1)
        Log.info l $ Log.msg (Log.val "Shutting down ...")
        shutdown (e^.cstate)
        Async.cancel lst
        Log.close (e^.applog)
  where
    pipeline e = do
        let routes = compile sitemap
        return $ measureRequests (e^.monitor) routes
               . catchErrors (e^.applog) (e^.monitor)
               . GZip.gunzip . GZip.gzip GZip.def
               $ \r k -> runGundeck e r (route routes r k)

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do

    -- Push API -----------------------------------------------------------

    post "/push/tokens" (continue Push.addToken) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. request
        .&. accept "application" "json"
        .&. contentType "application" "json"

    document "POST" "registerPushToken" $ do
        summary "Register a native push token"
        body (ref Model.push) $
            description "JSON body"
        returns (ref Model.push)
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

    post "/push/fallback/:notif/cancel" (continue Push.cancelFallback) $
        header "Z-User"
        .&. capture "notif"

    document "POST" "cancelFallback" $ do
        summary "Cancel a pending fallback notification."
        parameter Path "notif" bytes' $
            description "The notification ID"
        response 200 "Pending fallback notification cancelled" end

    get "/i/push/tokens" (continue Push.listTokens) $
        header "Z-User" .&. accept "application" "json"

    post "/i/push" (continue Push.push) $
        request .&. accept "application" "json"

    post "/i/push/v2" (continue Push.push) $
        request .&. accept "application" "json"

    -- Notification API --------------------------------------------------------

    get "/notifications" (continue Notification.paginate) $
        accept "application" "json"
        .&. header "Z-User"
        .&. opt (query "since")
        .&. opt (query "client")
        .&. def (unsafeRange 1000) (query "size")
        .&. opt (query "cancel_fallback")

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
        parameter Query "cancel_fallback" bytes' $ do
            optional
            description "Cancel pending fallback notifications for the given ID, if any."
        returns (ref Model.notificationList)
        response 200 "Notification list" end
        errorResponse' notificationNotFound Model.notificationList

    get "/notifications/:id" (continue Notification.getById) $
        accept "application" "json"
        .&. header "Z-User"
        .&. capture "id"
        .&. opt (query "client")
        .&. def False (query "cancel_fallback")

    document "GET" "getNotification" $ do
        summary "Fetch a notification by ID."
        parameter Query "id" bytes' $
            description "Notification ID"
        parameter Query "client" bytes' $ do
            optional
            description "Only return notifications targeted at the given client."
        parameter Query "cancel_fallback" (bool (Swagger.def False)) $ do
            optional
            description "Whether to cancel pending fallback notifications, if any."
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
    let doc = encode $ mkSwaggerApi (decodeLatin1 url) Model.gundeckModels sitemap in
    return $ responseLBS status200 [jsonContent] doc

monitoring :: JSON -> Gundeck Response
monitoring = const $ do
    m  <- view monitor
    ql <- Fallback.queueLength =<< view fbQueue
    Metrics.gaugeSet ql (Metrics.path "push.fallback.queue_length") m
    json <$> render m
