module Cannon.API (sitemap) where

import Imports hiding (head)
import Cannon.App
import Cannon.Types
import Cannon.WS hiding (env)
import Control.Monad.Catch
import Data.Aeson (encode)
import Data.Id (ClientId, UserId, ConnId)
import Data.Metrics.Middleware
import Data.Swagger.Build.Api hiding (def, Response)
import Network.HTTP.Types
import Gundeck.Types
import Gundeck.Types.BulkPush
import Network.Wai
import Network.Wai.Predicate hiding (Error, (#))
import Network.Wai.Routing hiding (route, path)
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Request (parseBody')
import Network.Wai.Utilities.Swagger
import Network.Wai.Handler.WebSockets
import System.Logger (msg, val)

import qualified Cannon.Dict                 as D
import qualified Data.ByteString.Lazy        as L
import qualified Data.Metrics.Middleware     as Metrics
import qualified Network.WebSockets          as Ws
import qualified System.Logger.Class         as LC


sitemap :: Routes ApiBuilder Cannon ()
sitemap = do
    get "/await" (continue await) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. opt (query "client")
        .&. request

    document "GET" "await" $ do
        summary "Establish websocket connection"
        parameter Header "Upgrade" (string $ enum ["websocket"]) end
        parameter Header "Connection" (string $ enum ["upgrade"]) end
        parameter Header "Sec-WebSocket-Key" bytes' $
            description "16-bytes base64 encoded nonce"
        parameter Header "Sec-WebSocket-Version" (int32 $ enum [13]) end
        parameter Query "client" string' $ do
            optional
            description "Client ID"
        response 426 "Upgrade required" end

    get "/await/api-docs" (continue docs) $
        accept "application" "json"
        .&. query "base_url"

    post "/i/push/:user/:conn" (continue push) $
        capture "user" .&. capture "conn" .&. request

    post "/i/bulkpush" (continue bulkpush) $
        request

    head "/i/presences/:user/:conn" (continue checkPresence) $
        param "user" .&. param "conn"

    get "/i/monitoring" (continue monitoring) $
        accept "application" "json"

    get  "/i/status" (continue (const $ return empty)) true

    head "/i/status" (continue (const $ return empty)) true

monitoring :: Media "application" "json" -> Cannon Response
monitoring = const $ do
    m <- monitor
    s <- D.size =<< clients
    gaugeSet (fromIntegral s) (path "net.websocket.clients") m
    json <$> Metrics.render m

docs :: Media "application" "json" ::: Text -> Cannon Response
docs (_ ::: url) = do
    let doc = encode $ mkSwaggerApi url [] sitemap
    return $ responseLBS status200 [jsonContent] doc

push :: UserId ::: ConnId ::: Request -> Cannon Response
push (user ::: conn ::: req) =
    singlePush (readBody req) (PushTarget user conn) >>= \case
        PushStatusOk   -> return empty
        PushStatusGone -> return $ errorRs status410 "general" "client gone"

-- | Parse the entire list of notifcations and targets, then call 'singlePush' on the each of them
-- in order.
bulkpush :: Request -> Cannon Response
bulkpush req = json <$> (parseBody' (JsonRequest req) >>= bulkpush')

-- | The typed part of 'bulkpush'.
bulkpush' :: BulkPushRequest -> Cannon BulkPushResponse
bulkpush' (BulkPushRequest notifs) =
    BulkPushResponse . mconcat . zipWith compileResp notifs <$> (uncurry doNotif `mapM` notifs)
  where
    doNotif :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotif (pure . encode -> notif) = mapConcurrentlyCannon (singlePush notif)

    compileResp :: (Notification, [PushTarget])
                -> [PushStatus]
                -> [(NotificationId, PushTarget, PushStatus)]
    compileResp (notif, prcs) pss = zip3 (repeat (ntfId notif)) prcs pss

-- | Take a serialized 'Notification' string and send it to the 'PushTarget'.
singlePush :: Cannon L.ByteString -> PushTarget -> Cannon PushStatus
singlePush notification (PushTarget usrid conid) = do
    let k = mkKey usrid conid
    d <- clients
    LC.debug $ client (key2bytes k) . msg (val "push")
    c <- D.lookup k d
    case c of
        Nothing -> do
            LC.debug $ client (key2bytes k) . msg (val "push: client gone")
            return PushStatusGone
        Just x -> do
            e <- wsenv
            b <- notification
            runWS e $
                (sendMsg b k x >> return PushStatusOk)
                `catchAll`
                const (terminate k x >> return PushStatusGone)

checkPresence :: UserId ::: ConnId -> Cannon Response
checkPresence (u ::: c) = do
    e <- wsenv
    registered <- runWS e $ isRemoteRegistered u c
    if registered
        then return empty
        else return $ errorRs status404 "not-found" "presence not registered"

await :: UserId ::: ConnId ::: Maybe ClientId ::: Request -> Cannon Response
await (u ::: a ::: c ::: r) = do
    l <- logger
    e <- wsenv
    case websocketsApp wsoptions (wsapp (mkKey u a) c l e) r of
        Nothing -> return $ errorRs status426 "request-error" "websocket upgrade required"
        Just rs -> return rs
  where
    status426 = mkStatus 426 "Upgrade Required"
    wsoptions = Ws.defaultConnectionOptions
