module Cannon.API (sitemap) where

import Cannon.App
import qualified Cannon.Dict as D
import Cannon.Types
import Cannon.WS hiding (env)
import Control.Monad.Catch
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L
import Data.Id (ClientId, ConnId, UserId)
import Data.Swagger.Build.Api hiding (Response, def)
import Gundeck.Types
import Gundeck.Types.BulkPush
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.Wai.Predicate hiding ((#), Error)
import Network.Wai.Routing hiding (path, route)
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Request (parseBody')
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger
import qualified Network.WebSockets as Ws
import System.Logger.Class (msg, val)
import qualified System.Logger.Class as LC

sitemap :: Routes ApiBuilder Cannon ()
sitemap = do
  get "/await" (continue awaitH) $
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
  get "/await/api-docs" (continue docsH) $
    accept "application" "json"
      .&. query "base_url"
  post "/i/push/:user/:conn" (continue pushH) $
    capture "user" .&. capture "conn" .&. request
  post "/i/bulkpush" (continue bulkpushH) $
    request
  head "/i/presences/:uid/:conn" (continue checkPresenceH) $
    param "uid" .&. param "conn"
  get "/i/status" (continue (const $ return empty)) true
  head "/i/status" (continue (const $ return empty)) true

docsH :: Media "application" "json" ::: Text -> Cannon Response
docsH (_ ::: url) = do
  let doc = mkSwaggerApi url [] sitemap
  return $ json doc

pushH :: UserId ::: ConnId ::: Request -> Cannon Response
pushH (user ::: conn ::: req) =
  singlePush (readBody req) (PushTarget user conn) >>= \case
    PushStatusOk -> return empty
    PushStatusGone -> return $ errorRs status410 "general" "client gone"

-- | Parse the entire list of notifcations and targets, then call 'singlePush' on the each of them
-- in order.
bulkpushH :: Request -> Cannon Response
bulkpushH req = json <$> (parseBody' (JsonRequest req) >>= bulkpush)

-- | The typed part of 'bulkpush'.
bulkpush :: BulkPushRequest -> Cannon BulkPushResponse
bulkpush (BulkPushRequest notifs) =
  BulkPushResponse . mconcat . zipWith compileResp notifs <$> (uncurry doNotif `mapM` notifs)
  where
    doNotif :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotif (pure . encode -> notif) = mapConcurrentlyCannon (singlePush notif)
    compileResp ::
      (Notification, [PushTarget]) ->
      [PushStatus] ->
      [(NotificationId, PushTarget, PushStatus)]
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
          `catchAll` const (terminate k x >> return PushStatusGone)

checkPresenceH :: UserId ::: ConnId -> Cannon Response
checkPresenceH (u ::: c) = do
  e <- wsenv
  registered <- runWS e $ isRemoteRegistered u c
  if registered
    then return empty
    else return $ errorRs status404 "not-found" "presence not registered"

awaitH :: UserId ::: ConnId ::: Maybe ClientId ::: Request -> Cannon Response
awaitH (u ::: a ::: c ::: r) = do
  e <- wsenv
  case websocketsApp wsoptions (wsapp (mkKey u a) c e) r of
    Nothing -> return $ errorRs status426 "request-error" "websocket upgrade required"
    Just rs -> return rs -- ensure all middlewares ignore RawResponse - see Note [Raw Response]
  where
    status426 = mkStatus 426 "Upgrade Required"
    wsoptions = Ws.defaultConnectionOptions
