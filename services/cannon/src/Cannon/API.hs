{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cannon.API (run) where

import Bilge (newManager, defaultManagerSettings, ManagerSettings (..))
import Cannon.App
import Cannon.Types
import Cannon.WS hiding (env)
import Control.Applicative hiding (empty, optional)
import Control.Monad.Catch
import Data.Aeson (encode)
import Data.Id (ClientId, UserId, ConnId)
import Data.Metrics.Middleware
import Data.Swagger.Build.Api hiding (def, Response)
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, (#))
import Network.Wai.Routing hiding (route, path)
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Server
import Network.Wai.Utilities.Swagger
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WebSockets
import Prelude hiding (head)
import System.Logger.Class hiding (Error)
import System.Random.MWC (createSystemRandom)

import qualified Cannon.Dict                 as D
import qualified Data.Metrics.Middleware     as Metrics
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.WebSockets          as Ws
import qualified System.Logger               as Logger

run :: Opts -> IO ()
run o = do
    m <- metrics
    g <- new (setOutput StdOut . setFormat Nothing $ defSettings)
    e <- mkEnv <$> pure m
               <*> pure o
               <*> pure g
               <*> D.empty 128
               <*> newManager defaultManagerSettings { managerConnCount = 128 }
               <*> createSystemRandom
               <*> mkClock
    s <- newSettings $ Server (host o) (port o) (applog e) m (Just idleTimeout) [] []
    let rtree    = compile sitemap
        measured = measureRequests m rtree
        app  r k = runCannon e (route rtree r k) r
        start    = measured . catchErrors g m $ Gzip.gzip Gzip.def app
    runSettings s start `finally` Logger.close (applog e)
  where
    idleTimeout = fromIntegral $ maxPingInterval + 3

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

    get "/i/monitoring" (continue monitoring) $
        accept "application" "json"

    get  "/i/status" (continue (const $ return empty)) true

    head "/i/status" (continue (const $ return empty)) true

monitoring :: Media "application" "json" -> Cannon Response
monitoring = const $ do
    m <- monitor
    s <- D.size =<< clients
    gaugeSet s (path "net.websocket.clients") m
    json <$> Metrics.render m

docs :: Media "application" "json" ::: Text -> Cannon Response
docs (_ ::: url) = do
    let doc = encode $ mkSwaggerApi url [] sitemap
    return $ responseLBS status200 [jsonContent] doc

push :: UserId ::: ConnId ::: Request -> Cannon Response
push (user ::: conn ::: req) = do
    let k = mkKey user conn
    d <- clients
    debug $ client (key2bytes k) . msg (val "push")
    c <- D.lookup k d
    case c of
        Nothing -> do
            debug $ client (key2bytes k) . msg (val "push: client gone")
            return clientGone
        Just x  -> do
            b <- readBody req
            e <- wsenv
            runWS e $
                (sendMsg b k x >> return empty)
                `catchAll`
                const (terminate k x >> return clientGone)
  where
    clientGone = errorRs status410 "general" "client gone"

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
