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

module Proxy.API.Public
  ( PublicAPI,
    servantSitemap,
    waiRoutingSitemap,
  )
where

import Bilge.Request qualified as Req
import Bilge.Response qualified as Res
import Cassandra.Options
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry
import Data.ByteString (breakSubstring)
import Data.ByteString.Lazy qualified as B
import Data.CaseInsensitive
import Data.CaseInsensitive qualified as CI
import Data.Configurator qualified as Config
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Lazy qualified as LText
import Imports hiding (head)
import Network.HTTP.Client qualified as Client
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal qualified as I
import Network.Wai.Predicate hiding (Error, err, setStatus)
import Network.Wai.Predicate.Request (getRequest)
import Network.Wai.Routing hiding (path, route)
import Network.Wai.Routing qualified as Routing
import Network.Wai.Utilities
import Network.Wai.Utilities.Server (compile)
import Proxy.Env
import Proxy.Options (Opts, disableTlsForTest, giphyEndpoint, googleMapsEndpoint, youtubeEndpoint)
import Proxy.Proxy
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.API.Extended.RawM
import System.Logger.Class hiding (Error, info, render)
import System.Logger.Class qualified as Logger
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Proxy

-- TODO: test that the switch from wai-route to servant doesn't break streaming
-- characteristics (we don't want to turn O(1) memory requirement into O(n))

type PublicAPI =
  ProxyAPIRoute "giphy-path" ("giphy" :> "v1" :> "gifs" :> RawM)
    :<|> ProxyAPIRoute "youtube-path" ("youtube" :> "v3" :> RawM)
    :<|> ProxyAPIRoute
           "gmaps-static"
           ( "googlemaps"
               :> "api"
               :> "staticmap"
               -- Why do we capture path segments here? We don't want to allow
               -- access to the proxied API beyond the base path. (Who knows
               -- what might be accessible then?!) The Handler will return HTTP
               -- 404 if there are any illegal path segments.
               :> Servant.CaptureAll "illegal_segments" String
               :> RawM
           )
    -- :<|> ProxyAPIRoute "gmaps-path" ("googlemaps" :> "maps" :> "api" :> "geocode" :> RawM)
    :<|> Servant.Raw -- see https://wearezeta.atlassian.net/browse/WPB-1216

servantSitemap :: Env -> Servant.ServerT PublicAPI Proxy.Proxy.Proxy
servantSitemap e =
  Named @"giphy-path" (giphyH e)
    :<|> Named @"youtube-path" (youtubeH e)
    :<|> Named @"gmaps-static" (gmapsStaticH e)
    :<|> Servant.Tagged app
  where
    app :: Application
    app r k = appInProxy e r (Routing.route tree r k')
      where
        tree :: Tree (App Proxy)
        tree = compile (waiRoutingSitemap e)

        k' :: Response -> Proxy.Proxy.Proxy ResponseReceived
        k' = liftIO . k

-- | IF YOU MODIFY THIS, BE AWARE OF:
--
-- >>> /libs/wire-api/src/Wire/API/Routes/Public/Proxy.hs
-- >>> https://wearezeta.atlassian.net/browse/SQSERVICES-1647
waiRoutingSitemap :: Env -> Routes a Proxy ()
waiRoutingSitemap e = do
  get
    "/proxy/googlemaps/api/staticmap"
    (proxyWaiPredicate e "key" "secrets.googlemaps" Static "/maps/api/staticmap" (googleMaps e))
    pure

  get
    "/proxy/googlemaps/maps/api/geocode/:path"
    (proxyWaiPredicate e "key" "secrets.googlemaps" Prefix "/maps/api/geocode" (googleMaps e))
    pure

  post "/proxy/spotify/api/token" (continue spotifyToken) request

  get "/proxy/soundcloud/resolve" (continue soundcloudResolve) (query "url")

  get "/proxy/soundcloud/stream" (continue soundcloudStream) (query "url")

googleMaps :: Env -> ProxyDest
googleMaps env = getProxiedEndpoint env googleMapsEndpoint (Endpoint "www.googleapis.com" 443)

giphyH :: Env -> Request -> (Response -> IO ResponseReceived) -> Proxy ResponseReceived
giphyH env = proxyServant "api_key" "secrets.giphy" Prefix "/v1/gifs" phost
  where
    phost = getProxiedEndpoint env giphyEndpoint (Endpoint "api.giphy.com" 443)

youtubeH :: Env -> Request -> (Response -> IO ResponseReceived) -> Proxy ResponseReceived
youtubeH env = proxyServant "key" "secrets.youtube" Prefix "/youtube/v3" phost
  where
    phost = getProxiedEndpoint env youtubeEndpoint (Endpoint "www.googleapis.com" 443)

gmapsStaticH :: Env -> [String] -> Request -> (Response -> IO ResponseReceived) -> Proxy ResponseReceived
gmapsStaticH env [] = (proxyServant "key" "secrets.googlemaps" Static "/maps/api/staticmap") (googleMaps env)
gmapsStaticH _env illegalPaths =
  const . const . liftIO . throwM $
    mkError
      status404
      "not-found"
      ("The path is longer then allowed. Illegal path segments: " <> LText.pack (unwords illegalPaths))

getProxiedEndpoint :: Env -> Getter Opts (Maybe Endpoint) -> Endpoint -> ProxyDest
getProxiedEndpoint env endpointInConfig defaultEndpoint = phost
  where
    phost = ProxyDest (encodeUtf8 endpoint.host) (fromIntegral endpoint.port)
    endpoint = fromMaybe defaultEndpoint (env ^. Proxy.Env.options . endpointInConfig)

proxyServant :: ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> ApplicationM Proxy
proxyServant qparam keyname reroute path phost rq kont = do
  env :: Env <- ask
  liftIO $ do
    assertMethod rq "GET"
    runProxy env $ proxy qparam keyname reroute path phost rq (liftIO . kont)
  where
    assertMethod :: Request -> ByteString -> IO ()
    assertMethod req meth = do
      when (mk (requestMethod req) /= mk meth) $ do
        throwM $ mkError status405 "method-not-allowed" "Method not allowed"

proxyWaiPredicate :: Env -> ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> App Proxy
proxyWaiPredicate env qparam keyname reroute path phost rq k = do
  proxy qparam keyname reroute path phost rq' k'
  where
    rq' :: Request
    rq' = getRequest rq

    k' :: Response -> IO ResponseReceived
    k' = runProxy env . k

proxy :: ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> ApplicationM Proxy
proxy qparam keyname reroute path phost rq k = do
  e :: Env <- ask
  s <- liftIO $ Config.require (e ^. secrets) keyname
  let r = getRequest rq
  let q = renderQuery True ((qparam, Just s) : safeQuery (I.queryString r))
  let r' =
        defaultRequest
          { I.httpVersion = http11,
            I.requestMethod = I.requestMethod r,
            I.rawPathInfo = case reroute of
              Static -> path
              Prefix -> snd $ breakSubstring path (I.rawPathInfo r),
            I.rawQueryString = q
          }
  runInIO <- askRunInIO
  liftIO $ loop e runInIO (2 :: Int) r (waiProxyResponse e r' phost)
  where
    loop :: Env -> (Proxy () -> IO a) -> Int -> Request -> WaiProxyResponse -> IO ResponseReceived
    loop e runInIO !n waiReq req =
      waiProxyTo (const $ pure req) (onUpstreamError runInIO) (e ^. manager) waiReq $ \res ->
        if responseStatus res == status502 && n > 0
          then do
            threadDelay 5000
            loop e runInIO (n - 1) waiReq req
          else appInProxy e waiReq (liftIO $ k res)

    onUpstreamError :: (Proxy () -> IO a) -> SomeException -> p -> (Response -> IO b) -> IO b
    onUpstreamError runInIO x _ next = do
      void . runInIO $ Logger.warn (msg (val "gateway error") ~~ field "error" (show x))
      next (errorRs error502)

    waiProxyResponse :: Env -> Request -> ProxyDest -> WaiProxyResponse
    waiProxyResponse e = case (e ^. Proxy.Env.options . disableTlsForTest) of
      Just True -> WPRModifiedRequest
      _ -> WPRModifiedRequestSecure

spotifyToken :: Request -> Proxy Response
spotifyToken rq = do
  e <- view secrets
  s <- liftIO $ Config.require e "secrets.spotify"
  b <- readBody rq
  let hdr = (hAuthorization, s) : basicHeaders (I.requestHeaders rq)
      req = baseReq {Client.requestHeaders = hdr}
  mgr <- view manager
  res <- liftIO $ recovering x2 [handler] $ const (Client.httpLbs (Req.lbytes b req) mgr)
  when (isError (Client.responseStatus res)) $
    debug $
      msg (val "unexpected upstream response")
        ~~ "upstream"
        .= val "spotify::token"
        ~~ "status"
        .= S (Client.responseStatus res)
        ~~ "body"
        .= B.take 256 (Client.responseBody res)
  pure $
    plain (Client.responseBody res)
      & setStatus (Client.responseStatus res)
        . maybeHeader hContentType res
  where
    baseReq =
      Req.method POST
        . Req.host "accounts.spotify.com"
        . Req.port 443
        . Req.path "/api/token"
        $ Req.empty {Client.secure = True}

soundcloudResolve :: ByteString -> Proxy Response
soundcloudResolve url = do
  e <- view secrets
  s <- liftIO $ Config.require e "secrets.soundcloud"
  let req = Req.queryItem "client_id" s . Req.queryItem "url" url $ baseReq
  mgr <- view manager
  res <- liftIO $ recovering x2 [handler] $ const (Client.httpLbs req mgr)
  when (isError (Client.responseStatus res)) $
    debug $
      msg (val "unexpected upstream response")
        ~~ "upstream"
        .= val "soundcloud::resolve"
        ~~ "status"
        .= S (Client.responseStatus res)
        ~~ "body"
        .= B.take 256 (Client.responseBody res)
  pure $
    plain (Client.responseBody res)
      & setStatus (Client.responseStatus res)
        . maybeHeader hContentType res
  where
    baseReq =
      Req.method GET
        . Req.host "api.soundcloud.com"
        . Req.port 443
        . Req.path "/resolve"
        $ Req.empty {Client.secure = True}

soundcloudStream :: Text -> Proxy Response
soundcloudStream url = do
  e <- view secrets
  s <- liftIO $ Config.require e "secrets.soundcloud"
  req <- Req.noRedirect . Req.queryItem "client_id" s <$> Client.parseRequest (Text.unpack url)
  unless (Client.secure req && Client.host req == "api.soundcloud.com") $
    failWith "insecure stream url"
  mgr <- view manager
  res <- liftIO $ recovering x2 [handler] $ const (Client.httpLbs req mgr)
  unless (status302 == Client.responseStatus res) $ do
    debug $
      msg (val "unexpected upstream response")
        ~~ "upstream"
        .= val "soundcloud::stream"
        ~~ "status"
        .= S (Client.responseStatus res)
        ~~ "body"
        .= B.take 256 (Client.responseBody res)
    failWith "unexpected upstream response"
  case Res.getHeader hLocation res of
    Nothing -> failWith "missing location header"
    Just loc -> pure (empty & setStatus status302 . addHeader hLocation loc)

x2 :: RetryPolicy
x2 = exponentialBackoff 5000 <> limitRetries 2

handler :: (MonadIO m) => RetryStatus -> Handler m Bool
handler = const . Handler $ \case
  Client.HttpExceptionRequest _ Client.NoResponseDataReceived -> pure True
  Client.HttpExceptionRequest _ Client.IncompleteHeaders -> pure True
  Client.HttpExceptionRequest _ Client.ConnectionTimeout -> pure True
  Client.HttpExceptionRequest _ (Client.ConnectionFailure _) -> pure True
  _ -> pure False

safeQuery :: Query -> Query
safeQuery = filter noAccessToken
  where
    noAccessToken (q, _)
      | CI.mk q == accessToken = False
      | otherwise = True

basicHeaders :: RequestHeaders -> RequestHeaders
basicHeaders = filter ((`elem` headers) . fst)
  where
    headers = [hAccept, hContentType]

maybeHeader :: HeaderName -> Client.Response a -> Response -> Response
maybeHeader n r =
  case List.find ((== n) . fst) (Client.responseHeaders r) of
    Nothing -> id
    Just v -> addHeader n (snd v)

accessToken :: CI ByteString
accessToken = CI.mk "access_token"

failWith :: Text -> Proxy a
failWith txt = err (msg txt) >> throwM error500

isError :: Status -> Bool
isError (statusCode -> c) = c < 200 || c > 299

error500 :: Error
error500 = mkError status500 "internal-error" "Internal server error"

error502 :: Error
error502 = mkError status502 "bad-gateway" "Bad gateway"

newtype S = S Status

instance ToBytes S where
  bytes (S s) =
    val "("
      +++ statusCode s
      +++ val ","
      +++ statusMessage s
      +++ val ")"

data Rerouting = Static | Prefix
