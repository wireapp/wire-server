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

module Proxy.API.Public (servantSitemap) where

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
import Imports hiding (head)
import Network.HTTP.Client qualified as Client
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal qualified as I
import Network.Wai.Utilities
import Proxy.Env
import Proxy.Options (Opts, disableTlsForTest, giphyEndpoint, googleMapsEndpoint, soundcloudEndpoint, spotifyEndpoint, youtubeEndpoint)
import Proxy.Proxy
import Servant ((:<|>) (..))
import Servant qualified
import System.Logger.Class hiding (Error, info, render)
import System.Logger.Class qualified as Logger
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Proxy

type ApplicationM m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

-- TODO: test that the switch from wai-route to servant doesn't break streaming
-- characteristics (we don't want to turn O(1) memory requirement into O(n))

servantSitemap :: Env -> Servant.ServerT ProxyAPI Proxy.Proxy.Proxy
servantSitemap e =
  Named @"giphy-path" (giphyH e)
    :<|> Named @"youtube-path" (youtubeH e)
    :<|> Named @"gmaps-static" (gmapsStaticH e)
    :<|> Named @"gmaps-path" (gmapsPathH e)
    :<|> Named @"spotify" (spotifyH e)
    :<|> Named @"soundcloud-resolve" (soundcloudResolveH e)
    :<|> Named @"soundcloud-stream" (soundcloudStreamH e)

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

gmapsStaticH :: Env -> Request -> (Response -> IO ResponseReceived) -> Proxy ResponseReceived
gmapsStaticH env = (proxyServant "key" "secrets.googlemaps" Static "/maps/api/staticmap") (googleMaps env)

gmapsPathH :: Env -> Request -> (Response -> IO ResponseReceived) -> Proxy ResponseReceived
gmapsPathH env = (proxyServant "key" "secrets.googlemaps" Prefix "/maps/api/geocode") (googleMaps env)

spotifyH :: Env -> ApplicationM Proxy
spotifyH env req kont = spotifyToken env req >>= liftIO . kont

soundcloudResolveH :: Env -> Text -> ApplicationM Proxy
soundcloudResolveH env url _req kont = soundcloudResolve env (encodeUtf8 url) >>= liftIO . kont

soundcloudStreamH :: Env -> Text -> ApplicationM Proxy
soundcloudStreamH env url _req kont = soundcloudStream env url >>= liftIO . kont

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

proxy :: ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> ApplicationM Proxy
proxy qparam keyname reroute path phost rq k = do
  e :: Env <- ask
  s <- liftIO $ Config.require (e ^. secrets) keyname
  let q = renderQuery True ((qparam, Just s) : safeQuery (I.queryString rq))
  let r' =
        defaultRequest
          { I.httpVersion = http11,
            I.requestMethod = I.requestMethod rq,
            I.rawPathInfo = case reroute of
              Static -> path
              Prefix -> snd $ breakSubstring path (I.rawPathInfo rq),
            I.rawQueryString = q
          }
  runInIO <- askRunInIO
  liftIO $ loop e runInIO (2 :: Int) rq (waiProxyResponse e r' phost)
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

spotifyToken :: Env -> Request -> Proxy Response
spotifyToken env rq = do
  s <- liftIO $ Config.require (env ^. secrets) "secrets.spotify"
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
        . Req.host (encodeUtf8 endpoint.host)
        . Req.port endpoint.port
        . Req.path "/api/token"
        $ Req.empty {Client.secure = maybe True not (env ^. Proxy.Env.options . disableTlsForTest)}

    endpoint = fromMaybe (Endpoint "accounts.spotify.com" 443) (env ^. Proxy.Env.options . spotifyEndpoint)

soundcloudResolve :: Env -> ByteString -> Proxy Response
soundcloudResolve env url = do
  s <- liftIO $ Config.require (env ^. secrets) "secrets.soundcloud"
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
        . Req.host (encodeUtf8 endpoint.host)
        . Req.port endpoint.port
        . Req.path "/resolve"
        $ Req.empty {Client.secure = maybe True not (env ^. Proxy.Env.options . disableTlsForTest)}

    endpoint = fromMaybe (Endpoint "api.soundcloud.com" 443) (env ^. Proxy.Env.options . soundcloudEndpoint)

soundcloudStream :: Env -> Text -> Proxy Response
soundcloudStream env url = do
  s <- liftIO $ Config.require (env ^. secrets) "secrets.soundcloud"
  req <- Req.noRedirect . Req.queryItem "client_id" s <$> Client.parseRequest (Text.unpack url)
  unless (checkHttps req && Client.host req == encodeUtf8 endpoint.host) $
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
  where
    endpoint = fromMaybe (Endpoint "api.soundcloud.com" 443) (env ^. Proxy.Env.options . soundcloudEndpoint)

    checkHttps req = case env ^. Proxy.Env.options . disableTlsForTest of
      Just True -> True
      _ -> Client.secure req

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
