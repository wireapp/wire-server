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
import Data.CaseInsensitive (CI)
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
import Network.Wai.Predicate hiding (Error, err, setStatus)
import Network.Wai.Predicate.Request (getRequest)
import Network.Wai.Routing hiding (path, route)
import Network.Wai.Routing qualified as Routing
import Network.Wai.Utilities
import Network.Wai.Utilities.Server (compile)
import Proxy.Env
import Proxy.Options (disableTlsForTest, giphyEndpoint)
import Proxy.Proxy
import Servant qualified
import System.Logger.Class hiding (Error, info, render)
import System.Logger.Class qualified as Logger

type PublicAPI = Servant.Raw -- see https://wearezeta.atlassian.net/browse/WPB-1216

servantSitemap :: Env -> Servant.ServerT PublicAPI Proxy.Proxy.Proxy
servantSitemap e = Servant.Tagged app
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
    "/proxy/youtube/v3/:path"
    (proxy e "key" "secrets.youtube" Prefix "/youtube/v3" youtube)
    pure

  get
    "/proxy/googlemaps/api/staticmap"
    (proxy e "key" "secrets.googlemaps" Static "/maps/api/staticmap" googleMaps)
    pure

  get
    "/proxy/googlemaps/maps/api/geocode/:path"
    (proxy e "key" "secrets.googlemaps" Prefix "/maps/api/geocode" googleMaps)
    pure

  get
    "/proxy/giphy/v1/gifs/:path"
    (proxy e "api_key" "secrets.giphy" Prefix "/v1/gifs" (giphy e))
    pure

  post "/proxy/spotify/api/token" (continue spotifyToken) request

  get "/proxy/soundcloud/resolve" (continue soundcloudResolve) (query "url")

  get "/proxy/soundcloud/stream" (continue soundcloudStream) (query "url")

youtube, googleMaps :: ProxyDest
youtube = ProxyDest "www.googleapis.com" 443
googleMaps = ProxyDest "maps.googleapis.com" 443

giphy :: Env -> ProxyDest
giphy ((^. Proxy.Env.options . giphyEndpoint) -> endpoint) =
  ProxyDest (encodeUtf8 endpoint.host) (fromIntegral endpoint.port)

proxy :: Env -> ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> App Proxy
proxy e qparam keyname reroute path phost rq k = do
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
  liftIO $ loop runInIO (2 :: Int) r (waiProxyResponse r' phost)
  where
    loop runInIO !n waiReq req =
      waiProxyTo (const $ pure req) (onUpstreamError runInIO) (e ^. manager) waiReq $ \res ->
        if responseStatus res == status502 && n > 0
          then do
            threadDelay 5000
            loop runInIO (n - 1) waiReq req
          else appInProxy e waiReq (k res)

    onUpstreamError runInIO x _ next = do
      void . runInIO $ Logger.warn (msg (val "gateway error") ~~ field "error" (show x))
      next (errorRs error502)

    waiProxyResponse :: Request -> ProxyDest -> WaiProxyResponse
    waiProxyResponse = case (e ^. Proxy.Env.options . disableTlsForTest) of
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
