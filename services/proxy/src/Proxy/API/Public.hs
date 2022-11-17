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
  ( servantSitemap,
  )
where

import qualified Bilge.Request as Req
import qualified Bilge.Response as Res
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry
import Data.ByteString (breakSubstring)
import qualified Data.ByteString.Lazy as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as Config
import qualified Data.List as List
import qualified Data.Text as Text
import Imports hiding (head)
import qualified Network.HTTP.Client as Client
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Internal as I
import Network.Wai.Predicate hiding (Error, err, setStatus)
import Network.Wai.Predicate.Request (getRequest)
import Network.Wai.Routing hiding (path, route)
import Network.Wai.Utilities
import Proxy.Env
import Proxy.Proxy
import Servant (ServerT)
import Servant.API ((:<|>) ((:<|>)))
import Servant.API.Extended.RawM (ApplicationM)
import System.Logger.Class hiding (Error, info, render)
import qualified System.Logger.Class as Logger
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Proxy (ProxyAPI)

servantSitemap :: ServerT ProxyAPI Proxy
servantSitemap =
  Named @"youtube-path" (proxy "key" "secrets.youtube" Prefix "/youtube/v3" youtube)
    :<|> Named @"gmaps-static" (proxy "key" "secrets.googlemaps" Static "/maps/api/staticmap" googleMaps)
    :<|> Named @"gmaps-path" (proxy "key" "secrets.googlemaps" Prefix "/maps/api/geocode" googleMaps)
    :<|> Named @"giphy-path" (proxy "api_key" "secrets.giphy" Prefix "/v1/gifs" giphy)
    :<|> Named @"spotify-token" undefined --  post "/proxy/spotify/api/token" (continue spotifyToken) request
    :<|> Named @"soundcloud-resolve" undefined -- get "/proxy/soundcloud/resolve" (continue soundcloudResolve) (query "url")
    :<|> Named @"soundcloud-stream" undefined -- get "/proxy/soundcloud/stream" (continue soundcloudStream) (query "url")

youtube, googleMaps, giphy :: ProxyDest
youtube = ProxyDest "www.googleapis.com" 443
googleMaps = ProxyDest "maps.googleapis.com" 443
giphy = ProxyDest "api.giphy.com" 443

proxy :: ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> ApplicationM Proxy
proxy qparam keyname reroute path phost rq kont = do
  env :: Env <- ask
  liftIO $ proxyIO env qparam keyname reroute path phost rq kont

proxyIO :: Env -> ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> Application
proxyIO env qparam keyname reroute path phost rq kont = do
  s <- Config.require (env ^. secrets) keyname
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
  loop (2 :: Int) env r (WPRModifiedRequestSecure r' phost)
  where
    loop :: Int -> Env -> Request -> WaiProxyResponse -> IO ResponseReceived
    loop !n env waiReq req =
      waiProxyTo (const $ pure req) onUpstreamError (env ^. manager) waiReq $ \res ->
        if responseStatus res == status502 && n > 0
          then do
            threadDelay 5000
            loop (n - 1) env waiReq req
          else kont res

    onUpstreamError :: SomeException -> ignored -> (Response -> IO a) -> IO a
    onUpstreamError x _ next = do
      void . runProxy env $ Logger.warn (msg (val "gateway error") ~~ field "error" (show x))
      next (errorRs' error502)

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
        ~~ "upstream" .= val "spotify::token"
        ~~ "status" .= S (Client.responseStatus res)
        ~~ "body" .= B.take 256 (Client.responseBody res)
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
        ~~ "upstream" .= val "soundcloud::resolve"
        ~~ "status" .= S (Client.responseStatus res)
        ~~ "body" .= B.take 256 (Client.responseBody res)
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
        ~~ "upstream" .= val "soundcloud::stream"
        ~~ "status" .= S (Client.responseStatus res)
        ~~ "body" .= B.take 256 (Client.responseBody res)
    failWith "unexpected upstream response"
  case Res.getHeader hLocation res of
    Nothing -> failWith "missing location header"
    Just loc -> pure (empty & setStatus status302 . addHeader hLocation loc)

x2 :: RetryPolicy
x2 = exponentialBackoff 5000 <> limitRetries 2

handler :: (MonadIO m, MonadMask m) => RetryStatus -> Handler m Bool
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
