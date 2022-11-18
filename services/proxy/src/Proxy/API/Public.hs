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
import Data.CaseInsensitive (CI, mk)
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as Config
import qualified Data.List as List
import Data.String.Conversions (cs)
import Imports hiding (head)
import qualified Network.HTTP.Client as Client
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Internal as I
import Network.Wai.Predicate.Request (getRequest)
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
    :<|> Named @"spotify-token" spotifyToken
    :<|> Named @"soundcloud-resolve" soundcloudResolve
    :<|> Named @"soundcloud-stream" soundcloudStream

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
  loop (2 :: Int) r (WPRModifiedRequestSecure r' phost)
  where
    loop :: Int -> Request -> WaiProxyResponse -> IO ResponseReceived
    loop !n waiReq req =
      waiProxyTo (const $ pure req) onUpstreamError (env ^. manager) waiReq $ \res ->
        if responseStatus res == status502 && n > 0
          then do
            threadDelay 5000
            loop (n - 1) waiReq req
          else kont res

    onUpstreamError :: SomeException -> ignored -> (Response -> IO a) -> IO a
    onUpstreamError x _ next = do
      void . runProxy env $ Logger.warn (msg (val "gateway error") ~~ field "error" (show x))
      next (errorRs' error502)

spotifyToken :: ApplicationM Proxy
spotifyToken req kont = do
  env <- ask
  liftIO $ do
    assertMethod req "POST"
    kont =<< spotifyTokenIO env req

spotifyTokenIO :: Env -> Request -> IO Response
spotifyTokenIO env rq = do
  s <- Config.require (env ^. secrets) "secrets.spotify"
  b <- readBody rq
  let hdr = (hAuthorization, s) : basicHeaders (I.requestHeaders rq)
      req = baseReq {Client.requestHeaders = hdr}
  let mgr = env ^. manager
  res <- recovering x2 [handler] $ const (Client.httpLbs (Req.lbytes b req) mgr)
  when (isError (Client.responseStatus res)) $
    runProxy env . debug $
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

-- get "/proxy/soundcloud/resolve" (continue soundcloudResolve) (query "url")
soundcloudResolve :: ApplicationM Proxy
soundcloudResolve req kont = do
  env <- ask
  liftIO $ do
    url <- lookupQueryParam "url" req
    assertMethod req "GET"
    kont =<< soundcloudResolveIO env url

soundcloudResolveIO :: Env -> Text -> IO Response
soundcloudResolveIO env url = do
  s <- liftIO $ Config.require (env ^. secrets) "secrets.soundcloud"
  let req = Req.queryItem "client_id" s . Req.queryItem "url" (cs url) $ baseReq
  let mgr = env ^. manager
  res <- liftIO $ recovering x2 [handler] $ const (Client.httpLbs req mgr)
  when (isError (Client.responseStatus res)) $
    runProxy env . debug $
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

soundcloudStream :: ApplicationM Proxy
soundcloudStream req kont = do
  env <- ask
  liftIO $ do
    url <- lookupQueryParam "url" req
    assertMethod req "GET"
    kont =<< soundcloudStreamIO env url

soundcloudStreamIO :: Env -> Text -> IO Response
soundcloudStreamIO env url = do
  s <- liftIO $ Config.require (env ^. secrets) "secrets.soundcloud"
  req <- Req.noRedirect . Req.queryItem "client_id" s <$> Client.parseRequest (cs url)
  unless (Client.secure req && Client.host req == "api.soundcloud.com") $ do
    runProxy env $ failWith "insecure stream url"
  let mgr = env ^. manager
  res <- liftIO $ recovering x2 [handler] $ const (Client.httpLbs req mgr)
  unless (status302 == Client.responseStatus res) $ do
    runProxy env . debug $
      msg (val "unexpected upstream response")
        ~~ "upstream" .= val "soundcloud::stream"
        ~~ "status" .= S (Client.responseStatus res)
        ~~ "body" .= B.take 256 (Client.responseBody res)
    runProxy env $ failWith "unexpected upstream response"
  case Res.getHeader hLocation res of
    Nothing -> runProxy env $ failWith "missing location header"
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

error400 :: ByteString -> Error
error400 param = mkError status400 "missing-query-param" ("Bad request: " <> cs param <> " query param is missing")

error405 :: Error
error405 = mkError status405 "method-not-allowed" "Method not allowed"

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

assertMethod :: Request -> ByteString -> IO ()
assertMethod req meth = do
  when (requestMethod req /= meth) $ do
    throwM error405

lookupQueryParam :: ByteString -> Request -> IO Text
lookupQueryParam key req = do
  lookup (mk key) (requestHeaders req)
    & maybe
      (throwM $ error400 key)
      (pure . cs)
