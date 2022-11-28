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

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry
import Data.ByteString (breakSubstring)
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
  Named @"giphy-path" (proxy "api_key" "secrets.giphy" Prefix "/v1/gifs" giphy)
    :<|> Named @"youtube-path" (proxy "key" "secrets.youtube" Prefix "/youtube/v3" youtube)
    :<|> Named @"gmaps-static" (proxy "key" "secrets.googlemaps" Static "/maps/api/staticmap" googleMaps)
    :<|> Named @"gmaps-path" (proxy "key" "secrets.googlemaps" Prefix "/maps/api/geocode" googleMaps)

giphy, youtube, googleMaps :: ProxyDest
giphy = ProxyDest "api.giphy.com" 443
youtube = ProxyDest "www.googleapis.com" 443
googleMaps = ProxyDest "maps.googleapis.com" 443

proxy :: ByteString -> Text -> Rerouting -> ByteString -> ProxyDest -> ApplicationM Proxy
proxy qparam keyname reroute path phost rq kont = do
  env :: Env <- ask
  liftIO $ do
    assertMethod rq "GET"
    proxyIO env qparam keyname reroute path phost rq kont

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

safeQuery :: Query -> Query
safeQuery = filter noAccessToken
  where
    noAccessToken (q, _)
      | CI.mk q == accessToken = False
      | otherwise = True

accessToken :: CI ByteString
accessToken = CI.mk "access_token"

error405 :: Error
error405 = mkError status405 "method-not-allowed" "Method not allowed"

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
  when (mk (requestMethod req) /= mk meth) $ do
    throwM error405
