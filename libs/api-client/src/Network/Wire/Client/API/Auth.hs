{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Network.Wire.Client.API.Auth
  ( login,
    refreshAuth,
    Auth (..),
    AuthCookie,
    token,
    module Auth,
  )
where

import Bilge
import Brig.Types.User.Auth as Auth hiding (Cookie, user)
import Control.Monad.Catch (MonadMask)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as Lazy
import Data.List.NonEmpty
import Data.Text (pack)
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import Imports
import Network.HTTP.Client (generateCookie)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import Web.Cookie (parseSetCookie)

newtype AuthCookie = AuthCookie Cookie

data Auth
  = Auth
      { authCookie :: !AuthCookie,
        authToken :: !AccessToken
      }

-------------------------------------------------------------------------------
-- Unauthenticated

login :: (MonadClient m, MonadUnliftIO m, MonadMask m) => Login -> m (Maybe Auth)
login l = do
  rs <- clientRequest req rsc consumeBody
  sv <- getServer
  liftIO $ tokenResponse (setServer sv req) rs Nothing
  where
    req =
      method POST
        . path "/login"
        . acceptJson
        . json l
        $ empty
    rsc = status200 :| [status403]

-------------------------------------------------------------------------------
-- Authenticated

refreshAuth :: (MonadClient m, MonadMask m, MonadUnliftIO m) => Auth -> m (Maybe Auth)
refreshAuth (Auth ac@(AuthCookie c) t) = do
  sv <- getServer
  rs <- clientRequest (req sv) rsc consumeBody
  liftIO $ tokenResponse (setServer sv $ req sv) rs (Just ac)
  where
    req s =
      method POST
        . path "/access"
        . acceptJson
        . setCookie s
        . token t
        $ empty
    rsc = status200 :| [status403]
    setCookie sv
      | serverSSL sv = cookie c
      | otherwise = header "Cookie" (cookie_name c <> "=" <> cookie_value c)

-------------------------------------------------------------------------------
-- Utilities

token :: AccessToken -> Request -> Request
token t = header "Authorization" (tokType <> " " <> tokValue)
  where
    tokType = C.pack (show (tokenType t))
    tokValue = Lazy.toStrict (access t)

-- | Construct an 'Auth'orisation out of an access token response.
tokenResponse ::
  -- | The associated request (for cookie verification).
  Request ->
  -- | The access token response.
  Response (Maybe LByteString) ->
  -- | The cookie to use if none is provided in the response.
  Maybe AuthCookie ->
  IO (Maybe Auth)
tokenResponse rq rs ck
  | statusCode rs == 200 = mkAuth
  | statusCode rs == 403 = return Nothing
  | otherwise = unexpected rs "tokenResponse: status code"
  where
    mkAuth = do
      cok <- mkCookie $ parseSetCookie <$> getHeader "Set-Cookie" rs
      tok <- responseJsonThrow (ParseError . pack) rs
      return . Just $ Auth cok tok
    mkCookie Nothing = maybe (unexpected rs "missing set-cookie") return ck
    mkCookie (Just hdr) = do
      now <- getCurrentTime
      case generateCookie hdr rq now True of
        Just cok | cookie_name cok == "zuid" -> return $ AuthCookie cok
        Just (cookie_name -> cok) -> unexpected rs $ "unknown cookie: " <> T.decodeLatin1 cok
        Nothing -> unexpected rs "invalid cookie"
