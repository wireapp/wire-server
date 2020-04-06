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

module Brig.Whitelist
  ( Whitelist (..),
    verify,
  )
where

import Bilge.IO
import Bilge.Request
import Bilge.Response
import Bilge.Retry
import Brig.Types
import Control.Monad.Catch (MonadMask, throwM)
import Control.Retry
import Data.Aeson
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Imports
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), parseRequest)

-- | A service providing a whitelist of allowed email addresses and phone numbers
data Whitelist
  = Whitelist
      { -- | Service URL
        whitelistUrl :: !Text,
        -- | Username
        whitelistUser :: !Text,
        -- | Password
        whitelistPass :: !Text
      }
  deriving (Show, Generic)

instance FromJSON Whitelist

-- | Do a request to the whitelist service and verify that the provided email/phone address is
-- whitelisted.
verify :: (MonadIO m, MonadMask m, MonadHttp m) => Whitelist -> Either Email Phone -> m Bool
verify (Whitelist url user pass) key =
  if isKnownDomain key
    then return True
    else recovering x3 httpHandlers . const $ do
      rq <- parseRequest $ unpack url
      rsp <- get' rq $ req (encodeUtf8 user) (encodeUtf8 pass)
      case statusCode rsp of
        200 -> return True
        404 -> return False
        _ ->
          throwM $
            HttpExceptionRequest rq (StatusCodeException (rsp {responseBody = ()}) mempty)
  where
    isKnownDomain (Left e) = emailDomain e == "wire.com"
    isKnownDomain _ = False
    urlEmail = queryItem "email" . encodeUtf8 . fromEmail
    urlPhone = queryItem "mobile" . encodeUtf8 . fromPhone
    req u p =
      port 443 . secure
        . either urlEmail urlPhone key
        . applyBasicAuth u p
    x3 = limitRetries 3 <> exponentialBackoff 100000
