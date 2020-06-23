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

-- | General RPC utilities.
module Brig.RPC where

import Bilge
import Bilge.RPC
import Bilge.Retry
import Brig.App
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.Aeson
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import Data.Id
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), checkResponse)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class hiding (name, (.=))

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

remote :: ByteString -> Msg -> Msg
remote = field "remote"

decodeBody :: (Typeable a, FromJSON a, MonadThrow m) => Text -> Response (Maybe BL.ByteString) -> m a
decodeBody ctx = responseJsonThrow (ParseException ctx)

expect :: [Status] -> Request -> Request
expect ss rq = rq {checkResponse = check}
  where
    check rq' rs = do
      let s = responseStatus rs
          rs' = rs {responseBody = ()}
      when (statusIsServerError s || s `notElem` ss) $
        throwM $
          HttpExceptionRequest rq' (StatusCodeException rs' mempty)

cargoholdRequest ::
  StdMethod ->
  (Request -> Request) ->
  AppIO (Response (Maybe BL.ByteString))
cargoholdRequest = serviceRequest "cargohold" cargohold

galleyRequest ::
  StdMethod ->
  (Request -> Request) ->
  AppIO (Response (Maybe BL.ByteString))
galleyRequest = serviceRequest "galley" galley

gundeckRequest ::
  StdMethod ->
  (Request -> Request) ->
  AppIO (Response (Maybe BL.ByteString))
gundeckRequest = serviceRequest "gundeck" gundeck

serviceRequest ::
  LT.Text ->
  Control.Lens.Getting Request Env Request ->
  StdMethod ->
  (Request -> Request) ->
  AppIO (Response (Maybe BL.ByteString))
serviceRequest nm svc m r = do
  service <- view svc
  recovering x3 rpcHandlers $
    const $
      rpc' nm service (method m . r)

-- | Failed to parse a response from another service.
data ParseException = ParseException
  { _parseExceptionRemote :: !Text,
    _parseExceptionMsg :: String
  }

instance Show ParseException where
  show (ParseException r m) =
    "Failed to parse response from remote "
      ++ Text.unpack r
      ++ " with message: "
      ++ m

instance Exception ParseException
