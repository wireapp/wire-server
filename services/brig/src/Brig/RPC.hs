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

-- | General RPC utilities.
module Brig.RPC
  ( module Brig.RPC,
    module Brig.RPC.Decode,
  )
where

import Bilge
import Bilge.RPC
import Bilge.Retry
import Brig.App
import Brig.RPC.Decode
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import Data.Id
import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Client (HttpExceptionContent (..), checkResponse)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class hiding (name, (.=))

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

remote :: ByteString -> Msg -> Msg
remote = field "remote"

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
  (MonadReader Env m, MonadIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
cargoholdRequest = serviceRequest "cargohold" cargohold

galleyRequest ::
  (MonadReader Env m, MonadIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
galleyRequest = serviceRequest "galley" galley

gundeckRequest ::
  (MonadReader Env m, MonadIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
gundeckRequest = serviceRequest "gundeck" gundeck

serviceRequest ::
  (MonadReader Env m, MonadIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  LT.Text ->
  Control.Lens.Getting Request Env Request ->
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
serviceRequest nm svc m r = do
  service <- view svc
  recovering x3 rpcHandlers $
    const $
      rpc' nm service (method m . r)
