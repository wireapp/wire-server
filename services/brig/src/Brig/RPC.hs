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
module Brig.RPC where

import Bilge
import Bilge.RPC
import Bilge.Retry
import Brig.App
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types.Method
import System.Logger.Class hiding (name, (.=))
import Wire.ParseException
import Wire.Rpc (x3)

remote :: ByteString -> Msg -> Msg
remote = field "remote"

decodeBody :: (Typeable a, FromJSON a, MonadThrow m) => Text -> Response (Maybe BL.ByteString) -> m a
decodeBody ctx = responseJsonThrow (ParseException ctx)

cargoholdRequest ::
  (MonadReader Env m, MonadUnliftIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
cargoholdRequest = serviceRequest "cargohold" cargohold

galleyRequest ::
  (MonadReader Env m, MonadUnliftIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
galleyRequest = serviceRequest "galley" galley

serviceRequest ::
  (MonadReader Env m, MonadUnliftIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  LT.Text ->
  Control.Lens.Getting Request Env Request ->
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
serviceRequest nm svc m r = do
  service <- view svc
  serviceRequestImpl nm service m r

serviceRequestImpl ::
  (MonadUnliftIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  LT.Text ->
  Request ->
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe BL.ByteString))
serviceRequestImpl nm service m r = do
  recovering x3 rpcHandlers $
    const $
      rpc' nm service (method m . r)
