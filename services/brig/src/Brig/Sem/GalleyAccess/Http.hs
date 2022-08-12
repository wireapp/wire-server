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

module Brig.Sem.GalleyAccess.Http (galleyAccessToHttp) where

import qualified Bilge as RPC
import Bilge.IO
import Bilge.RPC
import Bilge.Request
import Bilge.Retry
import qualified Brig.RPC.Decode as RPC
import Brig.Sem.GalleyAccess
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy as LBS
import Imports
import Network.HTTP.Client (Response)
import Network.HTTP.Types.Method
import Polysemy
import Wire.API.Team.Feature

galleyAccessToHttp ::
  forall m r a.
  ( MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    Member (Embed m) r
  ) =>
  RPC.Request ->
  Sem (GalleyAccess ': r) a ->
  Sem r a
galleyAccessToHttp g =
  interpret $
    embed @m . \case
      GetTeamSndFactorPasswordChallenge tid -> do
        let req =
              paths
                [ "i",
                  "teams",
                  toByteString' tid,
                  "features",
                  featureNameBS @SndFactorPasswordChallengeConfig
                ]
                . expect2xx
        response <- makeReq g GET req
        wsStatus @SndFactorPasswordChallengeConfig
          <$> RPC.decodeBody "galley" response

makeReq ::
  (MonadIO m, MonadMask m, MonadHttp m, HasRequestId m) =>
  RPC.Request ->
  StdMethod ->
  (Request -> Request) ->
  m (Response (Maybe LBS.ByteString))
makeReq galley m r =
  recovering x3 rpcHandlers $
    const $
      rpc' "galley" galley (method m . r)

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
