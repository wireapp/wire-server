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
import Brig.RPC
import qualified Brig.RPC.Decode as RPC
import Brig.Sem.Common
import Brig.Sem.GalleyAccess
import Control.Monad.Catch
import Data.ByteString.Conversion.To
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Polysemy
import Polysemy.TinyLog
import System.Logger.Class hiding (debug)
import Wire.API.Team.Feature

galleyAccessToHttp ::
  forall m r a.
  Member TinyLog r =>
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
  interpret $ \case
    GetTeamSndFactorPasswordChallenge tid -> embed @m $ do
      let req =
            paths
              [ "i",
                "teams",
                toByteString' tid,
                "features",
                featureNameBS @SndFactorPasswordChallengeConfig
              ]
              . expect2xx
      response <- makeReq "galley" g GET req
      wsStatus @SndFactorPasswordChallengeConfig
        <$> RPC.decodeBody "galley" response
    GetTeamContacts uid -> do
      debug $ remote "galley" . msg (val "Get team contacts")
      embed @m $ do
        let req =
              paths ["i", "users", toByteString' uid, "team", "members"]
                . expect [status200, status404]
        response <- makeReq "galley" g GET req
        case RPC.statusCode response of
          200 -> Just <$> decodeBody "galley" response
          _ -> pure Nothing
    GetTeamId uid -> do
      debug $ remote "galley" . msg (val "Get team from user")
      embed @m $ do
        let req =
              paths ["i", "users", toByteString' uid, "team"]
                . expect [status200, status404]
        response <- makeReq "galley" g GET req
        case RPC.statusCode response of
          200 -> Just <$> decodeBody "galley" response
          _ -> pure Nothing
    GetTeamLegalHoldStatus tid -> do
      debug $ remote "galley" . msg (val "Get legalhold settings")
      embed @m $ do
        let req =
              paths
                [ "i",
                  "teams",
                  toByteString' tid,
                  "features",
                  featureNameBS @LegalholdConfig
                ]
                . expect2xx
        makeReq "galley" g GET req >>= decodeBody "galley"
