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

module Brig.Effects.GalleyAccess.Http (galleyAccessToHttp) where

import qualified Bilge as RPC
import Bilge.IO
import Bilge.RPC
import Bilge.Request
import Brig.Effects.Common
import Brig.Effects.GalleyAccess
import Brig.RPC
import qualified Brig.RPC.Decode as RPC
import Control.Monad.Catch
import Data.Aeson (encode)
import Data.ByteString.Conversion.To
import Galley.Types.Bot
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
    RemoveBotMember zusr conn conv bot -> do
      debug $
        remote "galley"
          . field "user" (toByteString zusr)
          . field "conv" (toByteString conv)
          . field "bot" (toByteString bot)
          . msg (val "Removing bot member")
      embed @m $ do
        let req =
              path "/i/bots"
                . header "Z-User" (toByteString' zusr)
                . maybe id (header "Z-Connection" . toByteString') conn
                . contentJson
                . lbytes (encode (removeBot conv bot))
                . expect [status200, status404] -- 404 is allowed: a given conversation may no longer exist
        response <- makeReq "galley" g DELETE req
        if isJust (RPC.responseBody response) && RPC.statusCode response == 200
          then Just <$> decodeBody "galley" response
          else pure Nothing
