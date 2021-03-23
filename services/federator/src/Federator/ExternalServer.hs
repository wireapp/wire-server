{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

module Federator.ExternalServer where

import qualified Data.ByteString.Lazy as LBS
import Federator.App (Federator, runAppT)
import Federator.Brig (Brig, brigCall, interpretBrig)
import Federator.Env (Env)
import Federator.Utils.PolysemyServerError (absorbServerError)
import Imports
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT, singleService)
import qualified Mu.Server as Mu
import qualified Network.HTTP.Types as HTTP
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import Wire.API.Federation.GRPC.Types

-- FUTUREWORK(federation): Versioning of the federation API. See
-- https://higherkindness.io/mu-haskell/registry/ for some mu-haskell support
-- for versioning schemas here.

-- FUTUREWORK(federation): How do we make sure that only legitimate endpoints can be
-- reached, some discussion here:
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/224166764/Limiting+access+to+federation+endpoints
-- Also, see comment in 'Federator.Brig.interpretBrig'
callLocal :: (Members '[Brig, Embed IO] r) => Request -> Sem r InwardResponse
callLocal Request {..} = do
  -- FUTUREWORK(federation): before making a request, check the sender domain and only make the call if the allowlist (use Util.federateWith) allows it.
  (resStatus, resBody) <- brigCall (unwrapMethod method) path query body
  -- FUTUREWORK(federation): Decide what to do with 5xx statuses
  let statusW32 = fromIntegral $ HTTP.statusCode resStatus
      bodyBS = maybe mempty LBS.toStrict resBody
  pure $ InwardResponseHTTPResponse $ HTTPResponse statusW32 bodyBS

routeToInternal :: (Members '[Brig, Embed IO, Polysemy.Error ServerError] r) => SingleServerT info Inward (Sem r) _
routeToInternal = singleService (Mu.method @"call" callLocal)

serveInward :: Env -> Int -> IO ()
serveInward env port = do
  runGRpcAppTrans msgProtoBuf port transformer routeToInternal
  where
    transformer :: Sem '[Embed IO, Polysemy.Error ServerError, Brig, Embed Federator] a -> ServerErrorIO a
    transformer action =
      runAppT env
        . runM @Federator
        . interpretBrig
        . absorbServerError
        . embedToMonadIO @Federator
        $ action
