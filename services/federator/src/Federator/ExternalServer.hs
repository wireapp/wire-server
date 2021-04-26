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

import Control.Lens (view)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Federator.App (Federator, runAppT)
import Federator.Brig (Brig, brigCall, interpretBrig)
import Federator.Env (Env, applog, runSettings)
import Federator.Options (RunSettings)
import Federator.Utils.PolysemyServerError (absorbServerError)
import Federator.Validation
import Imports
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT, singleService)
import qualified Mu.Server as Mu
import qualified Network.HTTP.Types.Status as HTTP
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import Wire.API.Federation.GRPC.Types

-- FUTUREWORK(federation): Versioning of the federation API. See
-- https://higherkindness.io/mu-haskell/registry/ for some mu-haskell support
-- for versioning schemas here.

-- FUTUREWORK(federation): How do we make sure that only legitimate endpoints can be
-- reached, some discussion here:
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/224166764/Limiting+access+to+federation+endpoints
-- Also, see comment in 'Federator.Brig.interpretBrig'
callLocal :: (Members '[Brig, Embed IO, TinyLog, Polysemy.Reader RunSettings] r) => Request -> Sem r InwardResponse
callLocal req@Request {..} = do
  Log.debug $
    Log.msg ("Inward Request" :: ByteString)
      . Log.field "request" (show req)

  -- FUTUREWORK: implement server2server authentication!
  -- (current validation only checks parsing and compares to allowList)
  -- FUTUREWORK: would it make sense to have validations throw errors to reduce nesting and improve readability?
  validation <- federateWith' originDomain

  case validation of
    Left err -> pure $ InwardResponseErr err
    Right domain -> do
      (resStatus, resBody) <- brigCall path body domain
      pure $ case HTTP.statusCode resStatus of
        200 -> InwardResponseBody $ maybe mempty LBS.toStrict resBody
        -- TODO: There is a unit test for this, but Akshay has seen the integration
        -- test never sees InwardResponseErr, when the error is supposed to be
        -- returned, the integration test just sees `InwardResponseBody` with empty
        -- body. Maybe this is a bug in mu-haskell, maybe something is wrong with
        -- our integration test, let's verify this.
        code -> InwardResponseErr $ "Invalid HTTP status from component: " <> Text.pack (show code) <> " " <> Text.decodeUtf8 (HTTP.statusMessage resStatus)

routeToInternal :: (Members '[Brig, Embed IO, Polysemy.Error ServerError, TinyLog, Polysemy.Reader RunSettings] r) => SingleServerT info Inward (Sem r) _
routeToInternal = singleService (Mu.method @"call" callLocal)

serveInward :: Env -> Int -> IO ()
serveInward env port = do
  runGRpcAppTrans msgProtoBuf port transformer routeToInternal
  where
    transformer :: Sem '[TinyLog, Embed IO, Polysemy.Error ServerError, Brig, Polysemy.Reader RunSettings, Embed Federator] a -> ServerErrorIO a
    transformer action =
      runAppT env
        . runM @Federator
        . Polysemy.runReader (view runSettings env)
        . interpretBrig
        . absorbServerError
        . embedToMonadIO @Federator
        . Log.runTinyLog (view applog env)
        $ action
