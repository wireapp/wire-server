{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Federator.ExternalServer where

import qualified Data.ByteString.Lazy as LBS
import Federator.App (Federator, runAppT)
import Federator.Brig (Brig, brigCall, interpretBrig)
import Federator.Types (Env)
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

-- FUTUREWORK: How do we make sure that only legitimate endpoints can be
-- reached, some discussion here:
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/224166764/Limiting+access+to+federation+endpoints
callLocal :: (Members '[Brig, Embed IO] r) => LocalCall -> Sem r Response
callLocal LocalCall {..} = do
  (resStatus, resBody) <- brigCall (unwrapMethod method) path query body
  -- FUTUREWORK: Decide what to do with 5xx statuses
  let statusW32 = fromIntegral $ HTTP.statusCode resStatus
      bodyBS = maybe mempty LBS.toStrict resBody
  pure $ ResponseHTTPResponse $ HTTPResponse statusW32 bodyBS

routeToInternal :: (Members '[Brig, Embed IO, Polysemy.Error ServerError] r) => SingleServerT info RouteToInternal (Sem r) _
routeToInternal = singleService (Mu.method @"call" callLocal)

serveRouteToInternal :: Env -> Int -> IO ()
serveRouteToInternal env port = do
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
