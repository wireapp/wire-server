{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Federator.Federate where

import Data.Domain (Domain)
import qualified Data.Text as Text
import Federator.Types (Env)
import Imports
import Mu.GRpc.Client.Optics (GRpcReply (..))
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (Field (..), FieldValue (..), FromSchema (..), NP (..), NS (..), Term (TRecord), ToSchema (..))
import Mu.Server (MonadServer, SingleServerT, singleService)
import qualified Mu.Server as Mu
import Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HTTP
import Polysemy

-- * Types

-- These should be extracted out into a library which can be used by components

grpc "Router" id "router.proto"

data Component = Brig
  deriving (Show, Eq, Generic, ToSchema Router "Component", FromSchema Router "Component")

-- | FUTUREWORK: Make this a better ADT for the errors
data Response
  = ResponseOk ByteString
  | ResponseErr Text
  deriving (Show, Eq)

instance ToSchema Router "Response" Response where
  toSchema r =
    let protoChoice = case r of
          (ResponseOk res) -> Z (FPrimitive res)
          (ResponseErr e) -> S (Z (FPrimitive e))
     in TRecord (Field (FUnion protoChoice) :* Nil)

instance FromSchema Router "Response" Response where
  fromSchema (TRecord (Field (FUnion protoChoice) :* Nil)) =
    case protoChoice of
      Z (FPrimitive res) -> ResponseOk res
      S (Z (FPrimitive e)) -> ResponseErr e
      S (S x) ->
        -- I don't understand why this empty case is needed, but there is some
        -- explanation here:
        -- https://github.com/well-typed/generics-sop/issues/116
        case x of

-- Does this make it hard to use in a type checked way?
-- Does this need an HTTP method too? Or should this also be grpc?
data LocalCall = LocalCall
  { component :: Maybe Component,
    method :: Method,
    path :: Text,
    body :: ByteString
  }
  deriving (Eq, Show, Generic, ToSchema Router "LocalCall", FromSchema Router "LocalCall")

data RemoteCall = RemoteCall
  { domain :: Domain,
    localCall :: LocalCall
  }
  deriving (Eq, Show)

instance ToSchema Router "RemoteCall" RemoteCall where
  toSchema _ = undefined

instance FromSchema Router "RemoteCall" RemoteCall where
  fromSchema _ = undefined

-- * Effects

-- | Maybe we should support making a call to many remotes, but maybe we should
-- wait for the requirements before implementing it.
data Remote m a where
  DiscoverAndCall :: RemoteCall -> Remote m (GRpcReply Response)

makeSem ''Remote

-- | Basically this:
-- do
--   g :: GrpcClient <- grpcClient domain
--   liftIO $ gRpcCall @'MsgProtoBuf @RouteToInternal @"RouteToInternal" @"call" g args
--
-- grpcClient :: Monad m => Domain -> m GrpcClient
-- grpcClient = undefined
interpretRemote :: Member (Embed IO) r => Env -> Sem (Remote ': r) a -> Sem r a
interpretRemote _ = interpretH $ \case
  DiscoverAndCall _ -> error "Not implemented"

-- Is there is a point in creating an effect for each service?
--
-- FUTUREWORK: Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.
data Brig m a where
  -- | Returns status and body, 'HTTP.Response' is not nice to work with in tests
  BrigCall :: Method -> Text -> ByteString -> Brig m (HTTP.Status, ByteString)

makeSem ''Brig

interpretBrig :: Member (Embed IO) r => Env -> Sem (Brig ': r) a -> Sem r a
interpretBrig _ = interpret $ \case
  BrigCall {} -> do
    -- embed $ putStrLn "Calling brig"
    error "Not implemeneted"

-- * Routing logic

-- This can be tested now :)
-- TODO: How do we make sure that only legit endpoints can be reached
callRemote :: Member Remote r => RemoteCall -> Sem r Response
callRemote req = do
  reply <- discoverAndCall req
  pure $ mkResponseFromGRPcReply "remote federator -> local federator" reply

-- Look ma, no monads! But we probably want to log things here, so we can add it and test it :)
mkResponseFromGRPcReply :: Text -> GRpcReply Response -> Response
mkResponseFromGRPcReply errName reply =
  case reply of
    GRpcOk res -> res
    GRpcTooMuchConcurrency _ -> ResponseErr (errName <> ": too much concurrency")
    GRpcErrorCode grpcErr -> ResponseErr (errName <> ": " <> Text.pack (show grpcErr))
    GRpcErrorString grpcErr -> ResponseErr (errName <> ": error string: " <> Text.pack grpcErr)
    GRpcClientError clientErr -> ResponseErr (errName <> ": client error: " <> Text.pack (show clientErr))

callLocal :: (Members '[Brig, Embed IO] r) => LocalCall -> Sem r Response
callLocal req = do
  case component req of
    Just Brig -> do
      (resStatus, resBody) <- brigCall (method req) (path req) (body req)
      case HTTP.statusCode resStatus of
        200 -> do
          pure $ ResponseOk resBody
        code -> pure $ ResponseErr ("federator -> brig: unexpected http response code: " <> Text.pack (show code))
    _ -> pure $ ResponseErr "invalid request, missing component"

-- * Server wiring

routeToRemote :: (MonadServer (Sem r), Member Remote r) => SingleServerT info RouteToRemote (Sem r) _
routeToRemote = singleService (Mu.method @"call" callRemote)

routeToInternal :: (MonadServer (Sem r), Members '[Brig, Embed IO] r) => SingleServerT info RouteToInternal (Sem r) _
routeToInternal = singleService (Mu.method @"call" callLocal)

-- Or should this be AppIO?
serveRouteToRemote :: Env -> IO ()
serveRouteToRemote = undefined

serveRouteToInternal :: Env -> IO ()
serveRouteToInternal = undefined
