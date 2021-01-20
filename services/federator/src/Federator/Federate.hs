{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Federator.Federate where

import qualified Bilge as RPC
import Bilge.RPC (rpc')
import Bilge.Retry
import Control.Lens (view)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Except (MonadError (throwError))
import Control.Retry
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain)
import Data.Either.Validation
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Federator.App (AppIO, AppT, runAppT)
import Federator.PolysemyOrphans ()
import Federator.Types (Env, brig)
import Federator.UnliftExcept ()
import Imports
import Mu.GRpc.Client.Optics (GRpcReply (..))
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (Field (..), FieldValue (..), FromSchema (..), NP (..), NS (..), Term (..), ToSchema (..))
import Mu.Server (MonadServer, ServerError, ServerErrorIO, SingleServerT, singleService)
import qualified Mu.Server as Mu
import qualified Network.HTTP.Types as HTTP
import Polysemy
import Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import Test.QuickCheck (elements)
import Wire.API.Arbitrary

-- * Types

-- These should be extracted out into a library which can be used by components

grpc "Router" id "router.proto"

data Component
  = UnspecifiedComponent
  | Brig
  deriving (Show, Eq, Generic, ToSchema Router "Component", FromSchema Router "Component")
  deriving (Arbitrary) via (GenericUniform Component)

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

-- | This type exists to avoid orphan instances of ToSchema and FromSchema
newtype HTTPMethod = HTTPMethod {unwrapMethod :: HTTP.StdMethod}
  deriving (Eq, Show, Generic)

instance Arbitrary HTTPMethod where
  arbitrary =
    HTTPMethod
      <$> elements
        [ HTTP.GET,
          HTTP.POST,
          HTTP.HEAD,
          HTTP.PUT,
          HTTP.DELETE,
          HTTP.TRACE,
          HTTP.CONNECT,
          HTTP.OPTIONS,
          HTTP.PATCH
        ]

-- TODO: Write roundtrip tests
-- TODO: The instances seem to be wrong, at least they don't work with grpcui
instance ToSchema Router "Method" HTTPMethod where
  toSchema (HTTPMethod m) =
    let enumChoice = case m of
          HTTP.GET -> S (Z Proxy)
          HTTP.POST -> S (S (Z Proxy))
          HTTP.HEAD -> S (S (S (Z Proxy)))
          HTTP.PUT -> S (S (S (S (Z Proxy))))
          HTTP.DELETE -> S (S (S (S (S (Z Proxy)))))
          HTTP.TRACE -> S (S (S (S (S (S (Z Proxy))))))
          HTTP.CONNECT -> S (S (S (S (S (S (S (Z Proxy)))))))
          HTTP.OPTIONS -> S (S (S (S (S (S (S (S (Z Proxy))))))))
          HTTP.PATCH -> S (S (S (S (S (S (S (S (S (Z Proxy)))))))))
     in TEnum enumChoice

instance FromSchema Router "Method" HTTPMethod where
  fromSchema (TEnum enumChoice) =
    let m = case enumChoice of
          Z _ ->
            -- This is due to this bug
            -- https://github.com/higherkindness/mu-haskell/issues/282
            error "router:method:unspecified is impossible"
          S (Z _) -> HTTP.GET
          S (S (Z _)) -> HTTP.POST
          S (S (S (Z _))) -> HTTP.HEAD
          S (S (S (S (Z _)))) -> HTTP.PUT
          S (S (S (S (S (Z _))))) -> HTTP.DELETE
          S (S (S (S (S (S (Z _)))))) -> HTTP.TRACE
          S (S (S (S (S (S (S (Z _))))))) -> HTTP.CONNECT
          S (S (S (S (S (S (S (S (Z _)))))))) -> HTTP.OPTIONS
          S (S (S (S (S (S (S (S (S (Z _))))))))) -> HTTP.PATCH
          S (S (S (S (S (S (S (S (S (S x))))))))) -> case x of
     in HTTPMethod m

data QueryParam = QueryParam
  { key :: ByteString,
    value :: ByteString
  }
  deriving (Eq, Show, Generic, ToSchema Router "QueryParam", FromSchema Router "QueryParam")
  deriving (Arbitrary) via (GenericUniform QueryParam)

-- Does this make it hard to use in a type checked way?
-- Does this need an HTTP method too? Or should this also be grpc?
data LocalCall = LocalCall
  { component :: Maybe Component,
    method :: Maybe HTTPMethod,
    path :: ByteString,
    query :: [QueryParam],
    body :: ByteString
  }
  deriving (Eq, Show, Generic, ToSchema Router "LocalCall", FromSchema Router "LocalCall")
  deriving (Arbitrary) via (GenericUniform LocalCall)

-- | This type exists because Enums are wrongly marked optional by mu-protobuf
-- Bug: https://github.com/higherkindness/mu-haskell/issues/282
data ValidatedLocalCall = ValidatedLocalCall
  { vComponent :: Component,
    vMethod :: HTTP.StdMethod,
    vPath :: ByteString,
    vQuery :: [QueryParam],
    vBody :: ByteString
  }
  deriving (Show, Eq)

data LocalCallValidationError
  = ComponentMissing
  | MethodMissing
  deriving (Show, Eq)

validateLocalCall :: LocalCall -> Validation (NonEmpty LocalCallValidationError) ValidatedLocalCall
validateLocalCall LocalCall {..} = do
  let vPath = path
      vQuery = query
      vBody = body
  vComponent <- validateComponent component
  vMethod <- validateMethod method
  pure $ ValidatedLocalCall {..}
  where
    validateComponent :: Maybe Component -> Validation (NonEmpty LocalCallValidationError) Component
    validateComponent Nothing = Failure $ ComponentMissing :| []
    validateComponent (Just UnspecifiedComponent) = Failure $ ComponentMissing :| []
    validateComponent (Just c) = Success c

    validateMethod :: Maybe HTTPMethod -> Validation (NonEmpty LocalCallValidationError) HTTP.StdMethod
    validateMethod Nothing = Failure $ MethodMissing :| []
    validateMethod (Just (HTTPMethod m)) = Success m

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
  BrigCall :: HTTP.StdMethod -> ByteString -> [QueryParam] -> ByteString -> Brig m (HTTP.Status, Maybe LByteString)

makeSem ''Brig

-- This can realistically only be tested in an integration test
interpretBrig ::
  forall m r a.
  (Monad m, MonadUnliftIO m, MonadThrow m, MonadMask m, Member (Embed (AppT m)) r) =>
  Sem (Brig ': r) a ->
  Sem r a
interpretBrig = interpret $ \case
  BrigCall m p q b -> embed @(AppT m) $ do
    brigReq <- view brig <$> ask
    let theCall =
          rpc' "brig" brigReq $
            RPC.method m
              . RPC.path p
              . RPC.query (map (\(QueryParam k v) -> (k, Just v)) q)
              . RPC.body (RPC.RequestBodyBS b)
    -- TODO: Don't retry everything
    res <- recovering x3 rpcHandlers $ const theCall
    pure (RPC.responseStatus res, RPC.responseBody res)

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

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
  case validateLocalCall req of
    Success ValidatedLocalCall {..} -> do
      (resStatus, resBody) <- brigCall vMethod vPath vQuery vBody
      case HTTP.statusCode resStatus of
        200 -> do
          pure $ ResponseOk $ maybe mempty LBS.toStrict resBody
        -- FUTUREWORK: Maybe it is not good expect 200 only, but do we need to
        -- be RESTful here?
        code -> pure $ ResponseErr ("federator -> brig: unexpected http response code: " <> Text.pack (show code))
    (Failure errs) -> pure $ ResponseErr $ "invalid request: " <> Text.pack (show errs)

-- * Server wiring

routeToRemote :: (MonadServer (Sem r), Member Remote r) => SingleServerT info RouteToRemote (Sem r) _
routeToRemote = singleService (Mu.method @"call" callRemote)

routeToInternal :: (Members '[Brig, Embed IO, Polysemy.Error ServerError] r) => SingleServerT info RouteToInternal (Sem r) _
routeToInternal = singleService (Mu.method @"call" callLocal)

serveRouteToRemote :: Int -> AppIO ()
serveRouteToRemote = undefined

-- TODO: Check if adding polysemy-plugin removes the need for type applications here
serveRouteToInternal :: Env -> Int -> IO ()
serveRouteToInternal env port = do
  runGRpcAppTrans msgProtoBuf port transformer routeToInternal
  where
    transformer :: Sem '[Embed IO, Polysemy.Error ServerError, Brig, Embed AppIO] a -> ServerErrorIO a
    transformer action =
      runAppT env
        . runM @AppIO
        . interpretBrig @ServerErrorIO
        . absorbServerError
        . embedToMonadIO @AppIO
        $ action

absorbServerError :: forall r a. (Member (Embed AppIO) r) => Sem (Polysemy.Error ServerError ': r) a -> Sem r a
absorbServerError action = do
  eitherResult <- runError action
  case eitherResult of
    Left err -> embed @AppIO $ throwError err
    Right res -> pure res
