{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

module Wire.API.Federation.Client where

import Control.Monad.Except (MonadError (..))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain)
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps (GRpcMessageProtocol (MsgProtoBuf), GRpcReply (..), GrpcClient, gRpcCall)
import qualified Network.HTTP.Types as HTTP
import Servant.Client (ResponseF (..))
import qualified Servant.Client as Servant
import Servant.Client.Core (RequestBody (..), RequestF (..), RunClient (..))
import qualified Wire.API.Federation.GRPC.Types as Proto

data FederatorClientEnv = FederatorClientEnv
  { grpcClient :: GrpcClient,
    domain :: Domain
  }

newtype FederatorClient (component :: Proto.Component) m a = FederatorClient {runFederatorClient :: ReaderT FederatorClientEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader FederatorClientEnv, MonadIO)

runFederatorClientWith :: GrpcClient -> Domain -> FederatorClient component m a -> m a
runFederatorClientWith client dmn = flip runReaderT (FederatorClientEnv client dmn) . runFederatorClient

class KnownComponent (c :: Proto.Component) where
  componentVal :: Proto.Component

instance KnownComponent 'Proto.Brig where
  componentVal = Proto.Brig

-- | expectedStatuses is ignored as we don't want to deal with statuses, only
-- 200 is accepted. If other status code is encountered,
-- 'FederationClientInvalidStatus' is thrown.
instance (Monad m, MonadError FederationClientError m, MonadIO m, KnownComponent component) => RunClient (FederatorClient component m) where
  runRequestAcceptStatus _expectedStatuses req = do
    env <- ask
    parsedMethod <- either (throwError . FederationClientInvalidMethod) pure $ HTTP.parseMethod (requestMethod req)
    let query = foldMap (\(key, maybeVal) -> [Proto.QueryParam key (fromMaybe "" maybeVal)]) $ requestQueryString req
    body <- readBody . maybe (RequestBodyBS "") fst $ requestBody req
    let call =
          Proto.ValidatedFederatedRequest
            (domain env)
            ( Proto.Request
                (componentVal @component)
                (Proto.HTTPMethod parsedMethod)
                (LBS.toStrict . toLazyByteString $ requestPath req)
                query
                body
            )
    grpcResponse <- callRemote (grpcClient env) call
    case grpcResponse of
      GRpcTooMuchConcurrency _tmc -> rpcErr "too much concurrency"
      GRpcErrorCode code -> rpcErr $ "grpc error code: " <> T.pack (show code)
      GRpcErrorString msg -> rpcErr $ "grpc error: " <> T.pack msg
      GRpcClientError msg -> rpcErr $ "grpc client error: " <> T.pack (show msg)
      GRpcOk (Proto.OutwardResponseError err) -> throwError (FederationClientOutwardError err)
      GRpcOk (Proto.OutwardResponseHTTPResponse res) ->
        if Proto.responseStatus res /= 200
          then throwError $ FederationClientInvalidStatus (Proto.responseStatus res)
          else do
            pure $
              Response
                { responseStatusCode = HTTP.ok200,
                  -- This is required so servant can parse the body
                  responseHeaders = [(HTTP.hContentType, "application/json")],
                  -- Here HTTP 1.1 is hardcoded with the hope that it wouldn't
                  -- really be used anywhere.
                  responseHttpVersion = HTTP.http11,
                  responseBody = LBS.fromStrict $ Proto.responseBody res
                }
    where
      rpcErr = throwError . FederationClientRPCError
      readBody = \case
        RequestBodyLBS lbs -> pure $ LBS.toStrict lbs
        RequestBodyBS bs -> pure bs
        RequestBodySource _ -> throwError FederationClientStreamingUnsupported
  throwClientError = throwError . FederationClientServantError

instance (Monad m, MonadError FederationClientError m) => MonadError FederationClientError (FederatorClient c m) where
  throwError = FederatorClient . throwError
  catchError (FederatorClient action) f = FederatorClient $ catchError action (runFederatorClient . f)

data FederationClientError
  = FederationClientInvalidMethod HTTP.Method
  | FederationClientStreamingUnsupported
  | FederationClientRPCError Text
  | FederationClientOutwardError Proto.OutwardError
  | FederationClientInvalidStatus Word32
  | FederationClientServantError Servant.ClientError

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.OutwardResponse)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)
