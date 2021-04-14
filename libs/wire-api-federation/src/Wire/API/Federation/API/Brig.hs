{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

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

module Wire.API.Federation.API.Brig where

import Control.Monad.Except (MonadError (..))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain)
import Data.Handle (Handle)
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.Record (GRpcMessageProtocol (MsgProtoBuf), GRpcReply (..), GrpcClient)
import Mu.GRpc.Client.TyApps (gRpcCall)
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.API.Generic
import qualified Servant.Client as Servant
import Servant.Client.Core (RequestF (..), ResponseF (..))
import Servant.Client.Core.Request (RequestBody (..))
import Servant.Client.Core.RunClient (RunClient (..))
import Servant.Client.Generic (AsClientT, genericClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.User (UserProfile)
import Wire.API.User.Search

-- Maybe this module should be called Brig
data Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        :> Get '[JSON] UserProfile,
    searchUsers ::
      routes
        :- "federation"
        :> "search"
        :> "users"
        -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
        -- (handles can be up to 256 chars currently)
        -- FUTUREWORK(federation): change this to a POST with a body,
        -- rather than a query parameter, after deciding on a general pattern here
        :> QueryParam' '[Required, Strict] "q" Text
        :> Get '[JSON] (SearchResult Contact)
  }
  deriving (Generic)

clientRoutes :: (MonadError FederationClientError m, MonadIO m) => Api (AsClientT (FederatorClient m))
clientRoutes = genericClient

data FederatorClientEnv = FederatorClientEnv
  { grpcClient :: GrpcClient,
    domain :: Domain
  }

newtype FederatorClient m a = FederatorClient {runFederatorClient :: ReaderT FederatorClientEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader FederatorClientEnv, MonadIO)

runFederatorClientWith :: GrpcClient -> Domain -> FederatorClient m a -> m a
runFederatorClientWith client dmn = flip runReaderT (FederatorClientEnv client dmn) . runFederatorClient

instance (Monad m, MonadError FederationClientError m) => MonadError FederationClientError (FederatorClient m) where
  throwError = FederatorClient . throwError
  catchError (FederatorClient action) f = FederatorClient $ catchError action (runFederatorClient . f)

data FederationClientError
  = FederationClientInvalidMethod HTTP.Method
  | FederationClientStreamingUnsupported
  | FederationClientRPCError Text
  | FederationClientOutwardError Proto.OutwardError
  | FederationClientServantError Servant.ClientError

instance (Monad m, MonadError FederationClientError m, MonadIO m) => RunClient (FederatorClient m) where
  -- TODO: Figure out what needs to be done for _expectedStatuses
  runRequestAcceptStatus _expectedStatuses req = do
    env <- ask
    parsedMethod <- either (throwError . FederationClientInvalidMethod) pure $ HTTP.parseMethod (requestMethod req)
    let query = foldMap (\(key, maybeVal) -> [Proto.QueryParam key (fromMaybe "" maybeVal)]) $ requestQueryString req
    body <- readBody . maybe (RequestBodyBS "") fst $ requestBody req
    let call =
          Proto.ValidatedFederatedRequest
            (domain env)
            ( Proto.Request
                Proto.Brig
                (Proto.HTTPMethod parsedMethod)
                (LBS.toStrict . toLazyByteString $ requestPath req)
                query
                body
            )
    grpcResponse <- callRemote (grpcClient env) call
    -- TODO: Incorporate refactoring from https://github.com/wireapp/wire-server/pull/1438
    case grpcResponse of
      GRpcTooMuchConcurrency _tmc -> rpcErr "too much concurrency"
      GRpcErrorCode code -> rpcErr $ "grpc error code: " <> T.pack (show code)
      GRpcErrorString msg -> rpcErr $ "grpc error: " <> T.pack msg
      GRpcClientError msg -> rpcErr $ "grpc client error: " <> T.pack (show msg)
      GRpcOk (Proto.OutwardResponseError err) -> throwError (FederationClientOutwardError err)
      GRpcOk (Proto.OutwardResponseHTTPResponse res) ->
        pure $
          Response
            { -- TODO: Here only the status code is set and not the message
              -- along with the code. Figuring out right message will be
              -- tedious, but I guess it has to be done? Maybe we can always set
              -- this to 200 OK and throw some other error if it is not 200.
              responseStatusCode = HTTP.mkStatus (fromIntegral $ Proto.responseStatus res) "",
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

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.OutwardResponse)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)
