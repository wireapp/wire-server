{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Handle (Handle, fromHandle)
import qualified Data.Text.Encoding as T
import Imports
import Mu.GRpc.Client.Record (GRpcMessageProtocol (MsgProtoBuf), GRpcReply (..), GrpcClient)
import Mu.GRpc.Client.TyApps (gRpcCall)
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.API.Generic
import Servant.Client.Core (RequestF (..), ResponseF (..))
import Servant.Client.Core.Request (RequestBody (..))
import Servant.Client.Core.RunClient (RunClient (..))
import Servant.Client.Generic (AsClientT, genericClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.User (UserProfile)
import qualified Servant.Client as Servant

-- Maybe this module should be called Brig
newtype Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        :> Get '[JSON] UserProfile
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

instance (Monad m, MonadError FederationClientError m) => MonadError FederationClientError (FederatorClient m) where
  throwError = FederatorClient . throwError
  catchError (FederatorClient action) f = FederatorClient $ catchError action (runFederatorClient . f)

data FederationClientError
  = FederationClientInvalidMethod HTTP.Method
  | FederationClientStreamingUnsupported
  | FederationClientThrowBetterError
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
      GRpcTooMuchConcurrency _tmc ->
        throwError FederationClientThrowBetterError
      GRpcErrorCode _errCode ->
        throwError FederationClientThrowBetterError
      GRpcErrorString _errStr ->
        throwError FederationClientThrowBetterError
      GRpcClientError _clErr ->
        throwError FederationClientThrowBetterError
      GRpcOk (Proto.OutwardResponseError err) -> throwError (FederationClientOutwardError err)
      GRpcOk (Proto.OutwardResponseHTTPResponse res) ->
        pure $
          Response
            { responseStatusCode = HTTP.mkStatus (fromIntegral $ Proto.responseStatus res) ""
            , responseHeaders = mempty
            , responseHttpVersion = HTTP.http11
            , responseBody = LBS.fromStrict $ Proto.responseBody res
            }
    where
      readBody = \case
        RequestBodyLBS lbs -> pure $ LBS.toStrict lbs
        RequestBodyBS bs -> pure bs
        RequestBodySource _ -> throwError FederationClientStreamingUnsupported
  throwClientError = throwError . FederationClientServantError

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.OutwardResponse)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)

-- FUTUREWORK(federation): Idea: by keeping the functions to construct a Request and the API definitions in the same place,
-- we can:
-- - more easily make sure their definitions match
-- - probably add their path segments to a list for validation purposes to guard against path traversals.

-- FUTUREWORK(federation): I think we should make the federation/ prefix explicit here and not add it in services/federator/src/Federator/Federate.hs
mkGetUserInfoByHandle :: Handle -> Proto.Request
mkGetUserInfoByHandle handle =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "users/by-handle"
    [Proto.QueryParam "handle" (T.encodeUtf8 (fromHandle handle))]
    mempty

-- TODO: Can we write a test which makes use of mkSearchUsers against the Api in this file?
-- Naming bikeshedding: current /search/contacts. /search/users is a better name, no?
mkSearchUsers :: Text -> Proto.Request
mkSearchUsers searchTerm =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "search/users"
    [Proto.QueryParam "q" (T.encodeUtf8 searchTerm)]
    mempty
