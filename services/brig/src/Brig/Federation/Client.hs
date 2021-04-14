{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Brig.Federation.Client where

import Brig.API.Types (FederationError (..))
import Brig.App (AppIO, federator)
import Brig.Types (Prekey, PrekeyBundle)
import qualified Brig.Types.Search as Public
import Brig.Types.User
import Control.Error.Util ((!?))
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import qualified Data.Aeson as Aeson
import Data.Domain
import Data.Handle
import Data.Id (ClientId, UserId)
import qualified Data.Map as Map
import Data.Qualified
import Data.String.Conversions
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps
import qualified System.Logger.Class as Log
import Util.Options (epHost, epPort)
import Wire.API.Federation.API.Brig
import Wire.API.Federation.GRPC.Client
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Message (UserClientMap, UserClients)
import Wire.API.User.Client.Prekey (ClientPrekey)

type FederationAppIO = ExceptT FederationError AppIO

-- FUTUREWORK(federation): As of now, any failure in making a remote call results in 404.
-- This is not correct, we should figure out how we communicate failure
-- scenarios to the clients.
-- See https://wearezeta.atlassian.net/browse/SQCORE-491 for the issue on error handling improvements.
getUserHandleInfo :: Qualified Handle -> FederationAppIO (Maybe UserProfile)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg ("Brig-federation: handle lookup call on remote backend" :: ByteString)
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkGetUserInfoByHandle handle)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    404 -> pure Nothing
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwE (FederationInvalidResponseBody (T.pack err))
      Right x -> pure $ Just x
    code -> throwE (FederationInvalidResponseCode code)

-- FUTUREWORK: Test
getUsersByIds :: [Qualified UserId] -> FederationAppIO [UserProfile]
getUsersByIds quids = do
  Log.info $ Log.msg ("Brig-federation: get users by ids on remote backends" :: ByteString)
  federatorClient <- mkFederatorClient
  let domainWiseIds = qualifiedToMap quids
      requests = Map.foldMapWithKey (\domain uids -> [Proto.ValidatedFederatedRequest domain (mkGetUsersByIds uids)]) domainWiseIds
  -- TODO: Make these concurrent
  concat <$> mapM (processResponse <=< expectOk <=< callRemote federatorClient) requests
  where
    -- TODO: Do we want to not fail the whole request if there is one bad remote?
    processResponse :: Proto.HTTPResponse -> FederationAppIO [UserProfile]
    processResponse res =
      case Proto.responseStatus res of
        200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
          Left err -> throwE (FederationInvalidResponseBody (T.pack err))
          Right profiles -> pure profiles
        code -> throwE (FederationInvalidResponseCode code)

-- FUTUREWORK(federation): Abstract out all the rpc boilerplate and error handling
claimPrekey :: Qualified UserId -> ClientId -> FederationAppIO (Maybe ClientPrekey)
claimPrekey (Qualified user domain) client = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey"
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkClaimPrekey user client)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    404 -> pure Nothing
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwE (FederationInvalidResponseBody (T.pack err))
      Right x -> pure $ Just x
    code -> throwE (FederationInvalidResponseCode code)

claimPrekeyBundle :: Qualified UserId -> FederationAppIO PrekeyBundle
claimPrekeyBundle (Qualified user domain) = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey bundle"
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkClaimPrekeyBundle user)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwE (FederationInvalidResponseBody (T.pack err))
      Right x -> pure x
    code -> throwE (FederationInvalidResponseCode code)

claimMultiPrekeyBundle :: Domain -> UserClients -> FederationAppIO (UserClientMap (Maybe Prekey))
claimMultiPrekeyBundle domain uc = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote multi-user prekey bundle"
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkClaimMultiPrekeyBundle uc)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwE (FederationInvalidResponseBody (T.pack err))
      Right x -> pure x
    code -> throwE (FederationInvalidResponseCode code)

qualifiedToMap :: [Qualified a] -> Map Domain [a]
qualifiedToMap = Map.fromListWith (<>) . map (\(Qualified thing domain) -> (domain, [thing]))

-- FUTUREWORK(federation): reduce duplication between these functions
-- FUTUREWORK(federation): rework error handling and FUTUREWORK from getUserHandleInfo and search:
--       decoding error should not throw a 404 most likely
--       and non-200, non-404 should also not become 404s. Looks like some tests are missing and
--       https://wearezeta.atlassian.net/browse/SQCORE-491 is not quite done yet.
searchUsers :: Domain -> Text -> FederationAppIO (Public.SearchResult Public.Contact)
searchUsers domain searchTerm = do
  Log.warn $ Log.msg $ T.pack "Brig-federation: search call on remote backend"
  federatorClient <- mkFederatorClient
  let call = Proto.ValidatedFederatedRequest domain (mkSearchUsers searchTerm)
  res <- expectOk =<< callRemote federatorClient call
  case Proto.responseStatus res of
    200 -> case Aeson.eitherDecodeStrict (Proto.responseBody res) of
      Left err -> throwE $ (FederationInvalidResponseBody (T.pack err))
      Right x -> pure $ x
    code -> throwE (FederationInvalidResponseCode code)

-- FUTUREWORK: It would be nice to share the client across all calls to
-- federator and not call this function on every invocation of federated
-- requests, but there are some issues in http2-client which might need some
-- fixing first. More context here:
-- https://github.com/lucasdicioccio/http2-client/issues/37
-- https://github.com/lucasdicioccio/http2-client/issues/49
mkFederatorClient :: FederationAppIO GrpcClient
mkFederatorClient = do
  federatorEndpoint <- view federator !? FederationNotConfigured
  let cfg = grpcClientConfigSimple (T.unpack (federatorEndpoint ^. epHost)) (fromIntegral (federatorEndpoint ^. epPort)) False
  createGrpcClient cfg
    >>= either (throwE . FederationUnavailable . reason) pure

callRemote :: MonadIO m => GrpcClient -> Proto.ValidatedFederatedRequest -> m (GRpcReply Proto.OutwardResponse)
callRemote fedClient call = liftIO $ gRpcCall @'MsgProtoBuf @Proto.Outward @"Outward" @"call" fedClient (Proto.validatedFederatedRequestToFederatedRequest call)

-- FUTUREWORK(federation) All of this code is only exercised in the test which
-- goes between two Backends. This is not ideal, we should figure out a way to
-- test client side of federated code without needing another backend. We could
-- do this either by mocking the second backend in integration tests or making
-- all of this independent of the Handler monad and write unit tests.
expectOk :: GRpcReply Proto.OutwardResponse -> FederationAppIO Proto.HTTPResponse
expectOk = \case
  GRpcTooMuchConcurrency _tmc -> rpcErr "too much concurrency"
  GRpcErrorCode code -> rpcErr $ "grpc error code: " <> T.pack (show code)
  GRpcErrorString msg -> rpcErr $ "grpc error: " <> T.pack msg
  GRpcClientError msg -> rpcErr $ "grpc client error: " <> T.pack (show msg)
  GRpcOk (Proto.OutwardResponseError err) -> throwE (FederationRemoteError err)
  GRpcOk (Proto.OutwardResponseHTTPResponse res) -> pure res
  where
    rpcErr :: Text -> FederationAppIO a
    rpcErr msg = throwE (FederationRpcError msg)
