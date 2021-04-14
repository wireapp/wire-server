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
import qualified Brig.Types.Search as Public
import Brig.Types.User
import Control.Error.Util ((!?))
import Control.Lens (view, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Domain
import Data.Handle
import Data.Qualified
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps
import qualified Network.HTTP.Types.Status as HTTP
import qualified Servant.Client as Servant
import qualified System.Logger.Class as Log
import Util.Options (epHost, epPort)
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.GRPC.Client

type FederationAppIO = ExceptT FederationError AppIO

-- FUTUREWORK(federation): As of now, any failure in making a remote call results in 404.
-- This is not correct, we should figure out how we communicate failure
-- scenarios to the clients.
-- See https://wearezeta.atlassian.net/browse/SQCORE-491 for the issue on error handling improvements.
getUserHandleInfo :: Qualified Handle -> FederationAppIO (Maybe UserProfile)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  federatorClient <- mkFederatorClient
  eitherResponse <- runExceptT . runFederatorClientWith federatorClient domain $ getUserByHandle clientRoutes handle
  -- TODO: Change this API to not rely on 404, encode not found as 200 and empty
  -- response or whatever `Maybe UserProfile` will encode into JSON
  case eitherResponse of
    Right profile -> pure $ Just profile
    Left err@(FederationClientServantError (Servant.FailureResponse _ res)) ->
      if HTTP.statusCode (Servant.responseStatusCode res) == 404
        then pure Nothing
        else throwE $ FederationCallFailure err
    Left err -> throwE $ FederationCallFailure err

searchUsers :: Domain -> Text -> FederationAppIO (Public.SearchResult Public.Contact)
searchUsers domain searchTerm = do
  Log.warn $ Log.msg $ T.pack "Brig-federation: search call on remote backend"
  executeFederated domain $ FederatedBrig.searchUsers clientRoutes searchTerm

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

executeFederated :: Domain -> FederatorClient (ExceptT FederationClientError FederationAppIO) a -> FederationAppIO a
executeFederated domain action = do
  federatorClient <- mkFederatorClient
  runExceptT (runFederatorClientWith federatorClient domain action)
    >>= either (throwE . FederationCallFailure) pure
