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

import Brig.App (AppIO, federator, viewFederationDomain)
import Brig.Types (Prekey, PrekeyBundle)
import qualified Brig.Types.Search as Public
import Brig.Types.User
import Control.Error.Util ((!?))
import Control.Lens (view, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Domain
import Data.Handle
import Data.Id (ClientId, UserId)
import Data.Qualified
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.TyApps
import qualified System.Logger.Class as Log
import Util.Options (epHost, epPort)
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.Client (FederationClientError, FederationError (..), FederatorClient, runFederatorClientWith)
import Wire.API.Federation.GRPC.Client
import Wire.API.Message (UserClientMap, UserClients)
import Wire.API.User.Client.Prekey (ClientPrekey)

type FederationAppIO = ExceptT FederationError AppIO

-- FUTUREWORK: Maybe find a way to tranform 'clientRoutes' into a client which
-- only uses 'FederationAppIO' monad, then boilerplate in this module can all be
-- deleted.
getUserHandleInfo :: Qualified Handle -> FederationAppIO (Maybe UserProfile)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  executeFederated domain $ getUserByHandle clientRoutes handle

getUsersByIds :: Domain -> [UserId] -> FederationAppIO [UserProfile]
getUsersByIds domain uids = do
  Log.info $ Log.msg ("Brig-federation: get users by ids on remote backends" :: ByteString)
  executeFederated domain $ FederatedBrig.getUsersByIds clientRoutes uids

-- FUTUREWORK(federation): Abstract out all the rpc boilerplate and error handling
claimPrekey :: Qualified UserId -> ClientId -> FederationAppIO (Maybe ClientPrekey)
claimPrekey (Qualified user domain) client = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey"
  executeFederated domain $ FederatedBrig.claimPrekey clientRoutes (user, client)

claimPrekeyBundle :: Qualified UserId -> FederationAppIO PrekeyBundle
claimPrekeyBundle (Qualified user domain) = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey bundle"
  executeFederated domain $ FederatedBrig.getPrekeyBundle clientRoutes user

claimMultiPrekeyBundle ::
  Domain ->
  UserClients ->
  FederationAppIO (UserClientMap (Maybe Prekey))
claimMultiPrekeyBundle domain uc = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote multi-user prekey bundle"
  executeFederated domain $ FederatedBrig.getMultiPrekeyBundle clientRoutes uc

-- FUTUREWORK(federation): rework error handling and FUTUREWORK from getUserHandleInfo and search:
--       decoding error should not throw a 404 most likely
--       and non-200, non-404 should also not become 404s. Looks like some tests are missing and
--       https://wearezeta.atlassian.net/browse/SQCORE-491 is not quite done yet.
searchUsers :: Domain -> SearchRequest -> FederationAppIO (Public.SearchResult Public.Contact)
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

executeFederated :: Domain -> FederatorClient component (ExceptT FederationClientError FederationAppIO) a -> FederationAppIO a
executeFederated targetDomain action = do
  federatorClient <- mkFederatorClient
  originDomain <- viewFederationDomain
  runExceptT (runFederatorClientWith federatorClient targetDomain originDomain action)
    >>= either (throwE . FederationCallFailure) pure
