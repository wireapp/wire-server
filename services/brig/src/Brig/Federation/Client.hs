-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- FUTUREWORK: Remove this module all together.
module Brig.Federation.Client where

import Brig.App
import Control.Monad
import Control.Monad.Catch (MonadMask, throwM)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Control.Retry
import Control.Timeout
import Data.Domain
import Data.Handle
import Data.Id
import Data.Qualified
import Data.Range (Range)
import Data.Text qualified as T
import Data.Time.Units
import Imports
import Network.AMQP qualified as Q
import System.Logger.Class qualified as Log
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap

getUserHandleInfo ::
  ( MonadReader Env m,
    MonadIO m,
    Log.MonadLogger m
  ) =>
  Remote Handle ->
  ExceptT FederationError m (Maybe UserProfile)
getUserHandleInfo (tUntagged -> Qualified handle domain) = do
  lift $ Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  runBrigFederatorClient domain $ fedClient @'Brig @"get-user-by-handle" handle

getUsersByIds ::
  ( MonadReader Env m,
    MonadIO m,
    Log.MonadLogger m
  ) =>
  Domain ->
  [UserId] ->
  ExceptT FederationError m [UserProfile]
getUsersByIds domain uids = do
  lift $ Log.info $ Log.msg ("Brig-federation: get users by ids on remote backends" :: ByteString)
  runBrigFederatorClient domain $ fedClient @'Brig @"get-users-by-ids" uids

claimPrekey ::
  (MonadReader Env m, MonadIO m, Log.MonadLogger m) =>
  Qualified UserId ->
  ClientId ->
  ExceptT FederationError m (Maybe ClientPrekey)
claimPrekey (Qualified user domain) client = do
  lift $ Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey"
  runBrigFederatorClient domain $ fedClient @'Brig @"claim-prekey" (user, client)

claimPrekeyBundle ::
  ( MonadReader Env m,
    MonadIO m,
    Log.MonadLogger m
  ) =>
  Qualified UserId ->
  ExceptT FederationError m PrekeyBundle
claimPrekeyBundle (Qualified user domain) = do
  lift $ Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey bundle"
  runBrigFederatorClient domain $ fedClient @'Brig @"claim-prekey-bundle" user

claimMultiPrekeyBundle ::
  ( Log.MonadLogger m,
    MonadReader Env m,
    MonadIO m
  ) =>
  Domain ->
  UserClients ->
  ExceptT FederationError m UserClientPrekeyMap
claimMultiPrekeyBundle domain uc = do
  lift . Log.info $ Log.msg @Text "Brig-federation: claiming remote multi-user prekey bundle"
  runBrigFederatorClient domain $ fedClient @'Brig @"claim-multi-prekey-bundle" uc

searchUsers ::
  ( MonadReader Env m,
    MonadIO m,
    Log.MonadLogger m
  ) =>
  Domain ->
  SearchRequest ->
  ExceptT FederationError m SearchResponse
searchUsers domain searchTerm = do
  lift $ Log.info $ Log.msg $ T.pack "Brig-federation: search call on remote backend"
  runBrigFederatorClient domain $ fedClient @'Brig @"search-users" searchTerm

getUserClients ::
  ( MonadReader Env m,
    MonadIO m,
    Log.MonadLogger m
  ) =>
  Domain ->
  GetUserClients ->
  ExceptT FederationError m (UserMap (Set PubClient))
getUserClients domain guc = do
  lift $ Log.info $ Log.msg @Text "Brig-federation: get users' clients from remote backend"
  runBrigFederatorClient domain $ fedClient @'Brig @"get-user-clients" guc

sendConnectionAction ::
  (MonadReader Env m, MonadIO m, Log.MonadLogger m) =>
  Local UserId ->
  Maybe (Local TeamId) ->
  Remote UserId ->
  RemoteConnectionAction ->
  ExceptT FederationError m NewConnectionResponse
sendConnectionAction self mSelfTeam (tUntagged -> other) action = do
  let req =
        NewConnectionRequest
          (tUnqualified self)
          (tUnqualified <$> mSelfTeam)
          (qUnqualified other)
          action
  lift $ Log.info $ Log.msg @Text "Brig-federation: sending connection action to remote backend"
  runBrigFederatorClient (qDomain other) $ fedClient @'Brig @"send-connection-action" req

notifyUserDeleted ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    Log.MonadLogger m
  ) =>
  Local UserId ->
  Remote (Range 1 1000 [UserId]) ->
  m ()
notifyUserDeleted self remotes = do
  let remoteConnections = tUnqualified remotes
  let notif = UserDeletedConnectionsNotification (tUnqualified self) remoteConnections
      remoteDomain = tDomain remotes
  asks (.rabbitmqChannel) >>= \case
    Just chanVar -> do
      enqueueNotification (tDomain self) remoteDomain Q.Persistent chanVar $
        fedQueueClient @'OnUserDeletedConnectionsTag notif
    Nothing ->
      Log.err $
        Log.msg ("Federation error while notifying remote backends of a user deletion." :: ByteString)
          . Log.field "user_id" (show self)
          . Log.field "domain" (domainText remoteDomain)
          . Log.field "error" (show FederationNotConfigured)

-- | Enqueues notifications in RabbitMQ. Retries 3 times with a delay of 1s.
enqueueNotification :: (MonadIO m, MonadMask m, Log.MonadLogger m, MonadReader Env m) => Domain -> Domain -> Q.DeliveryMode -> MVar Q.Channel -> FedQueueClient c () -> m ()
enqueueNotification ownDomain remoteDomain deliveryMode chanVar action = do
  let policy = limitRetries 3 <> constantDelay 1_000_000
  recovering policy [logRetries (const $ pure True) logError] (const go)
  where
    logError willRetry (SomeException e) status = do
      rid <- asks (.requestId)
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
          . Log.field "request" rid
    go = do
      rid <- asks (.requestId)
      mChan <- timeout (1 :: Second) (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> liftIO $ enqueue chan rid ownDomain remoteDomain deliveryMode action

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel

runBrigFederatorClient ::
  (MonadReader Env m, MonadIO m) =>
  Domain ->
  FederatorClient 'Brig a ->
  ExceptT FederationError m a
runBrigFederatorClient targetDomain action = do
  ownDomain <- viewFederationDomain
  endpoint <- asks (.federator) >>= maybe (throwE FederationNotConfigured) pure
  mgr <- asks (.http2Manager)
  rid <- asks (.requestId)
  let env =
        FederatorClientEnv
          { ceOriginDomain = ownDomain,
            ceTargetDomain = targetDomain,
            ceFederator = endpoint,
            ceHttp2Manager = mgr,
            ceOriginRequestId = rid
          }
  liftIO (runFederatorClient env action)
    >>= either (throwE . FederationCallFailure) pure
