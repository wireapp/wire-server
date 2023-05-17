{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadMask, throwM)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Control.Retry
import Control.Timeout
import Data.Domain
import Data.Handle
import Data.Id (ClientId, UserId)
import Data.Qualified
import Data.Range (Range)
import qualified Data.Text as T
import Data.Time.Units
import Imports
import qualified Network.AMQP as Q
import qualified System.Logger.Class as Log
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
  Remote UserId ->
  RemoteConnectionAction ->
  ExceptT FederationError m NewConnectionResponse
sendConnectionAction self (tUntagged -> other) action = do
  let req = NewConnectionRequest (tUnqualified self) (qUnqualified other) action
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
      domain = tDomain self
  let notif = BackendNotification domain (OnUserDeletedConnections $ UserDeletedConnectionsNotification (tUnqualified self) remoteConnections)
  enqueueNotification (tDomain remotes) notif Q.Persistent

-- | Enqueues notifications in RabbitMQ. Retries 3 times with a delay of 1s.
enqueueNotification :: (MonadReader Env m, MonadIO m, MonadMask m, Log.MonadLogger m) => Domain -> BackendNotification -> Q.DeliveryMode -> m ()
enqueueNotification domain notif deliveryMode =
  recovering (limitRetries 3 <> constantDelay 1_000_000) [logRetries (const $ pure True) logError] (const go)
  where
    logError willRetry (SomeException e) status = do
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
    go = do
      mChan <- timeout (1 :: Second) (readMVar =<< view rabbitmqChannel)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> liftIO $ enqueue chan domain notif deliveryMode

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
  endpoint <- view federator >>= maybe (throwE FederationNotConfigured) pure
  mgr <- view http2Manager
  let env =
        FederatorClientEnv
          { ceOriginDomain = ownDomain,
            ceTargetDomain = targetDomain,
            ceFederator = endpoint,
            ceHttp2Manager = mgr
          }
  liftIO (runFederatorClient env action)
    >>= either (throwE . FederationCallFailure) pure
