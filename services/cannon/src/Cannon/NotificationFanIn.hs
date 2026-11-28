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

-- | Socket adapter and interpreter bootstrap for the notification fan-in API.
-- Each socket has its own state lock. The shared registry only routes wake-ups;
-- database reads never hold its lock.
module Cannon.NotificationFanIn where

import Cannon.Options qualified as O
import Cannon.WS qualified as Legacy
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (check)
import Control.Exception (finally, throwIO, try)
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Range (unsafeRange)
import Data.Set qualified as Set
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as PG
import Imports
import Network.HTTP.Types (status400, status403, status500, statusCode)
import Network.Wai.Utilities.Error qualified as Wai
import Network.WebSockets qualified as WS
import Polysemy hiding (run)
import Polysemy.Error hiding (try)
import Polysemy.State
import System.Logger qualified as Log
import Wire.API.Notification.FanIn
import Wire.FanInAckStore
import Wire.FanInAckStore.Postgres
import Wire.FanInDeliveryAccess
import Wire.FanInDeliveryAccess.Rpc
import Wire.FanInDeliverySubsystem
import Wire.FanInDeliverySubsystem.Interpreter
import Wire.FanInEpochMembershipStore
import Wire.FanInEpochMembershipStore.Postgres
import Wire.FanInNotificationStore
import Wire.FanInNotificationStore.Postgres
import Wire.ParseException
import Wire.Rpc

-- Fan-in sockets are registered here for explicit wakeups. Registering them
-- with legacy presence would make Gundeck target them with legacy pushes.
data FanInSocket = FanInSocket
  { userId :: UserId,
    clientId :: Maybe ClientId,
    wake :: TVar Bool,
    transient :: TBQueue FanInPush,
    socket :: WS.Connection
  }

data SocketCommand = ReadBacklog | Forward FanInPush | PeerClosed

data FanInRuntime = FanInRuntime
  { pool :: PG.Pool,
    sockets :: TVar (Map FanInSessionId FanInSocket)
  }

newFanInRuntime :: O.NotificationFanInPostgres -> IO FanInRuntime
newFanInRuntime config = do
  pool <- PG.initPostgresPool config.pool config.connection config.password
  sockets <- newTVarIO Map.empty
  pure FanInRuntime {pool, sockets}

type FanInApp = Sem '[FanInDeliverySubsystem, State FanInDeliveryState, FanInDeliveryAccess, FanInNotificationStore, FanInAckStore, FanInEpochMembershipStore, Error FanInDeliveryError, Error FanInNotificationStoreError, Error FanInAckStoreError, Error FanInEpochMembershipStoreError, Error UsageError, Error ParseException, Error Wai.Error, Rpc, Embed IO]

runFanIn :: O.Opts -> Legacy.Env -> FanInRuntime -> FanInDeliveryState -> FanInApp a -> IO (FanInDeliveryState, a)
runFanIn opts env runtime state action = do
  result <-
    runM
      . runRpcWithHttp env.manager env.reqId
      . runError @Wai.Error
      . mapError (const internalError :: ParseException -> Wai.Error)
      . mapError (const internalError :: UsageError -> Wai.Error)
      . mapError (const internalError :: FanInEpochMembershipStoreError -> Wai.Error)
      . mapError (const internalError :: FanInAckStoreError -> Wai.Error)
      . mapError (const internalError :: FanInNotificationStoreError -> Wai.Error)
      . mapError deliveryError
      . interpretFanInEpochMembershipStoreToPostgres runtime.pool
      . interpretFanInAckStoreToPostgres runtime.pool
      . interpretFanInNotificationStoreToPostgres runtime.pool
      . runFanInDeliveryAccess opts._optsBrig opts._optsGalley opts._optsFederationDomain (notifySockets runtime)
      . runState state
      . interpretFanInDeliverySubsystem opts._optsFederationDomain
      $ action
  either throwIO pure result
  where
    internalError = Wai.mkError status500 "server-error" "Could not process fan-in notifications"
    deliveryError = \case
      FanInClientNotOwned -> Wai.mkError status403 "client-not-owned" "The client does not belong to this user"
      FanInInvalidStoredPayload -> internalError
      _ -> Wai.mkError status400 "invalid-fan-in-message" "Invalid fan-in session message"

notifySockets :: FanInRuntime -> FanInCannonRequest -> IO ()
notifySockets runtime request = do
  let recipientClients = Set.fromList [(recipient.userId, recipient.clientId) | recipient <- toList request.recipients]
      recipientUsers = Set.fromList (map fst (Set.toList recipientClients))
      matches entry = maybe (Set.member entry.userId recipientUsers) (\cid -> Set.member (entry.userId, cid) recipientClients) entry.clientId
  atomically $ do
    sockets <- readTVar runtime.sockets
    for_ (Map.elems sockets) $ \entry ->
      when (matches entry) $
        if request.push.transient
          then do
            full <- isFullTBQueue entry.transient
            unless full (writeTBQueue entry.transient request.push)
          else writeTVar entry.wake True

notifyFanIn :: O.Opts -> Legacy.Env -> FanInRuntime -> FanInCannonRequest -> IO ()
notifyFanIn opts env runtime request = void $ runFanIn opts env runtime emptyFanInDeliveryState (notifyFanInSessions request)

drainFanIn :: FanInRuntime -> IO ()
drainFanIn runtime = do
  sockets <- readTVarIO runtime.sockets
  for_ (Map.elems sockets) $ \entry -> void (try @SomeException (WS.sendClose entry.socket ("server shutdown" :: Text)))

fanInWebSocketApp :: O.Opts -> Legacy.Env -> FanInRuntime -> UserId -> Maybe ClientId -> WS.ServerApp
fanInWebSocketApp opts env runtime uid cid pending = do
  state <- newMVar emptyFanInDeliveryState
  let run :: forall a. FanInApp a -> IO a
      run action = modifyMVar state $ \current -> runFanIn opts env runtime current action
  opened <- try @Wai.Error (run (openFanInSession uid cid))
  case opened of
    Left err -> WS.rejectRequestWith pending WS.defaultRejectRequest {WS.rejectCode = statusCode err.code, WS.rejectMessage = "Fan-in connection rejected", WS.rejectBody = BS.toStrict (A.encode err)}
    Right sid -> do
      conn <- WS.acceptRequest pending
      let close = do
            atomically $ modifyTVar' runtime.sockets (Map.delete sid)
            void (try @SomeException (run (closeFanInSession sid)))
            void (try @SomeException (WS.sendClose conn ("" :: Text)))
      flip finally close $ WS.withPingPong WS.defaultPingPongOptions conn $ \socket -> do
        wake <- newTVarIO True
        transient <- newTBQueueIO 100
        let entry = FanInSocket uid cid wake transient socket
        atomically $ modifyTVar' runtime.sockets (Map.insert sid entry)
        peerClosed <- newTVarIO False
        let readMessages = do
              received <- try @WS.ConnectionException (WS.receiveData socket)
              case received of
                Left _ -> atomically (writeTVar peerClosed True)
                Right incomingBytes -> case A.eitherDecode @FanInClientMessage incomingBytes of
                  Left _ -> WS.sendCloseCode socket 1008 ("Invalid fan-in message" :: Text) >> throwIO (Wai.mkError status400 "invalid-fan-in-message" "Invalid fan-in message")
                  Right message -> do
                    -- Commit before receiving the next frame, including Close.
                    -- A close handshake therefore cannot overtake an ack.
                    run (handleFanInClientMessage sid message)
                    atomically (writeTVar wake True)
                    readMessages
            sendBatch = do
              events <- run (readFanInSession sid (unsafeRange 100))
              for_ events (WS.sendBinaryData socket . A.encode)
              unless (null events) (atomically (writeTVar wake True))
            sendMessages preferTransient = do
              let backlog = readTVar wake >>= check >> writeTVar wake False >> pure ReadBacklog
                  ephemeral = Forward <$> readTBQueue transient
              command <-
                atomically $
                  (readTVar peerClosed >>= check >> pure PeerClosed)
                    `orElse` (if preferTransient then ephemeral `orElse` backlog else backlog `orElse` ephemeral)
              case command of
                PeerClosed -> pure ()
                ReadBacklog -> sendBatch >> sendMessages True
                Forward push -> do
                  event <- run (deliverFanInTransient sid push)
                  for_ event (WS.sendBinaryData socket . A.encode)
                  sendMessages False
        -- Wait for the reader to commit every acknowledgement preceding Close.
        -- Exceptions still cancel both workers and the bracket closes the socket.
        result <- try @SomeException (concurrently_ readMessages (sendMessages False))
        case result of
          Right () -> pure ()
          Left _ -> Log.debug env.logg (Log.msg (Log.val "Fan-in WebSocket closed") . Log.field "user" (idToText uid))
