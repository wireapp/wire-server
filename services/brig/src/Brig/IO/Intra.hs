{-# LANGUAGE RecordWildCards #-}

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

-- FUTUREWORK: Move to Brig.User.RPC or similar.
module Brig.IO.Intra
  ( -- * Pushing & Journaling Events
    sendUserEvent,
    onConnectionEvent,
    onPropertyEvent,
    onClientEvent,

    -- * user subsystem interpretation for user events
    runEvents,

    -- * Conversations
    createConnectConv,
    acceptConnectConv,
    blockConv,
    upsertOne2OneConversation,

    -- * Clients
    rmClient,

    -- * Account Deletion
    rmUser,

    -- * Low Level API for Notifications
    notify,
  )
where

import Bilge hiding (head, options, requestId)
import Bilge.RPC
import Brig.API.Error (internalServerError)
import Brig.App
import Brig.Data.Connection
import Brig.Data.Connection qualified as Data
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.ConnectionStore qualified as E
import Brig.IO.Journal qualified as Journal
import Brig.IO.Logging
import Brig.RPC
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy
import Data.Qualified
import Data.Range
import Imports
import Network.AMQP qualified as Q
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Polysemy.TinyLog (TinyLog)
import System.Logger.Message hiding ((.=))
import Wire.API.Component
import Wire.API.Connection
import Wire.API.Conversation hiding (Member)
import Wire.API.Event.Conversation (Connect (Connect))
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Error
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Push.V2 qualified as V2
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Team.Member qualified as Team
import Wire.API.User
import Wire.API.UserEvent
import Wire.BackendNotificationQueueAccess
import Wire.Events
import Wire.FederationAPIAccess
import Wire.NotificationSubsystem
import Wire.Rpc
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Paging qualified as P
import Wire.Sem.Paging.Cassandra (InternalPaging)

-----------------------------------------------------------------------------
-- Event Handlers

sendUserEvent ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ConnectionStore InternalPaging) r,
    HasBrigFederationAccess m r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  UserId ->
  Maybe ConnId ->
  UserEvent ->
  Sem r ()
sendUserEvent orig conn e =
  dispatchNotifications orig conn e
    *> embed (journalEvent orig e)

runEvents ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ConnectionStore InternalPaging) r,
    HasBrigFederationAccess m r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  InterpreterFor Events r
runEvents = interpret \case
  -- FUTUREWORK(mangoiv): should this be in another module?
  GenerateUserEvent uid mconnid event -> sendUserEvent uid mconnid event
  GeneratePropertyEvent uid connid event -> onPropertyEvent uid connid event

onConnectionEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID, if any.
  Maybe ConnId ->
  -- | The event.
  ConnectionEvent ->
  Sem r ()
onConnectionEvent orig conn evt = do
  let from = ucFrom (ucConn evt)
  notify
    (ConnectionEvent evt)
    orig
    V2.RouteAny
    conn
    (pure $ NonEmpty.singleton from)

onPropertyEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  ConnId ->
  PropertyEvent ->
  Sem r ()
onPropertyEvent orig conn e =
  notify
    (PropertyEvent e)
    orig
    V2.RouteDirect
    (Just conn)
    (pure $ NonEmpty.singleton orig)

onClientEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  Sem r ()
onClientEvent orig conn e = do
  let event = ClientEvent e
  let rcpt = Recipient orig V2.RecipientClientsAll
  pushNotifications
    [ def
        { origin = Just orig,
          json = toJSONObject event,
          recipients = [rcpt],
          conn,
          apsData = toApsData event
        }
    ]

journalEvent :: (MonadReader Env m, MonadIO m) => UserId -> UserEvent -> m ()
journalEvent orig e = case e of
  UserActivated acc ->
    Journal.userActivate acc
  UserUpdated UserUpdatedData {eupName = Just name} ->
    Journal.userUpdate orig Nothing Nothing (Just name)
  UserUpdated UserUpdatedData {eupLocale = Just loc} ->
    Journal.userUpdate orig Nothing (Just loc) Nothing
  UserIdentityUpdated (UserIdentityUpdatedData _ (Just em) _) ->
    Journal.userUpdate orig (Just em) Nothing Nothing
  UserIdentityRemoved (UserIdentityRemovedData _ (Just em) _) ->
    Journal.userEmailRemove orig em
  UserDeleted {} ->
    Journal.userDelete orig
  _ ->
    pure ()

-------------------------------------------------------------------------------
-- Low-Level Event Notification

-- | Notify the origin user's contact list (first-level contacts),
-- as well as his other clients about a change to his user account
-- or profile.
dispatchNotifications ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ConnectionStore InternalPaging) r,
    HasBrigFederationAccess m r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  UserId ->
  Maybe ConnId ->
  UserEvent ->
  Sem r ()
dispatchNotifications orig conn e = case e of
  UserCreated {} -> pure ()
  UserSuspended {} -> pure ()
  UserResumed {} -> pure ()
  LegalHoldClientRequested {} -> notifyContacts event orig V2.RouteAny conn
  UserLegalHoldDisabled {} -> notifyContacts event orig V2.RouteAny conn
  UserLegalHoldEnabled {} -> notifyContacts event orig V2.RouteAny conn
  UserUpdated UserUpdatedData {..}
    -- This relies on the fact that we never change the locale AND something else.
    | isJust eupLocale -> notifySelf event orig V2.RouteDirect conn
    | otherwise -> notifyContacts event orig V2.RouteDirect conn
  UserActivated {} -> notifySelf event orig V2.RouteAny conn
  UserIdentityUpdated {} -> notifySelf event orig V2.RouteDirect conn
  UserIdentityRemoved {} -> notifySelf event orig V2.RouteDirect conn
  UserDeleted {} -> do
    -- n.b. Synchronously fetch the contact list on the current thread.
    -- If done asynchronously, the connections may already have been deleted.
    notifyUserDeletionLocals orig conn event
    notifyUserDeletionRemotes orig
  UserSessionRefreshSuggested -> notifySelf event orig V2.RouteDirect conn
  where
    event = UserEvent e

notifyUserDeletionLocals ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member Now r
  ) =>
  UserId ->
  Maybe ConnId ->
  Event ->
  Sem r ()
notifyUserDeletionLocals deleted conn event = do
  luid <- qualifyLocal' deleted
  -- first we send a notification to the deleted user's devices
  notify event deleted V2.RouteDirect conn (pure (NonEmpty.singleton deleted))
  -- then to all their connections
  connectionPages Nothing luid (toRange (Proxy @500))
  where
    handler :: [UserConnection] -> Sem r ()
    handler connections = do
      -- sent event to connections that are accepted
      case qUnqualified . ucTo <$> filter ((==) Accepted . ucStatus) connections of
        x : xs -> notify event deleted V2.RouteDirect conn (pure (x :| xs))
        [] -> pure ()
      -- also send a connection cancelled event to connections that are pending
      d <- tDomain <$> input
      forM_
        (filter ((==) Sent . ucStatus) connections)
        ( \uc -> do
            now <- toUTCTimeMillis <$> Now.get
            -- because the connections are going to be removed from the database anyway when a user gets deleted
            -- we don't need to save the updated connection state in the database
            -- note that we switch from and to users so that the "other" user becomes the recipient of the event
            let ucCancelled =
                  UserConnection
                    (qUnqualified (ucTo uc))
                    (Qualified (ucFrom uc) d)
                    Cancelled
                    now
                    (ucConvId uc)
            let e = ConnectionUpdated ucCancelled Nothing
            onConnectionEvent deleted conn e
        )

    connectionPages :: Maybe UserId -> Local UserId -> Range 1 500 Int32 -> Sem r ()
    connectionPages mbStart user pageSize = do
      page <- embed $ Data.lookupLocalConnections user mbStart pageSize
      case resultList page of
        [] -> pure ()
        xs -> do
          handler xs
          when (Data.resultHasMore page) $
            connectionPages (Just (maximum (qUnqualified . ucTo <$> xs))) user pageSize

notifyUserDeletionRemotes ::
  forall r m.
  ( Member TinyLog r,
    Member (Input (Local ())) r,
    Member (ConnectionStore InternalPaging) r,
    HasBrigFederationAccess m r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  UserId ->
  Sem r ()
notifyUserDeletionRemotes deleted = do
  luid <- qualifyLocal' deleted
  P.withChunks (\mps -> E.remoteConnectedUsersPaginated luid mps maxBound) fanoutNotifications
  where
    fanoutNotifications :: [Remote UserConnection] -> Sem r ()
    fanoutNotifications = mapM_ notifyBackend . bucketRemote

    notifyBackend :: Remote [UserConnection] -> Sem r ()
    notifyBackend ucs = do
      case tUnqualified (checked <$> ucs) of
        Nothing ->
          -- The user IDs cannot be more than 1000, so we can assume the range
          -- check will only fail because there are 0 User Ids.
          pure ()
        Just rangedUcs -> do
          luidDeleted <- qualifyLocal' deleted
          let remotes = mapRange (qUnqualified . ucTo) rangedUcs
              notif = UserDeletedConnectionsNotification (tUnqualified luidDeleted) remotes
              client = fedQueueClient @'OnUserDeletedConnectionsTag notif
          enqueueNotification Q.Persistent ucs client
          -- also sent connection cancelled events to the connections that are pending
          let remotePendingConnections = qualifyAs ucs <$> filter ((==) Sent . ucStatus) (fromRange rangedUcs)
          forM_ remotePendingConnections $ sendCancelledEvent luidDeleted

    sendCancelledEvent :: Local UserId -> Remote UserConnection -> Sem r ()
    sendCancelledEvent luidDeleted ruc = do
      let remoteUid :: Remote UserId = qUnqualified . ucTo <$> ruc
          req = NewConnectionRequest (tUnqualified luidDeleted) Nothing (qUnqualified $ tUntagged remoteUid) RemoteRescind
      Log.info $ msg @Text "Brig-federation: sending connection action to remote backend"
      result <- runFederatedEither remoteUid $ fedClient @'Brig @"send-connection-action" req
      case result of
        Left e ->
          Log.err $
            field "error" (show e)
              . msg (val "An error occurred while sending a connection cancelled event to a remote backend.")
        Right _ -> pure ()

-- | (Asynchronously) notifies other users of events.
notify ::
  (Member NotificationSubsystem r) =>
  Event ->
  -- | Origin user, TODO: Delete
  UserId ->
  -- | Push routing strategy.
  V2.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  -- | Users to notify.
  Sem r (NonEmpty UserId) ->
  Sem r ()
notify event orig route conn recipients = do
  rs <- (\u -> Recipient u RecipientClientsAll) <$$> recipients
  let push =
        def
          { origin = Just orig,
            json = toJSONObject event,
            recipients = toList rs,
            conn,
            route,
            apsData = toApsData event
          }
  void $ pushNotificationAsync push

notifySelf ::
  (Member NotificationSubsystem r) =>
  Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  V2.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  Sem r ()
notifySelf event orig route conn =
  notify event orig route conn (pure (NonEmpty.singleton orig))

notifyContacts ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r
  ) =>
  Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  V2.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  Sem r ()
notifyContacts event orig route conn = do
  notify event orig route conn $
    (:|) orig <$> liftA2 (++) contacts teamContacts
  where
    contacts :: Sem r [UserId]
    contacts = embed $ lookupContactList orig

    teamContacts :: Sem r [UserId]
    teamContacts = screenMemberList <$> getTeamContacts orig
    -- If we have a truncated team, we just ignore it all together to avoid very large fanouts
    --
    screenMemberList :: Maybe Team.TeamMemberList -> [UserId]
    screenMemberList (Just mems)
      | mems ^. Team.teamMemberListType == Team.ListComplete =
          view Team.userId <$> mems ^. Team.teamMembers
    screenMemberList _ = []

-------------------------------------------------------------------------------
-- Conversation Management

-- | Calls 'Wire.ConversationSubsystem.createConnectConversation'.
createLocalConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Local UserId ->
  Maybe Text ->
  Maybe ConnId ->
  Sem r ConvId
createLocalConnectConv from to cname conn = do
  Log.debug $
    logConnection (tUnqualified from) (tUntagged to)
      . remote "galley"
      . msg (val "Creating connect conversation")
  let req =
        path "/i/conversations/connect"
          . zUser (tUnqualified from)
          . maybe id (header "Z-Connection" . fromConnId) conn
          . contentJson
          . lbytes (encode $ Connect (tUntagged to) Nothing cname Nothing)
          . expect2xx
  r <- embed $ galleyRequest POST req
  maybe (error "invalid conv id") pure $
    fromByteString $
      getHeader' "Location" r

createConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Qualified UserId ->
  Maybe Text ->
  Maybe ConnId ->
  (AppT r) (Qualified ConvId)
createConnectConv from to cname conn = do
  lfrom <- ensureLocal from
  lto <- ensureLocal to
  tUntagged . qualifyAs lfrom
    <$> liftSem (createLocalConnectConv lfrom lto cname conn)

-- | Calls 'Galley.API.acceptConvH'.
acceptLocalConnectConv ::
  (Member (Embed HttpClientIO) r, Member TinyLog r) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r OwnConversation
acceptLocalConnectConv from conn cnv = do
  Log.debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Accepting connect conversation")
  embed $ galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "accept", "v2"]
        . zUser (tUnqualified from)
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

acceptConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  AppT r OwnConversation
acceptConnectConv from conn =
  foldQualified
    from
    (liftSem . acceptLocalConnectConv from conn . tUnqualified)
    (const (throwM federationNotImplemented))

blockConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r ()
blockConv lusr qcnv = do
  Log.debug $
    remote "galley"
      . field "conv" (toByteString . qUnqualified $ qcnv)
      . field "domain" (toByteString . qDomain $ qcnv)
      . msg (val "Blocking conversation")
  embed . void $ galleyRequest PUT req
  where
    req =
      paths
        [ "i",
          "conversations",
          toByteString' (qDomain qcnv),
          toByteString' (qUnqualified qcnv),
          "block"
        ]
        . zUser (tUnqualified lusr)
        . expect2xx

upsertOne2OneConversation ::
  ( MonadReader Env m,
    MonadUnliftIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  UpsertOne2OneConversationRequest ->
  m ()
upsertOne2OneConversation urequest = do
  response <- galleyRequest POST req
  case Bilge.statusCode response of
    200 -> pure ()
    _ -> throwM internalServerError
  where
    req =
      paths ["i", "conversations", "one2one", "upsert"]
        . header "Content-Type" "application/json"
        . lbytes (encode urequest)

-------------------------------------------------------------------------------
-- User management

-- | Calls Galley's endpoint with the internal route ID "delete-user", as well
-- as gundeck and cargohold.
rmUser ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  [Asset] ->
  Sem r ()
rmUser usr asts = do
  Log.debug $
    remote "gundeck"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  cleanupUser usr
  Log.debug $
    remote "galley"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  embed $ void $ galleyRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  Log.debug $
    remote "cargohold"
      . field "user" (toByteString usr)
      . msg (val "remove profile assets")
  -- Note that we _may_ not get a 2xx response code from cargohold (e.g., client has
  -- deleted the asset "directly" with cargohold; on our side, we just do our best to
  -- delete it in case it is still there
  embed $ forM_ asts $ \ast ->
    cargoholdRequest DELETE (paths ["assets/v3", toByteString' $ assetKey ast] . zUser usr)

-------------------------------------------------------------------------------
-- Client management

-- | Calls 'Galley.API.rmClientH', as well as gundeck.
rmClient ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
rmClient u c = do
  let cid = toByteString' c
  Log.debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "remove client")
  let p = paths ["i", "clients", cid]
  embed $ void $ galleyRequest DELETE (p . zUser u . expect expected)
  -- for_ clabel rmClientCookie
  Log.debug $
    remote "gundeck"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "unregister push client")
  unregisterPushClient u c
  where
    expected = [status200, status204, status404]

-------------------------------------------------------------------------------
-- Team Management

-- | Only works on 'BindingTeam's! The list of members returned is potentially truncated.
--
-- Calls 'Galley.API.getBindingTeamMembersH'.
getTeamContacts ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r
  ) =>
  UserId ->
  Sem r (Maybe Team.TeamMemberList)
getTeamContacts u = do
  Log.debug $ remote "galley" . msg (val "Get team contacts")
  rs <- embed $ galleyRequest GET req
  embed $ case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team", "members"]
        . expect [status200, status404]
