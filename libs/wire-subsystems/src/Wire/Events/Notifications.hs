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

module Wire.Events.Notifications
  ( dispatchNotifications,
    notify,
    notifySelf,
    notifyContacts,
  )
where

import Control.Lens (view, (?~), (^.))
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Qualified
import Data.Range
import Imports hiding (local)
import Polysemy
import Polysemy.Input (Input, input)
import Polysemy.TinyLog (TinyLog)
import System.Logger.Message hiding ((.=))
import Servant.Client.Core.RunClient (RunClient)
import Wire.API.Connection
import Wire.API.Federation.API (FederationMonad, fedClient, fedQueueClient)
import Wire.API.Federation.API.Brig (NewConnectionRequest (..), RemoteConnectionAction (..))
import Wire.API.Federation.API.Brig.Notifications (BrigNotificationTag (..), UserDeletedConnectionsNotification (..))
import Wire.API.Federation.Component (Component (Brig))
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Push.V2 qualified as V2
import Wire.API.Team.Member (ListType (ListComplete), TeamMemberList, teamMemberListType, teamMembers)
import Wire.API.Team.Member qualified as TM
import Wire.API.User
import Wire.API.UserEvent
import Wire.BackendNotificationSubsystem
import Wire.ConnectionStore (ConnectionStore)
import Wire.FederationAPIAccess
import Wire.ConnectionStore qualified as CS
import Wire.ConnectionStore.Types (resultHasMore, resultList)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPI
import Wire.NotificationSubsystem
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Paging qualified as P
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | Helper to qualify a value as local using Input (Local ())
qualifyLocal' :: (Member (Input (Local ())) r) => a -> Sem r (Local a)
qualifyLocal' a = flip toLocalUnsafe a . tDomain <$> input

-- | Notify users about events
dispatchNotifications ::
  forall fedM r.
  ( Member (ConnectionStore InternalPaging) r,
    Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member BackendNotificationSubsystem r,
    Member (FederationAPIAccess fedM) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
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
  where
    event = UserEvent e

-- | Notify local users about a user deletion
notifyUserDeletionLocals ::
  forall r.
  ( Member (ConnectionStore InternalPaging) r,
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
  notify event deleted V2.RouteDirect conn (pure (deleted :| []))
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
      page <- CS.lookupLocalConnections user mbStart pageSize
      case resultList page of
        [] -> pure ()
        xs -> do
          handler xs
          when (resultHasMore page) $
            connectionPages (Just (maximum (qUnqualified . ucTo <$> xs))) user pageSize

-- | Notify remote backends about a user deletion
notifyUserDeletionRemotes ::
  forall fedM r.
  ( Member TinyLog r,
    Member (Input (Local ())) r,
    Member (ConnectionStore InternalPaging) r,
    Member BackendNotificationSubsystem r,
    Member (FederationAPIAccess fedM) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  UserId ->
  Sem r ()
notifyUserDeletionRemotes deleted = do
  luid <- qualifyLocal' deleted
  P.withChunks (\mps -> CS.remoteConnectedUsersPaginated luid mps maxBound) fanoutNotifications
  where
    fanoutNotifications :: [Remote UserConnection] -> Sem r ()
    fanoutNotifications = mapM_ notifyBackend . bucketRemote

    notifyBackend :: Remote [UserConnection] -> Sem r ()
    notifyBackend ucs = do
      case tUnqualified (checked <$> ucs) of
        Nothing -> pure () -- No connections or invalid range
        Just rangedUcs -> do
          luidDeleted <- qualifyLocal' deleted
          ownDomain <- input

          -- Send async user deleted notification
          let notif =
                UserDeletedConnectionsNotification
                  (tUnqualified luidDeleted)
                  (mapRange (qUnqualified . ucTo) rangedUcs)
          sendBackendNotification
            ownDomain
            (tDomain ucs)
            (fedQueueClient @'OnUserDeletedConnectionsTag notif)

          -- Send connection cancelled events for pending connections
          let remotePendingConns =
                qualifyAs ucs <$>
                filter ((==) Sent . ucStatus) (fromRange rangedUcs)
          forM_ remotePendingConns $ sendCancelledEvent luidDeleted

    sendCancelledEvent :: Local UserId -> Remote UserConnection -> Sem r ()
    sendCancelledEvent luidDeleted ruc = do
      let req =
            NewConnectionRequest
              (tUnqualified luidDeleted)
              Nothing
              (tUnqualified (qUnqualified . ucTo <$> ruc))
              RemoteRescind
      result <- runFederatedEither ruc (fedClient @'Brig @"send-connection-action" req)
      case result of
        Left e ->
          Log.err $
            field "error" (show e)
              . msg (val "Failed to send connection cancelled to remote")
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

-- | Notify only the origin user
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
  notify event orig route conn (pure (orig :| []))

-- | Notify the origin user's contacts (connections and team members)
notifyContacts ::
  forall r.
  ( Member (ConnectionStore InternalPaging) r,
    Member GalleyAPIAccess r,
    Member NotificationSubsystem r
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
    contacts = CS.lookupContactList orig

    teamContacts :: Sem r [UserId]
    teamContacts = screenMemberList <$> GalleyAPI.getTeamContacts orig
    -- If we have a truncated team, we just ignore it all together to avoid very large fanouts
    --
    screenMemberList :: Maybe TeamMemberList -> [UserId]
    screenMemberList (Just mems)
      | mems ^. teamMemberListType == ListComplete =
          view TM.userId <$> mems ^. teamMembers
    screenMemberList _ = []

-- | Event handler for connection events
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
    (pure $ from :| [])

-- | Convert events to APS data for iOS push notifications
toApsData :: Event -> Maybe V2.ApsData
toApsData (ConnectionEvent (ConnectionUpdated uc name)) =
  case (ucStatus uc, name) of
    (MissingLegalholdConsent, _) -> Nothing
    (Pending, n) -> apsConnRequest <$> n
    (Accepted, n) -> apsConnAccept <$> n
    (Blocked, _) -> Nothing
    (Ignored, _) -> Nothing
    (Sent, _) -> Nothing
    (Cancelled, _) -> Nothing
  where
    apsConnRequest n =
      V2.apsData (V2.ApsLocKey "push.notification.connection.request") [fromName n]
        & V2.apsSound ?~ V2.ApsSound "new_message_apns.caf"
    apsConnAccept n =
      V2.apsData (V2.ApsLocKey "push.notification.connection.accepted") [fromName n]
        & V2.apsSound ?~ V2.ApsSound "new_message_apns.caf"
toApsData _ = Nothing
