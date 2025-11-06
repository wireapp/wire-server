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

module Wire.Events.Interpreter
  ( runEvents,
    onConnectionEvent,
    onPropertyEvent,
    onClientEvent,
    notify,
    toApsData,
  )
where

import Control.Lens ((?~))
import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Qualified (Local)
import Imports
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Connection
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Push.V2 qualified as V2
import Wire.API.User (Name (..))
import Wire.API.UserEvent
import Wire.AWSSubsystem (AWSSubsystem)
import Wire.ConnectionStore (ConnectionStore)
import Wire.Events
import Wire.Events.Journal qualified as Journal
import Wire.Events.Notifications qualified as Notifications
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | Interpreter for the Events effect
runEvents ::
  ( Member (Embed IO) r,
    Member NotificationSubsystem r,
    Member AWSSubsystem r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  InterpreterFor Events r
runEvents = interpret \case
  GenerateUserEvent uid mconnid event -> sendUserEvent uid mconnid event
  GeneratePropertyEvent uid connid event -> onPropertyEvent uid connid event

sendUserEvent ::
  ( Member (Embed IO) r,
    Member AWSSubsystem r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  Maybe ConnId ->
  UserEvent ->
  Sem r ()
sendUserEvent orig conn e = do
  Notifications.dispatchNotifications orig conn e
  Journal.journalUserEvent orig e

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
    (pure $ orig :| [])

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

-- | (Asynchronously) notifies other users of events.
notify ::
  (Member NotificationSubsystem r) =>
  Event ->
  -- | Origin user
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
