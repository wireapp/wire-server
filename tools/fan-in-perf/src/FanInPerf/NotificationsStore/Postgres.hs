{-# LANGUAGE RecordWildCards #-}

module FanInPerf.NotificationsStore.Postgres where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Domain (Domain, domainText)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V7 qualified as UUIDv7
import Data.Vector qualified as V
import FanInPerf.NotificationsStore
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.Postgres

interpretFanInNotificationsStoreToPostgres ::
  (PGConstraints r, Member (Input (Local ())) r) =>
  TxSessions.IsolationLevel ->
  InterpreterFor FanInNotificationsStore r
interpretFanInNotificationsStoreToPostgres isolationLevel = interpret $ \case
  PushViaFanIn push -> pushViaFanInImpl isolationLevel push
  FetchNotifications stream cursor n -> fetchNotificationsImpl stream cursor n
  GetLastNotificationId stream -> getLastNotificationIdImpl stream
  InitAcks uid cid stream -> initAcksImpl uid cid stream
  AckNotification uid cid stream nid -> ackNotificationImpl uid cid stream nid

pushViaFanInImpl ::
  (PGConstraints r, Member (Input (Local ())) r) =>
  TxSessions.IsolationLevel ->
  FanInPush ->
  Sem r ()
pushViaFanInImpl isolationLevel push = do
  notifId <- embed @IO genNotificationId
  loc <- inputQualifyLocal ()
  let payload = Aeson.toJSON push.json
  runTransactionWithRetry isolationLevel TxSessions.Write do
    forM_ (normalizeTargets push.targets) \target -> do
      case target of
        TargetUser uid ->
          pushUserNotification uid.toUUID notifId payload push.origin
        TargetUserClients (uid, clientIds) ->
          pushClientNotifications uid.toUUID clientIds notifId payload push.origin
        TargetTeam tid ->
          pushTeamNotification tid.toUUID notifId payload push.origin
        TargetEpoch (gid, epoch) ->
          pushEpochNotification gid.unGroupId epoch.epochNumber notifId payload push.origin
        TargetConnections quid -> do
          let Qualified uid domain = quid
              locDomain = loc.tUntagged.qDomain
          if locDomain == domain
            then pushLocalConnectionNotification uid.toUUID notifId payload push.origin
            else pushRemoteConnectionNotification (domainText domain) uid.toUUID notifId payload push.origin

-- | Dedupe and sort targets. Duplicates would violate primary keys (and make
-- the @last_*@ upserts affect a row twice); a stable order avoids deadlocks
-- between concurrent pushes locking the same rows in opposite order.
normalizeTargets :: [Target] -> [Target]
normalizeTargets = mergeClients . map NE.head . NE.group . sortOn targetKey
  where
    mergeClients = \case
      TargetUserClients (u, cs) : TargetUserClients (u', cs') : rest
        | u == u' -> mergeClients (TargetUserClients (u, NE.sort (NE.nub (cs <> cs'))) : rest)
      TargetUserClients (u, cs) : rest -> TargetUserClients (u, NE.sort (NE.nub cs)) : mergeClients rest
      t : rest -> t : mergeClients rest
      [] -> []

targetKey :: Target -> (Int, UUID, Maybe Domain, Word64, ByteString)
targetKey = \case
  TargetUser u -> (0, u.toUUID, Nothing, 0, mempty)
  TargetUserClients (u, _) -> (1, u.toUUID, Nothing, 0, mempty)
  TargetTeam t -> (2, t.toUUID, Nothing, 0, mempty)
  TargetEpoch (g, e) -> (3, UUID.nil, Nothing, fromIntegral e.epochNumber, g.unGroupId)
  TargetConnections (Qualified u d) -> (4, u.toUUID, Just d, 0, mempty)

-- | Time-ordered (UUIDv7) notification id.
genNotificationId :: IO (Id a)
genNotificationId = Id <$> UUIDv7.genUUID

-- | Milliseconds since the Unix epoch encoded in the first 48 bits of a UUIDv7.
notificationIdMillis :: UUID -> Word64
notificationIdMillis u =
  let (w1, w2, _, _) = UUID.toWords u
   in (fromIntegral w1 `shiftL` 16) .|. (fromIntegral w2 `shiftR` 16)

fetchNotificationsImpl ::
  (PGConstraints r) =>
  Stream ->
  UUID ->
  Int ->
  Sem r [(UUID, Value)]
fetchNotificationsImpl stream cursor n =
  V.toList <$> case stream of
    StreamUser uid -> runStatement (uid.toUUID, cursor, limit') fetchUserStatement
    StreamClient uid cid ->
      runStatement (uid.toUUID, clientInt cid, cursor, limit') fetchClientStatement
    StreamTeam tid -> runStatement (tid.toUUID, cursor, limit') fetchTeamStatement
    StreamEpoch gid epoch ->
      runStatement (gid.unGroupId, fromIntegral epoch.epochNumber, cursor, limit') fetchEpochStatement
  where
    limit' = fromIntegral n :: Int64

fetchUserStatement :: Statement (UUID, UUID, Int64) (V.Vector (UUID, Value))
fetchUserStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from user_notifications
    where user_id = ($1 :: uuid) and notification_id > ($2 :: uuid)
    order by notification_id
    limit ($3 :: int8)
  |]

fetchClientStatement :: Statement (UUID, Int64, UUID, Int64) (V.Vector (UUID, Value))
fetchClientStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from client_notifications
    where user_id = ($1 :: uuid) and client_id = ($2 :: int8) and notification_id > ($3 :: uuid)
    order by notification_id
    limit ($4 :: int8)
  |]

fetchTeamStatement :: Statement (UUID, UUID, Int64) (V.Vector (UUID, Value))
fetchTeamStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from team_notifications
    where team_id = ($1 :: uuid) and notification_id > ($2 :: uuid)
    order by notification_id
    limit ($3 :: int8)
  |]

fetchEpochStatement :: Statement (ByteString, Int64, UUID, Int64) (V.Vector (UUID, Value))
fetchEpochStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from epoch_notifications
    where group_id = ($1 :: bytea) and epoch = ($2 :: int8) and notification_id > ($3 :: uuid)
    order by notification_id
    limit ($4 :: int8)
  |]

getLastNotificationIdImpl ::
  (PGConstraints r) =>
  Stream ->
  Sem r (Maybe UUID)
getLastNotificationIdImpl = \case
  StreamUser uid -> runStatement uid.toUUID lastUserStatement
  StreamClient uid cid -> runStatement (uid.toUUID, clientInt cid) lastClientStatement
  StreamTeam tid -> runStatement tid.toUUID lastTeamStatement
  StreamEpoch gid epoch -> runStatement (gid.unGroupId, fromIntegral epoch.epochNumber) lastEpochStatement

lastUserStatement :: Statement UUID (Maybe UUID)
lastUserStatement =
  [maybeStatement|
    select notification_id :: uuid from last_user_notifications where user_id = ($1 :: uuid)
  |]

lastClientStatement :: Statement (UUID, Int64) (Maybe UUID)
lastClientStatement =
  [maybeStatement|
    select notification_id :: uuid from last_client_notifications
    where user_id = ($1 :: uuid) and client_id = ($2 :: int8)
  |]

lastTeamStatement :: Statement UUID (Maybe UUID)
lastTeamStatement =
  [maybeStatement|
    select notification_id :: uuid from last_team_notifications where team_id = ($1 :: uuid)
  |]

lastEpochStatement :: Statement (ByteString, Int64) (Maybe UUID)
lastEpochStatement =
  [maybeStatement|
    select notification_id :: uuid from last_epoch_notifications
    where group_id = ($1 :: bytea) and epoch = ($2 :: int8)
  |]

clientInt :: ClientId -> Int64
clientInt c = fromIntegral c.clientToWord64

-- | Ack rows belong to the client (@uid@, @cid@); the stream's own key selects
-- the latest notification. Conflict targets are the ack tables' primary keys
-- (the RFC's SQL lacks them).
initAcksImpl :: (PGConstraints r) => UserId -> ClientId -> Stream -> Sem r (Maybe UUID)
initAcksImpl uid cid = \case
  StreamUser u -> runStatement (uid.toUUID, clientInt cid, u.toUUID) initUserAckStatement
  StreamClient u c -> runStatement (uid.toUUID, clientInt cid, u.toUUID, clientInt c) initClientAckStatement
  StreamTeam t -> runStatement (uid.toUUID, clientInt cid, t.toUUID) initTeamAckStatement
  StreamEpoch g e -> runStatement (uid.toUUID, clientInt cid, g.unGroupId, fromIntegral e.epochNumber) initEpochAckStatement

initUserAckStatement :: Statement (UUID, Int64, UUID) (Maybe UUID)
initUserAckStatement =
  [maybeStatement|
    insert into user_notification_acks (user_id, client_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), notification_id
      from last_user_notifications where user_id = ($3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

initClientAckStatement :: Statement (UUID, Int64, UUID, Int64) (Maybe UUID)
initClientAckStatement =
  [maybeStatement|
    insert into client_notification_acks (user_id, client_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), notification_id
      from last_client_notifications where user_id = ($3 :: uuid) and client_id = ($4 :: int8)
      on conflict (user_id, client_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- RFC: selected from team_notifications with an invalid GROUP BY; the last_*
-- table holds exactly the wanted id.
initTeamAckStatement :: Statement (UUID, Int64, UUID) (Maybe UUID)
initTeamAckStatement =
  [maybeStatement|
    insert into team_notification_acks (user_id, client_id, team_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), team_id, notification_id
      from last_team_notifications where team_id = ($3 :: uuid)
      on conflict (user_id, client_id, team_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- RFC: wrote the nonexistent conversation_notification_acks with EXCLUDED.notificationId.
initEpochAckStatement :: Statement (UUID, Int64, ByteString, Int64) (Maybe UUID)
initEpochAckStatement =
  [maybeStatement|
    insert into epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), group_id, epoch, notification_id
      from last_epoch_notifications where group_id = ($3 :: bytea) and epoch = ($4 :: int8)
      on conflict (user_id, client_id, group_id, epoch) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- | @greatest@: a late or repeated ack never moves the ack backwards.
ackNotificationImpl :: (PGConstraints r) => UserId -> ClientId -> Stream -> UUID -> Sem r ()
ackNotificationImpl uid cid stream nid = case stream of
  StreamUser _ -> runStatement (uid.toUUID, clientInt cid, nid) ackUserStatement
  StreamClient _ _ -> runStatement (uid.toUUID, clientInt cid, nid) ackClientStatement
  StreamTeam t -> runStatement (uid.toUUID, clientInt cid, t.toUUID, nid) ackTeamStatement
  StreamEpoch g e -> runStatement (uid.toUUID, clientInt cid, g.unGroupId, fromIntegral e.epochNumber, nid) ackEpochStatement

ackUserStatement :: Statement (UUID, Int64, UUID) ()
ackUserStatement =
  [resultlessStatement|
    insert into user_notification_acks (user_id, client_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = greatest(user_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackClientStatement :: Statement (UUID, Int64, UUID) ()
ackClientStatement =
  [resultlessStatement|
    insert into client_notification_acks (user_id, client_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = greatest(client_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackTeamStatement :: Statement (UUID, Int64, UUID, UUID) ()
ackTeamStatement =
  [resultlessStatement|
    insert into team_notification_acks (user_id, client_id, team_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid, $4 :: uuid)
      on conflict (user_id, client_id, team_id) do update
        set last_notification_id = greatest(team_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackEpochStatement :: Statement (UUID, Int64, ByteString, Int64, UUID) ()
ackEpochStatement =
  [resultlessStatement|
    insert into epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: bytea, $4 :: int8, $5 :: uuid)
      on conflict (user_id, client_id, group_id, epoch) do update
        set last_notification_id = greatest(epoch_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

pushUserNotification ::
  UUID ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushUserNotification uid notifId payload origin = do
  Tx.statement
    (uid, notifId.toUUID, payload, fmap (.toUUID) origin)
    insertUserNotificationStatement
  Tx.statement
    (uid, notifId.toUUID)
    upsertLastUserNotificationStatement
  where
    insertUserNotificationStatement :: Statement (UUID, UUID, Value, Maybe UUID) ()
    insertUserNotificationStatement =
      [resultlessStatement|
        insert into user_notifications (user_id, notification_id, payload, origin)
          values ($1 :: uuid, $2 :: uuid, $3 :: jsonb, $4 :: uuid?)
      |]

    upsertLastUserNotificationStatement :: Statement (UUID, UUID) ()
    upsertLastUserNotificationStatement =
      [resultlessStatement|
        insert into last_user_notifications (user_id, notification_id)
          values ($1 :: uuid, $2 :: uuid)
          on conflict (user_id) do update
            set notification_id = greatest(last_user_notifications.notification_id, excluded.notification_id)
      |]

pushClientNotifications ::
  UUID ->
  NE.NonEmpty ClientId ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushClientNotifications uid clientIds notifId payload origin = do
  let clientIds' = V.fromList $ map clientInt (NE.toList clientIds) :: V.Vector Int64
  Tx.statement
    (uid, clientIds', notifId.toUUID, payload, fmap (.toUUID) origin)
    insertClientNotificationsStatement
  Tx.statement
    (uid, clientIds', notifId.toUUID)
    upsertLastClientNotificationsStatement
  where
    insertClientNotificationsStatement :: Statement (UUID, V.Vector Int64, UUID, Value, Maybe UUID) ()
    insertClientNotificationsStatement =
      [resultlessStatement|
        insert into client_notifications (user_id, client_id, notification_id, payload, origin)
          select $1 :: uuid, unnest($2 :: int8[]), $3 :: uuid, $4 :: jsonb, $5 :: uuid?
      |]

    upsertLastClientNotificationsStatement :: Statement (UUID, V.Vector Int64, UUID) ()
    upsertLastClientNotificationsStatement =
      [resultlessStatement|
        insert into last_client_notifications (user_id, client_id, notification_id)
          select $1 :: uuid, unnest($2 :: int8[]), $3 :: uuid
          on conflict (user_id, client_id) do update
            set notification_id = greatest(last_client_notifications.notification_id, excluded.notification_id)
      |]

pushTeamNotification ::
  UUID ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushTeamNotification tid notifId payload origin = do
  Tx.statement
    (tid, notifId.toUUID, payload, fmap (.toUUID) origin)
    insertTeamNotificationStatement
  Tx.statement
    (tid, notifId.toUUID)
    upsertLastTeamNotificationStatement
  where
    insertTeamNotificationStatement :: Statement (UUID, UUID, Value, Maybe UUID) ()
    insertTeamNotificationStatement =
      [resultlessStatement|
        insert into team_notifications (team_id, notification_id, payload, origin)
          values ($1 :: uuid, $2 :: uuid, $3 :: jsonb, $4 :: uuid?)
      |]

    upsertLastTeamNotificationStatement :: Statement (UUID, UUID) ()
    upsertLastTeamNotificationStatement =
      [resultlessStatement|
        insert into last_team_notifications (team_id, notification_id)
          values ($1 :: uuid, $2 :: uuid)
          on conflict (team_id) do update
            set notification_id = greatest(last_team_notifications.notification_id, excluded.notification_id)
      |]

pushEpochNotification ::
  ByteString ->
  Word64 ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushEpochNotification gid epoch notifId payload origin = do
  let epoch' = fromIntegral epoch :: Int64
  Tx.statement
    (gid, epoch', notifId.toUUID, payload, fmap (.toUUID) origin)
    insertEpochNotificationStatement
  Tx.statement
    (gid, epoch', notifId.toUUID)
    upsertLastEpochNotificationStatement
  where
    insertEpochNotificationStatement :: Statement (ByteString, Int64, UUID, Value, Maybe UUID) ()
    insertEpochNotificationStatement =
      [resultlessStatement|
        insert into epoch_notifications (group_id, epoch, notification_id, payload, origin)
          values ($1 :: bytea, $2 :: bigint, $3 :: uuid, $4 :: jsonb, $5 :: uuid?)
      |]

    upsertLastEpochNotificationStatement :: Statement (ByteString, Int64, UUID) ()
    upsertLastEpochNotificationStatement =
      [resultlessStatement|
        insert into last_epoch_notifications (group_id, epoch, notification_id)
          values ($1 :: bytea, $2 :: bigint, $3 :: uuid)
          on conflict (group_id, epoch) do update
            set notification_id = greatest(last_epoch_notifications.notification_id, excluded.notification_id)
      |]

pushLocalConnectionNotification ::
  UUID ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushLocalConnectionNotification uid notifId payload origin = do
  Tx.statement
    (uid, notifId.toUUID, payload, fmap (.toUUID) origin)
    insertLocalConnectionNotificationStatement
  Tx.statement
    (uid, notifId.toUUID)
    upsertLastLocalConnectionNotificationStatement
  where
    insertLocalConnectionNotificationStatement :: Statement (UUID, UUID, Value, Maybe UUID) ()
    insertLocalConnectionNotificationStatement =
      [resultlessStatement|
        insert into local_connection_notifications (user_id, notification_id, payload, origin)
          values ($1 :: uuid, $2 :: uuid, $3 :: jsonb, $4 :: uuid?)
      |]

    upsertLastLocalConnectionNotificationStatement :: Statement (UUID, UUID) ()
    upsertLastLocalConnectionNotificationStatement =
      [resultlessStatement|
        insert into last_local_connection_notifications (user_id, notification_id)
          values ($1 :: uuid, $2 :: uuid)
          on conflict (user_id) do update
            set notification_id = greatest(last_local_connection_notifications.notification_id, excluded.notification_id)
      |]

pushRemoteConnectionNotification ::
  Text ->
  UUID ->
  Id a ->
  Value ->
  Maybe UserId ->
  Tx.Transaction ()
pushRemoteConnectionNotification domain uid notifId payload origin = do
  Tx.statement
    (domain, uid, notifId.toUUID, payload, fmap (.toUUID) origin)
    insertRemoteConnectionNotificationStatement
  Tx.statement
    (domain, uid, notifId.toUUID)
    upsertLastRemoteConnectionNotificationStatement
  where
    insertRemoteConnectionNotificationStatement :: Statement (Text, UUID, UUID, Value, Maybe UUID) ()
    insertRemoteConnectionNotificationStatement =
      [resultlessStatement|
        insert into remote_connection_notifications (user_domain, user_id, notification_id, payload, origin)
          values ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: jsonb, $5 :: uuid?)
      |]

    upsertLastRemoteConnectionNotificationStatement :: Statement (Text, UUID, UUID) ()
    upsertLastRemoteConnectionNotificationStatement =
      [resultlessStatement|
        insert into last_remote_connection_notifications (user_domain, user_id, notification_id)
          values ($1 :: text, $2 :: uuid, $3 :: uuid)
          on conflict (user_domain, user_id) do update
            set notification_id = greatest(last_remote_connection_notifications.notification_id, excluded.notification_id)
      |]
