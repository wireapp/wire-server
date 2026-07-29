{-# LANGUAGE RecordWildCards #-}

module Wire.FanInNotificationsStore.Postgres where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Domain (domainText)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V7 qualified as UUIDv7
import Data.Vector qualified as V
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.FanInNotificationsStore
import Wire.Postgres

type FanInNotificationsStorePostgresEffectConstraints r =
  PGConstraints r

interpretFanInNotificationsStoreToPostgres ::
  (FanInNotificationsStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  InterpreterFor FanInNotificationsStore r
interpretFanInNotificationsStoreToPostgres = interpret $ \case
  PushViaFanIn push -> pushViaFanInImpl push

pushViaFanInImpl ::
  (FanInNotificationsStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  FanInPush ->
  Sem r ()
pushViaFanInImpl push = do
  notifId <- embed @IO genNotificationId
  loc <- inputQualifyLocal ()
  let payload = Aeson.toJSON push.json
  runTransactionWithRetry TxSessions.Serializable TxSessions.Write do
    forM_ push.targets \target -> do
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

genNotificationId :: IO (Id a)
genNotificationId = do
  uuidV7 <- UUIDv7.genUUID
  let uuidText = show uuidV7
  case UUID.fromString uuidText of
    Nothing -> genNotificationId
    Just uuid -> pure (Id uuid)

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
  let clientIds' = V.fromList $ map (fromIntegral . (.clientToWord64)) (NE.toList clientIds) :: V.Vector Int64
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
    (TE.decodeUtf8 gid, epoch', notifId.toUUID, payload, fmap (.toUUID) origin)
    insertEpochNotificationStatement
  Tx.statement
    (TE.decodeUtf8 gid, epoch', notifId.toUUID)
    upsertLastEpochNotificationStatement
  where
    insertEpochNotificationStatement :: Statement (Text, Int64, UUID, Value, Maybe UUID) ()
    insertEpochNotificationStatement =
      [resultlessStatement|
        insert into epoch_notifications (group_id, epoch, notification_id, payload, origin)
          values ($1 :: text, $2 :: bigint, $3 :: uuid, $4 :: jsonb, $5 :: uuid?)
      |]

    upsertLastEpochNotificationStatement :: Statement (Text, Int64, UUID) ()
    upsertLastEpochNotificationStatement =
      [resultlessStatement|
        insert into last_epoch_notifications (group_id, epoch, notification_id)
          values ($1 :: text, $2 :: bigint, $3 :: uuid)
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

