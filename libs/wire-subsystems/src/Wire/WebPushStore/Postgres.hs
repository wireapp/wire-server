{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.WebPushStore.Postgres
  ( interpretWebPushStoreToPostgres,
  )
where

import Control.Lens ((^.))
import Data.Id
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Push.V2.WebSubscription
import Wire.Postgres
import Wire.WebPushStore

-- | Postgres interpreter for 'WebPushStore'. Mirrors
-- 'Wire.CodeStore.Postgres.interpretCodeStoreToPostgres'.
interpretWebPushStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor WebPushStore r
interpretWebPushStoreToPostgres = interpret $ \case
  InsertSubscription uid sub conn -> insertSubscriptionImpl uid sub conn
  LookupSubscriptions uid -> lookupSubscriptionsImpl uid
  DeleteSubscription uid endpoint -> deleteSubscriptionImpl uid endpoint
  DeleteAllForUser uid -> deleteAllForUserImpl uid
  PurgeExpired uid -> purgeExpiredImpl uid

insertSubscriptionImpl ::
  (PGConstraints r) =>
  UserId ->
  WebPushSubscription ->
  ConnId ->
  Sem r ()
insertSubscriptionImpl uid sub conn =
  runStatement
    ( uid,
      sub ^. wpsClient,
      sub ^. wpsEndpoint,
      sub ^. (wpsKeys . wpkP256dh),
      sub ^. (wpsKeys . wpkAuth),
      sub ^. wpsExpirationTime,
      conn
    )
    insertSub

insertSub ::
  Hasql.Statement
    (UserId, ClientId, EndpointUrl, P256dhKey, AuthSecret, Maybe Word64, ConnId)
    ()
insertSub =
  lmapPG
    [resultlessStatement|
      INSERT INTO webpush_subscriptions
        (user_id, client_id, endpoint, p256dh, auth, expiration, conn_id)
      VALUES
        ($1 :: uuid, $2 :: text, $3 :: text, $4 :: text, $5 :: text, $6 :: int8?, $7 :: bytea)
      ON CONFLICT (user_id, client_id, endpoint) DO UPDATE SET
        p256dh      = ($4 :: text),
        auth        = ($5 :: text),
        expiration  = ($6 :: int8?),
        conn_id     = ($7 :: bytea)
    |]

lookupSubscriptionsImpl ::
  (PGConstraints r) =>
  UserId ->
  Sem r [WebPushAddress]
lookupSubscriptionsImpl uid =
  runStatement uid lookupSubs
  where
    lookupSubs ::
      Hasql.Statement UserId [WebPushAddress]
    lookupSubs =
      dimapPG
        [vectorStatement|
          SELECT (user_id :: uuid),
                 (client_id :: text),
                 (endpoint :: text),
                 (p256dh :: text),
                 (auth :: text),
                 (conn_id :: bytea)
          FROM webpush_subscriptions
          WHERE user_id = ($1 :: uuid)
        |]

deleteSubscriptionImpl ::
  (PGConstraints r) =>
  UserId ->
  EndpointUrl ->
  Sem r ()
deleteSubscriptionImpl uid endpoint =
  runStatement (uid, endpoint) deleteSub
  where
    deleteSub :: Hasql.Statement (UserId, EndpointUrl) ()
    deleteSub =
      lmapPG
        [resultlessStatement|
          DELETE FROM webpush_subscriptions
          WHERE user_id = ($1 :: uuid)
            AND endpoint = ($2 :: text)
        |]

deleteAllForUserImpl :: (PGConstraints r) => UserId -> Sem r ()
deleteAllForUserImpl uid =
  runStatement uid deleteAll
  where
    deleteAll :: Hasql.Statement UserId ()
    deleteAll =
      lmapPG
        [resultlessStatement|
          DELETE FROM webpush_subscriptions
          WHERE user_id = ($1 :: uuid)
        |]

purgeExpiredImpl :: (PGConstraints r) => UserId -> Sem r ()
purgeExpiredImpl uid =
  runStatement uid purgeStmt
  where
    purgeStmt :: Hasql.Statement UserId ()
    purgeStmt =
      lmapPG
        [resultlessStatement|
          DELETE FROM webpush_subscriptions
          WHERE user_id = ($1 :: uuid)
            AND expiration IS NOT NULL
            AND expiration < (SELECT FLOOR(EXTRACT(EPOCH FROM now()) * 1000))
        |]
