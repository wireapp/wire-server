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

-- | Postgres implementation of the user push-token store.
--
-- The row layout mirrors the Cassandra @user_push@ table. Cassandra's
-- insert is a primary-key upsert, so the Postgres insert rewrites the
-- non-key columns on conflict.
module Wire.PushTokenStore.Postgres
  ( interpretPushTokenStoreToPostgres,
    insertPushTokenRow,
  )
where

import Data.ByteString.Conversion (fromByteString)
import Data.Id (ClientId, ConnId (..), Id (..), UserId, toUUID)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports hiding (lookup)
import Polysemy
import System.IO.Error (ioError, userError)
import Wire.API.PostgresMarshall
import Wire.API.Push.V2 (AppName (..), Token (..), Transport)
import Wire.Postgres
import Wire.PushTokenStore

interpretPushTokenStoreToPostgres ::
  (PGConstraints r) =>
  Sem (PushTokenStore ': r) a ->
  Sem r a
interpretPushTokenStoreToPostgres = interpret $ \case
  -- The read consistency is a Cassandra concept; Postgres reads are
  -- always consistent.
  LookupPushTokens u _c -> pgLookup u
  InsertPushToken u t a p arn conn client ->
    insertPushTokenRow u t a p (Just arn) conn (Just client)
  UpdatePushTokenArn u t a p arn ->
    runStatement (toUUID u, postgresMarshall t, appNameText a, tokenText p, arn)
      updateArnStatement
  DeletePushToken u t a p ->
    runStatement (toUUID u, postgresMarshall t, appNameText a, tokenText p)
      deleteStatement
  ErasePushTokens u ->
    runStatement (toUUID u) eraseStatement

-- | Insert (upsert) one @user_push@ row. Shared by the runtime path (which
-- always has an ARN and a client) and the migration copy loop (which copies
-- whatever Cassandra holds, including nulls) so they cannot drift.
insertPushTokenRow ::
  (PGConstraints r) =>
  UserId ->
  Transport ->
  AppName ->
  Token ->
  Maybe Text ->
  ConnId ->
  Maybe ClientId ->
  Sem r ()
insertPushTokenRow u t a p arn conn client =
  runStatement
    (toUUID u, postgresMarshall t, appNameText a, tokenText p, arn, postgresMarshall conn, postgresMarshall <$> client)
    insertStatement

pgLookup ::
  (PGConstraints r) =>
  UserId ->
  Sem r [PushTokenRow]
pgLookup u = do
  rows <- runStatement (toUUID u) lookupStatement
  traverse decodeRow (Vector.toList rows)
  where
    decodeRow (usr, trp, app, tok, arn, conn, client) = do
      transport <- decodeTransport usr trp
      pure
        ( Id usr,
          transport,
          AppName app,
          Token tok,
          arn,
          either (error . Text.unpack) id (postgresUnmarshall conn),
          client >>= fromByteString . encodeUtf8
        )

-- | Transports are written only by this store (0-4), so an undecodable
-- value is a data integrity bug: fail loudly instead of dropping rows.
-- The push token is a device credential and is not included in the error.
decodeTransport :: (PGConstraints r) => UUID -> Int32 -> Sem r Transport
decodeTransport usr trp =
  case postgresUnmarshall trp of
    Right t -> pure t
    Left err ->
      embed . ioError . userError $
        "push token store: undecodable transport "
          <> show trp
          <> " for user "
          <> show usr
          <> ": "
          <> Text.unpack err

lookupStatement :: Hasql.Statement UUID (Vector (UUID, Int32, Text, Text, Maybe Text, Text, Maybe Text))
lookupStatement =
  [vectorStatement|
      SELECT usr :: uuid, transport :: int4, app :: text, ptoken :: text, arn :: text?, connection :: text, client :: text?
      FROM user_push
      WHERE usr = ($1 :: uuid)
      ORDER BY ptoken ASC, app ASC, transport ASC
    |]

insertStatement :: Hasql.Statement (UUID, Int32, Text, Text, Maybe Text, Text, Maybe Text) ()
insertStatement =
  [resultlessStatement|
      INSERT INTO user_push
        (usr, transport, app, ptoken, arn, connection, client)
      VALUES
        ($1 :: uuid, $2 :: int4, $3 :: text, $4 :: text, $5 :: text?, $6 :: text, $7 :: text?)
      ON CONFLICT (usr, ptoken, app, transport) DO UPDATE SET
        arn = EXCLUDED.arn,
        connection = EXCLUDED.connection,
        client = EXCLUDED.client
    |]

updateArnStatement :: Hasql.Statement (UUID, Int32, Text, Text, Text) ()
updateArnStatement =
  [resultlessStatement|
      UPDATE user_push
      SET arn = ($5 :: text)
      WHERE usr = ($1 :: uuid) AND transport = ($2 :: int4) AND app = ($3 :: text) AND ptoken = ($4 :: text)
    |]

deleteStatement :: Hasql.Statement (UUID, Int32, Text, Text) ()
deleteStatement =
  [resultlessStatement|
      DELETE FROM user_push
      WHERE usr = ($1 :: uuid) AND transport = ($2 :: int4) AND app = ($3 :: text) AND ptoken = ($4 :: text)
    |]

eraseStatement :: Hasql.Statement UUID ()
eraseStatement =
  [resultlessStatement|
      DELETE FROM user_push WHERE usr = ($1 :: uuid)
    |]
