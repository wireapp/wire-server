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

module Wire.PropertyStore.Postgres
  ( interpretPropertyStoreToPostgres,
    insertPropertyImpl,
  )
where

import Data.Id (UserId)
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Properties
import Wire.Postgres
import Wire.PropertyStore

interpretPropertyStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor PropertyStore r
interpretPropertyStoreToPostgres = interpret $ \case
  InsertProperty u k v -> insertPropertyImpl u k v
  LookupProperty u k -> lookupPropertyImpl u k
  CountProperties u -> countPropertiesImpl u
  DeleteProperty u k -> deletePropertyImpl u k
  ClearProperties u -> clearPropertiesImpl u
  GetPropertyKeys u -> getPropertyKeysImpl u
  GetAllProperties u -> getAllPropertiesImpl u

insertPropertyImpl ::
  (PGConstraints r) =>
  UserId ->
  PropertyKey ->
  RawPropertyValue ->
  Sem r ()
insertPropertyImpl u k v =
  runStatement (u, k, v) . lmapPG $
    [resultlessStatement|INSERT INTO properties (user_id, key, value)
                         VALUES ($1 :: uuid, $2 :: text, $3 :: bytea)
                         ON CONFLICT (user_id, key) DO UPDATE
                         SET value = ($3 :: bytea)
                        |]

lookupPropertyImpl ::
  (PGConstraints r) =>
  UserId ->
  PropertyKey ->
  Sem r (Maybe RawPropertyValue)
lookupPropertyImpl u k =
  runStatement (u, k) . dimapPG $
    [maybeStatement|SELECT value :: bytea
                    FROM properties
                    WHERE user_id = ($1 :: uuid) AND key = ($2 :: text)
                   |]

countPropertiesImpl ::
  (PGConstraints r) =>
  UserId ->
  Sem r Int
countPropertiesImpl u = fromIntegral <$> runStatement (postgresMarshall u) count
  where
    count :: Hasql.Statement UUID Int64
    count =
      [singletonStatement|SELECT count(*) :: int8
                          FROM properties
                          WHERE user_id = ($1 :: uuid)
                         |]

deletePropertyImpl ::
  (PGConstraints r) =>
  UserId ->
  PropertyKey ->
  Sem r ()
deletePropertyImpl u k =
  runStatement (u, k) . lmapPG $
    [resultlessStatement|DELETE FROM properties
                         WHERE user_id = ($1 :: uuid) AND key = ($2 :: text)
                        |]

clearPropertiesImpl ::
  (PGConstraints r) =>
  UserId ->
  Sem r ()
clearPropertiesImpl u =
  runStatement u . lmapPG $
    [resultlessStatement|DELETE FROM properties
                         WHERE user_id = ($1 :: uuid)
                        |]

getPropertyKeysImpl ::
  (PGConstraints r) =>
  UserId ->
  Sem r [PropertyKey]
getPropertyKeysImpl u =
  fmap Vector.toList . runStatement u . dimapPG $
    [vectorStatement|SELECT key :: text
                     FROM properties
                     WHERE user_id = ($1 :: uuid)
                    |]

getAllPropertiesImpl ::
  (PGConstraints r) =>
  UserId ->
  Sem r [(PropertyKey, RawPropertyValue)]
getAllPropertiesImpl u =
  fmap Vector.toList . runStatement u . dimapPG $
    [vectorStatement|SELECT key :: text, value :: bytea
                     FROM properties
                     WHERE user_id = ($1 :: uuid)
                    |]
