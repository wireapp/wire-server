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

module Wire.CustomBackendStore.Postgres
  ( interpretCustomBackendStoreToPostgres,
  )
where

import Data.Domain (Domain)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.CustomBackend
import Wire.API.PostgresMarshall
import Wire.CustomBackendStore
import Wire.Postgres

interpretCustomBackendStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor CustomBackendStore r
interpretCustomBackendStoreToPostgres = interpret $ \case
  GetCustomBackend dom -> getImpl dom
  SetCustomBackend dom cb -> setImpl (dom, cb)
  DeleteCustomBackend dom -> deleteImpl dom

getImpl :: (PGConstraints r) => Domain -> Sem r (Maybe CustomBackend)
getImpl dom =
  runStatement dom selectStatement
  where
    selectStatement :: Hasql.Statement Domain (Maybe CustomBackend)
    selectStatement =
      dimapPG @Text @Domain @(Maybe CustomBackendRow) @(Maybe CustomBackend) $
        [maybeStatement|SELECT (domain :: text), (config_json_url :: bytea), (webapp_welcome_url :: bytea)
                         FROM custom_backend
                         WHERE domain = ($1 :: text)
                        |]

setImpl :: (PGConstraints r) => (Domain, CustomBackend) -> Sem r ()
setImpl input =
  runStatement input upsertStatement
  where
    upsertStatement :: Hasql.Statement (Domain, CustomBackend) ()
    upsertStatement =
      lmapPG
        [resultlessStatement|INSERT INTO custom_backend (domain, config_json_url, webapp_welcome_url)
                             VALUES (($1 :: text), ($2 :: bytea), ($3 :: bytea))
                             ON CONFLICT (domain) DO UPDATE
                             SET config_json_url = ($2 :: bytea),
                                 webapp_welcome_url = ($3 :: bytea)
                            |]

deleteImpl :: (PGConstraints r) => Domain -> Sem r ()
deleteImpl dom =
  runStatement dom deleteStatement
  where
    deleteStatement :: Hasql.Statement Domain ()
    deleteStatement =
      lmapPG
        [resultlessStatement|DELETE FROM custom_backend
                             WHERE domain = ($1 :: text)
                            |]
