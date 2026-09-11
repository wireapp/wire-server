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

module Wire.BlockListStore.Postgres
  ( interpretBlockListStoreToPostgres,
    insertKey,
  )
where

import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.BlockListStore (BlockListStore (..))
import Wire.Postgres
import Wire.UserKeyStore (EmailKey, emailKeyUniq)

interpretBlockListStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor BlockListStore r
interpretBlockListStoreToPostgres = interpret $ \case
  Insert key -> insertImpl key
  Exists key -> existsImpl key
  Delete key -> deleteImpl key

insertImpl :: (PGConstraints r) => EmailKey -> Sem r ()
insertImpl = insertKey . emailKeyUniq

insertKey :: (PGConstraints r) => Text -> Sem r ()
insertKey key =
  runStatement key insertStatement
  where
    insertStatement :: Hasql.Statement Text ()
    insertStatement =
      [resultlessStatement|INSERT INTO blacklist (key)
                           VALUES ($1 :: text)
                           ON CONFLICT DO NOTHING
                          |]

existsImpl :: (PGConstraints r) => EmailKey -> Sem r Bool
existsImpl key =
  runStatement (emailKeyUniq key) existsStatement
  where
    existsStatement :: Hasql.Statement Text Bool
    existsStatement =
      [singletonStatement|SELECT EXISTS (
                           SELECT 1
                           FROM blacklist
                           WHERE key = ($1 :: text)
                         ) :: bool|]

deleteImpl :: (PGConstraints r) => EmailKey -> Sem r ()
deleteImpl key =
  runStatement (emailKeyUniq key) deleteStatement
  where
    deleteStatement :: Hasql.Statement Text ()
    deleteStatement =
      [resultlessStatement|DELETE FROM blacklist
                           WHERE key = ($1 :: text)
                          |]
