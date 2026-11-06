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

module Wire.PasswordStore.Postgres
  ( interpretPasswordStoreToPostgres,
    upsertProviderPassword,
    insertProviderPasswordRowIfNotDeleted,
    insertProviderPasswordDeletionMarker,
  )
where

import Data.Id
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.Password
import Wire.API.PostgresMarshall (dimapPG, lmapPG)
import Wire.PasswordStore
import Wire.Postgres

interpretPasswordStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor PasswordStore r
interpretPasswordStoreToPostgres = interpret $ \case
  LookupHashedProviderPassword pid -> lookupImpl pid
  InsertHashedProviderPassword pid pw -> upsertProviderPassword pid pw
  DeleteHashedProviderPassword pid -> deleteProviderPassword pid

lookupImpl :: (PGConstraints r) => ProviderId -> Sem r (Maybe Password)
lookupImpl pid = join <$> runStatement pid select
  where
    select :: Hasql.Statement ProviderId (Maybe (Maybe Password))
    select =
      dimapPG
        [maybeStatement|SELECT password :: text?
                        FROM provider_password
                        WHERE id = $1 :: uuid|]

upsertProviderPassword :: (PGConstraints r) => ProviderId -> Password -> Sem r ()
upsertProviderPassword pid pw = runStatement (pid, pw) upsertStatement
  where
    upsertStatement :: Hasql.Statement (ProviderId, Password) ()
    upsertStatement =
      lmapPG
        [resultlessStatement|INSERT INTO provider_password (id, password)
                             VALUES ($1 :: uuid, $2 :: text)
                             ON CONFLICT (id) DO UPDATE
                             SET password = EXCLUDED.password|]

deleteProviderPassword :: (PGConstraints r) => ProviderId -> Sem r ()
deleteProviderPassword pid = runStatement pid deleteStatement
  where
    deleteStatement :: Hasql.Statement ProviderId ()
    deleteStatement =
      dimapPG
        [resultlessStatement|DELETE FROM provider_password
                             WHERE id = $1 :: uuid|]

-- | Copy-loop insert used by 'Wire.PasswordStore.Migration'. Unlike
-- 'upsertProviderPassword' (the runtime write path), it skips rows whose
-- provider has a @provider_passwords_deletions@ marker, so a copy pass cannot
-- resurrect a credential deleted by dual-write traffic after the pass read
-- its Cassandra page. See 'Wire.PasswordStore.Migration' for the bounded-race
-- rationale (nonce/DPoP precedent, PR #5675).
insertProviderPasswordRowIfNotDeleted :: (PGConstraints r) => (ProviderId, Password) -> Sem r ()
insertProviderPasswordRowIfNotDeleted row = runStatement row insertIfNotDeletedStatement

insertIfNotDeletedStatement :: Hasql.Statement (ProviderId, Password) ()
insertIfNotDeletedStatement =
  lmapPG
    [resultlessStatement|INSERT INTO provider_password (id, password)
                         SELECT ($1 :: uuid), ($2 :: text)
                         WHERE NOT EXISTS (SELECT 1 FROM provider_passwords_deletions WHERE id = ($1 :: uuid))
                         ON CONFLICT (id) DO UPDATE
                         SET password = EXCLUDED.password|]

-- | Record a provider password as deleted in the @provider_passwords_deletions@
-- marker table; called by the dual-write delete so the copy loop can filter it
-- out.
insertProviderPasswordDeletionMarker :: (PGConstraints r) => ProviderId -> Sem r ()
insertProviderPasswordDeletionMarker pid = runStatement pid insertMarkerStatement

insertMarkerStatement :: Hasql.Statement ProviderId ()
insertMarkerStatement =
  dimapPG
    [resultlessStatement|INSERT INTO provider_passwords_deletions (id) VALUES ($1 :: uuid)
                         ON CONFLICT (id) DO NOTHING|]
