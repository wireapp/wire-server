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

module Wire.PasswordResetCodeStore.DualWrite
  ( interpretPasswordResetCodeStoreToCassandraAndPostgres,
  )
where

import Cassandra (MonadClient)
import Imports
import Polysemy
import Wire.PasswordResetCodeStore
import Wire.PasswordResetCodeStore.Cassandra (passwordResetCodeStoreToCassandra)
import Wire.PasswordResetCodeStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | During migration, Cassandra remains the source of truth for reads while
-- writes are mirrored to Postgres. The generation constructors are pure (they
-- only draw randomness); they run exactly once — invoking both interpreters
-- would produce two independent random codes.
interpretPasswordResetCodeStoreToCassandraAndPostgres ::
  forall m r.
  ( MonadClient m,
    Member (Embed m) r,
    PGConstraints r
  ) =>
  InterpreterFor PasswordResetCodeStore r
interpretPasswordResetCodeStoreToCassandraAndPostgres = interpret $ \case
  GenerateEmailCode -> Postgres.genEmailCode
  GeneratePhoneCode -> Postgres.genPhoneCode
  CodeSelect prk ->
    passwordResetCodeStoreToCassandra @m $ codeSelect prk
  CodeInsert prk prqd ttl -> do
    passwordResetCodeStoreToCassandra @m $ codeInsert prk prqd ttl
    Postgres.interpretPasswordResetCodeStoreToPostgres $ codeInsert prk prqd ttl
  CodeDelete prk -> do
    passwordResetCodeStoreToCassandra @m $ codeDelete prk
    Postgres.interpretPasswordResetCodeStoreToPostgres $ codeDelete prk
