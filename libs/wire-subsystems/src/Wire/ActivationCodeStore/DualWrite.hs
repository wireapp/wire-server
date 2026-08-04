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

module Wire.ActivationCodeStore.DualWrite
  ( interpretActivationCodeStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.ActivationCodeStore
import Wire.ActivationCodeStore qualified as ActivationCodeStore
import Wire.ActivationCodeStore.Cassandra qualified as Cassandra
import Wire.ActivationCodeStore.Postgres qualified as Postgres
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.Postgres
import Wire.UserKeyStore

interpretActivationCodeStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor ActivationCodeStore r
interpretActivationCodeStoreToCassandraAndPostgres cs = interpret $ \case
  LookupActivationCode ek ->
    Cassandra.interpretActivationCodeStoreToCassandra cs $ ActivationCodeStore.lookupActivationCode ek
  NewActivationCode ek timeout uid -> do
    activation <-
      Cassandra.interpretActivationCodeStoreToCassandra cs $
        ActivationCodeStore.newActivationCode ek timeout uid
    Postgres.interpretActivationCodeStoreToPostgres $
      Postgres.insertActivationKeyRow
        ( activationKey activation,
          "email",
          fromEmail (emailKeyOrig ek),
          activationCode activation,
          uid,
          maxAttempts,
          round timeout
        )
    pure activation
  DeleteActivationCode ek -> do
    Cassandra.interpretActivationCodeStoreToCassandra cs $ ActivationCodeStore.deleteActivationCode ek
    Postgres.interpretActivationCodeStoreToPostgres $ ActivationCodeStore.deleteActivationCode ek
  VerifyActivationCode key code -> do
    -- Cassandra is the source of truth for reads; the Postgres result is
    -- discarded for state convergence. A Postgres error fails the operation
    -- (consistent with sibling stores).
    result <- Cassandra.interpretActivationCodeStoreToCassandra cs $ ActivationCodeStore.verifyActivationCode key code
    _ <- Postgres.interpretActivationCodeStoreToPostgres $ ActivationCodeStore.verifyActivationCode key code
    pure result
