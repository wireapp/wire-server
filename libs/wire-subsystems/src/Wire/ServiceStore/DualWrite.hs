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

module Wire.ServiceStore.DualWrite
  ( interpretServiceStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Wire.Postgres (PGConstraints)
import Wire.ServiceStore (ServiceStore (..))
import Wire.ServiceStore qualified as ServiceStore
import Wire.ServiceStore.Cassandra qualified as Cassandra
import Wire.ServiceStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres. Each mirror write of a mutation carries a client-generated
-- timestamp taken /before/ the Cassandra write, so it is ≤ the Cassandra
-- coordinator writetime of the same mutation: any later backfill replay of
-- that row (writetime ≥ ts) still passes the @last_update <= replay_ts@ guard,
-- preserving the "replay heals drift" property.
--
-- Why the guard closes the ordering race: two mutations ordered A→B get
-- tsA < tsB; whichever PG write lands last, the @ON CONFLICT ... WHERE
-- service.last_update <= ts@ guard keeps the max-ts value — the same winner
-- Cassandra LWW picked. Previously the live mirror used an unconditional
-- upsert stamped with PG-server @now()@, so an out-of-order PG write could
-- keep stale credentials, and the @now()@ stamp poisoned the backfill guard.
--
-- Residuals (accepted, same wording family as 'Postgres.upsertServiceIfNewer'):
--
--   * The PG delete is unconditional; an out-of-order create-then-delete at PG
--     can transiently drop a row Cassandra still has. This self-heals: reads
--     go to Cassandra while migrating, and the backfill replay-insert is
--     unconditional on an empty row. A row deleted in Cassandra is dual-written
--     synchronously, so no replay can resurrect it.
--   * Client timestamps vs coordinator clocks rely on NTP; same-microsecond
--     concurrent updates to one service are beyond realistic traffic.
interpretServiceStoreToCassandraAndPostgres ::
  ( Member TinyLog r,
    PGConstraints r
  ) =>
  ClientState ->
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToCassandraAndPostgres cassClient = interpret $ \case
  GetService sr ->
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.getService sr
  CreateService s -> do
    ts <- embed $ posixSecondsToUTCTime <$> getPOSIXTime
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.createService s
    Postgres.upsertServiceIfNewer s ts
  DeleteService sr -> do
    Cassandra.interpretServiceStoreToCassandra cassClient $ ServiceStore.deleteService sr
    Postgres.interpretServiceStoreToPostgres $ ServiceStore.deleteService sr
