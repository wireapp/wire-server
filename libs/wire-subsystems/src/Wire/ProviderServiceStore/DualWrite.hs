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

module Wire.ProviderServiceStore.DualWrite
  ( interpretProviderServiceStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Wire.Postgres (PGConstraints, runStatement)
import Wire.ProviderServiceStore
import Wire.ProviderServiceStore qualified as Store
import Wire.ProviderServiceStore.Cassandra qualified as Cassandra
import Wire.ProviderServiceStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres so the backfill only has to heal drift, not copy everything.
--
-- Each row mutation (create / update / update-conn) generates a client
-- timestamp /before/ the Cassandra write, so it is ≤ the Cassandra coordinator
-- writetime of the same mutation: backfill replays of that row (rowTs ≥ ts)
-- still pass the @last_update <= replay_ts@ guard, preserving the "replay
-- heals drift" property. The mirror writes themselves are guarded by the same
-- timestamp, so two mutations ordered A→B (tsA < tsB) keep in Postgres the
-- same winner Cassandra LWW picked — previously the mirror used unconditional
-- statements stamped with PG-server @now()@, which let an out-of-order PG
-- write keep stale credentials.
--
-- Accepted trade-offs and residuals:
--
--   1. The Postgres mirror is applied sequentially after the Cassandra write,
--      not atomically with it. If a Postgres write fails after a successful
--      Cassandra write, the stores drift until the next backfill pass heals
--      them.
--   2. The PK-only companion tables (@provider_service_tag@,
--      @provider_service_prefix@, @provider_service_whitelist{,_rev}@) have no
--      writetime, so their backfill is copy-only: deletes that happen between
--      the start of a backfill and the cutover can survive in Postgres. Their
--      live mirror writes are likewise unguarded (no ts column to guard on).
--   3. The PG delete is unconditional; an out-of-order create-then-delete at
--      PG can transiently drop a row Cassandra still has. This self-heals:
--      reads go to Cassandra while migrating, and the backfill replay-insert
--      is unconditional on an empty row.
--   4. A guarded mirror UPDATE that matches 0 rows (row not yet mirrored, or
--      genuinely stale) leaves drift that the periodic backfill heals (replay
--      insert is unconditional on an empty row, and
--      @upsertProviderServiceIfNewer@ accepts replays whose rowTs ≥ the
--      stored @last_update@).
--   5. Client timestamps vs coordinator clocks rely on NTP; same-microsecond
--      concurrent updates to one service are beyond realistic traffic.
interpretProviderServiceStoreToCassandraAndPostgres ::
  forall r a.
  ( Member TinyLog r,
    PGConstraints r
  ) =>
  ClientState ->
  Sem (ProviderServiceStore ': r) a ->
  Sem r a
interpretProviderServiceStoreToCassandraAndPostgres cassClient = interpret $ \case
  CreateProviderService pid name summary descr url token key fprint assets tags -> do
    ts <- embed $ posixSecondsToUTCTime <$> getPOSIXTime
    sid <- Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.createProviderService pid name summary descr url token key fprint assets tags
    runStatement (pid, sid, name, summary, descr, url, [token], [key], [fprint], assets, tags, False, ts) Postgres.insertProviderServiceWithTs
    pure sid
  LookupProviderService pid sid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.lookupProviderService pid sid
  ListProviderServices pid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.listProviderServices pid
  UpdateProviderService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled ->
    do
      ts <- embed $ posixSecondsToUTCTime <$> getPOSIXTime
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.updateProviderService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled
      Postgres.updateProviderServiceRowIfNewer ts pid sid svcName svcTags nameChange summary descr assets tagsChange enabled
  DeleteProviderService pid sid name tags ->
    do
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.deleteProviderService pid sid name tags
      Postgres.interpretProviderServiceStoreToPostgres $ Store.deleteProviderService pid sid name tags
  LookupProviderServiceProfile pid sid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.lookupProviderServiceProfile pid sid
  ListProviderServiceProfiles pid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.listProviderServiceProfiles pid
  LookupProviderServiceConn pid sid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.lookupProviderServiceConn pid sid
  UpdateProviderServiceConn pid sid url tokens keys enabled ->
    do
      ts <- embed $ posixSecondsToUTCTime <$> getPOSIXTime
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.updateProviderServiceConn pid sid url tokens keys enabled
      Postgres.updateProviderServiceConnRowIfNewer ts pid sid url tokens keys enabled
  InsertProviderServiceIndexes pid sid name tags ->
    do
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.insertProviderServiceIndexes pid sid name tags
      Postgres.interpretProviderServiceStoreToPostgres $ Store.insertProviderServiceIndexes pid sid name tags
  DeleteProviderServiceIndexes pid sid name tags ->
    do
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.deleteProviderServiceIndexes pid sid name tags
      Postgres.interpretProviderServiceStoreToPostgres $ Store.deleteProviderServiceIndexes pid sid name tags
  PaginateProviderServiceTags tags start size providerFilter ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.paginateProviderServiceTags tags start size providerFilter
  PaginateProviderServiceNames mbPrefix size providerFilter ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.paginateProviderServiceNames mbPrefix size providerFilter
  InsertProviderServiceWhitelist tid pid sid ->
    do
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.insertProviderServiceWhitelist tid pid sid
      Postgres.interpretProviderServiceStoreToPostgres $ Store.insertProviderServiceWhitelist tid pid sid
  DeleteProviderServiceWhitelist mbTid pid sid ->
    do
      Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.deleteProviderServiceWhitelist mbTid pid sid
      Postgres.interpretProviderServiceStoreToPostgres $ Store.deleteProviderServiceWhitelist mbTid pid sid
  PaginateProviderServiceWhitelist tid mbPrefix filterDisabled size ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.paginateProviderServiceWhitelist tid mbPrefix filterDisabled size
  GetProviderServiceWhitelistStatus tid pid sid ->
    Cassandra.interpretProviderServiceStoreToCassandra cassClient $ Store.getProviderServiceWhitelistStatus tid pid sid
