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

module Wire.ServiceStore.Postgres
  ( interpretServiceStoreToPostgres,
    upsertServiceIfNewer,
  )
where

import Control.Lens
import Data.Id
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.Bot.Service qualified as Bot
import Wire.API.PostgresMarshall
import Wire.API.Provider.Service (ServiceRef, ServiceToken, serviceRefId, serviceRefProvider)
import Wire.Postgres
import Wire.ServiceStore (ServiceStore (..))

interpretServiceStoreToPostgres ::
  (PGConstraints r) =>
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToPostgres = interpret $ \case
  CreateService s -> insertService s
  GetService sr -> lookupService sr
  DeleteService sr -> deleteService sr

insertService :: (PGConstraints r) => Bot.Service -> Sem r ()
insertService s =
  runStatement
    ( s ^. Bot.serviceRef . serviceRefProvider,
      s ^. Bot.serviceRef . serviceRefId,
      s ^. Bot.serviceUrl,
      s ^. Bot.serviceToken,
      s ^. Bot.serviceFingerprints,
      s ^. Bot.serviceEnabled
    )
    insert
  where
    insert ::
      Hasql.Statement (ProviderId, ServiceId, HttpsUrl, ServiceToken, [Fingerprint Rsa], Bool) ()
    insert =
      lmapPG @(UUID, UUID, ByteString, Text, Vector ByteString, Bool)
        [resultlessStatement|INSERT INTO service
                               (provider, id, base_url, auth_token, fingerprints, enabled, last_update)
                             VALUES
                               ($1 :: uuid, $2 :: uuid, $3 :: bytea, $4 :: text, $5 :: bytea[], $6 :: boolean, now())
                             ON CONFLICT (provider, id) DO UPDATE
                             SET base_url = ($3 :: bytea),
                                 auth_token = ($4 :: text),
                                 fingerprints = ($5 :: bytea[]),
                                 enabled = ($6 :: boolean),
                                 last_update = now()
        |]

-- | Guarded upsert used by both the backfill (Wire.ServiceStore.Migration)
-- and the live dual-write mirror (Wire.ServiceStore.DualWrite). On conflict
-- the row is only overwritten when the replayed Cassandra row is at least as
-- new as what is stored (ties go to Cassandra), so a stale replay cannot
-- clobber a newer row that dual-write already put in Postgres. During
-- migration the stored timestamps are mutation timestamps supplied by the
-- dual-write interpreter; the backfill compares them against Cassandra
-- coordinator writetimes. NTP-level clock skew between the two is negligible
-- against the page-scan-scale staleness the guard exists to block.
--
-- Known residual (accepted, cf. merged conversation/user backfills): a row
-- deleted in Cassandra after the scan that produced it can still be replayed
-- and re-inserted here — the delete has already removed the Postgres row, so
-- the guard has nothing to compare against.
upsertServiceIfNewer :: (PGConstraints r) => Bot.Service -> UTCTime -> Sem r ()
upsertServiceIfNewer s ts =
  runStatement
    ( s ^. Bot.serviceRef . serviceRefProvider,
      s ^. Bot.serviceRef . serviceRefId,
      s ^. Bot.serviceUrl,
      s ^. Bot.serviceToken,
      s ^. Bot.serviceFingerprints,
      s ^. Bot.serviceEnabled,
      ts
    )
    upsertIfNewer
  where
    upsertIfNewer ::
      Hasql.Statement (ProviderId, ServiceId, HttpsUrl, ServiceToken, [Fingerprint Rsa], Bool, UTCTime) ()
    upsertIfNewer =
      lmapPG @(UUID, UUID, ByteString, Text, Vector ByteString, Bool, UTCTime)
        [resultlessStatement|INSERT INTO service
                               (provider, id, base_url, auth_token, fingerprints, enabled, last_update)
                             VALUES
                               ($1 :: uuid, $2 :: uuid, $3 :: bytea, $4 :: text, $5 :: bytea[], $6 :: boolean, ($7 :: timestamptz))
                             ON CONFLICT (provider, id) DO UPDATE
                             SET base_url = ($3 :: bytea),
                                 auth_token = ($4 :: text),
                                 fingerprints = ($5 :: bytea[]),
                                 enabled = ($6 :: boolean),
                                 last_update = ($7 :: timestamptz)
                             WHERE service.last_update <= ($7 :: timestamptz)
        |]

lookupService ::
  (PGConstraints r) =>
  ServiceRef ->
  Sem r (Maybe Bot.Service)
lookupService sr =
  fmap (\(url, tok, fps, ena) -> Bot.Service sr url tok fps ena)
    <$> runStatement (sr ^. serviceRefProvider, sr ^. serviceRefId) select
  where
    select ::
      Hasql.Statement
        (ProviderId, ServiceId)
        (Maybe (HttpsUrl, ServiceToken, [Fingerprint Rsa], Bool))
    select =
      dimapPG @(UUID, UUID) @(ProviderId, ServiceId) @(Maybe (ByteString, Text, Vector ByteString, Bool))
        [maybeStatement|SELECT (base_url :: bytea),
                               (auth_token :: text),
                               (fingerprints :: bytea[]),
                               (enabled :: boolean)
                        FROM service
                        WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]

deleteService :: (PGConstraints r) => ServiceRef -> Sem r ()
deleteService sr =
  runStatement (sr ^. serviceRefProvider, sr ^. serviceRefId) delete
  where
    delete :: Hasql.Statement (ProviderId, ServiceId) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM service
                             WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]
