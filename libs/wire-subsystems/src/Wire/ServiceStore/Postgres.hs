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
  )
where

import Control.Lens
import Data.Id
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
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
      lmapPG @(UUID, UUID, ByteString, ByteString, Vector ByteString, Bool)
        [resultlessStatement|INSERT INTO service
                               (provider, id, base_url, auth_token, fingerprints, enabled)
                             VALUES
                               ($1 :: uuid, $2 :: uuid, $3 :: bytea, $4 :: bytea, $5 :: bytea[], $6 :: boolean)
                             ON CONFLICT (provider, id) DO UPDATE
                             SET base_url = ($3 :: bytea),
                                 auth_token = ($4 :: bytea),
                                 fingerprints = ($5 :: bytea[]),
                                 enabled = ($6 :: boolean)
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
      dimapPG @(UUID, UUID) @(ProviderId, ServiceId) @(Maybe (ByteString, ByteString, Vector ByteString, Bool))
        [maybeStatement|SELECT (base_url :: bytea),
                               (auth_token :: bytea),
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
