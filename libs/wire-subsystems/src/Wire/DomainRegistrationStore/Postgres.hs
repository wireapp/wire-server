{-# LANGUAGE RecordWildCards #-}

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

module Wire.DomainRegistrationStore.Postgres
  ( interpretDomainRegistrationStoreToPostgres,
  )
where

import Data.Id (TeamId)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports hiding (lookup)
import Polysemy
import Wire.API.PostgresMarshall
import Wire.DomainRegistrationStore
import Wire.Postgres

interpretDomainRegistrationStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor DomainRegistrationStore r
interpretDomainRegistrationStoreToPostgres = interpret $ \case
  UpsertInternal dr -> upsertImpl dr
  LookupInternal domain -> lookupImpl domain
  LookupByTeamInternal tid -> lookupByTeamInternalImpl tid
  DeleteInternal domain -> deleteImpl domain

upsertImpl :: (PGConstraints r) => StoredDomainRegistration -> Sem r ()
upsertImpl dr =
  runStatement dr upsertStatement
  where
    upsertStatement :: Hasql.Statement StoredDomainRegistration ()
    upsertStatement =
      lmapPG
        [resultlessStatement|INSERT INTO domain_registration
                               (domain, domain_redirect, team_invite, idp_id, backend_url,
                                team, dns_verification_token, ownership_token_hash, authorized_team, webapp_url)
                             VALUES
                               ($1 :: text, $2 :: int?, $3 :: int?, $4 :: uuid?, $5 :: bytea?,
                                $6 :: uuid?, $7 :: text?, $8 :: bytea?, $9 :: uuid?, $10 :: bytea?)
                             ON CONFLICT (domain) DO UPDATE
                             SET domain_redirect = ($2 :: int?),
                                 team_invite = ($3 :: int?),
                                 idp_id = ($4 :: uuid?),
                                 backend_url = ($5 :: bytea?),
                                 team = ($6 :: uuid?),
                                 dns_verification_token = ($7 :: text?),
                                 ownership_token_hash = ($8 :: bytea?),
                                 authorized_team = ($9 :: uuid?),
                                 webapp_url = ($10 :: bytea?)
                          |]

lookupImpl :: (PGConstraints r) => DomainKey -> Sem r (Maybe StoredDomainRegistration)
lookupImpl domain =
  runStatement domain selectStatement
  where
    selectStatement :: Hasql.Statement DomainKey (Maybe StoredDomainRegistration)
    selectStatement =
      dimapPG @Text @DomainKey @(Maybe DomainRegistrationRow) @(Maybe StoredDomainRegistration) $
        [maybeStatement|SELECT (domain :: text), (domain_redirect :: int?), (team_invite :: int?),
                                 (idp_id :: uuid?), (backend_url :: bytea?), (team :: uuid?),
                                 (dns_verification_token :: text?), (ownership_token_hash :: bytea?),
                                 (authorized_team :: uuid?), (webapp_url :: bytea?)
                          FROM domain_registration
                          WHERE domain = ($1 :: text)
                         |]

lookupByTeamInternalImpl :: (PGConstraints r) => TeamId -> Sem r [StoredDomainRegistration]
lookupByTeamInternalImpl tid = do
  rows <- runStatement tid selectByTeamStatement
  pure $ Vector.toList rows
  where
    selectByTeamStatement :: Hasql.Statement TeamId (Vector StoredDomainRegistration)
    selectByTeamStatement =
      dimapPG @UUID @TeamId @(Vector DomainRegistrationRow) @(Vector StoredDomainRegistration) $
        [vectorStatement|SELECT (domain :: text), (domain_redirect :: int?), (team_invite :: int?),
                                  (idp_id :: uuid?), (backend_url :: bytea?), (team :: uuid?),
                                  (dns_verification_token :: text?), (ownership_token_hash :: bytea?),
                                  (authorized_team :: uuid?), (webapp_url :: bytea?)
                           FROM domain_registration
                           WHERE authorized_team = ($1 :: uuid)
                          |]

deleteImpl :: (PGConstraints r) => DomainKey -> Sem r ()
deleteImpl domain =
  runStatement domain deleteStatement
  where
    deleteStatement :: Hasql.Statement DomainKey ()
    deleteStatement =
      lmapPG
        [resultlessStatement|DELETE FROM domain_registration
                             WHERE domain = ($1 :: text)
                            |]
