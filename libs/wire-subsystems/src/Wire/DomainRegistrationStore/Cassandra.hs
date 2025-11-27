{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.DomainRegistrationStore.Cassandra
  ( interpretDomainRegistrationStoreToCassandra,
  )
where

import Cassandra
import Data.Id (TeamId)
import Database.CQL.Protocol (Record (..), TupleType, asTuple)
import Imports hiding (lookup)
import Polysemy
import SAML2.WebSSO qualified as SAML
import UnliftIO (pooledForConcurrentlyN)
import Wire.DomainRegistrationStore

deriving instance Cql SAML.IdPId

interpretDomainRegistrationStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor DomainRegistrationStore r
interpretDomainRegistrationStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      UpsertInternal dr -> upsertImpl dr
      LookupInternal domain -> lookupImpl domain
      LookupByTeamInternal tid -> lookupByTeamInternalImpl tid
      DeleteInternal domain -> deleteImpl domain

lookupByTeamInternalImpl :: (MonadClient m, MonadUnliftIO m) => TeamId -> m [StoredDomainRegistration]
lookupByTeamInternalImpl tid = do
  domains <- lookupTeamDomains tid
  catMaybes <$> pooledForConcurrentlyN 16 domains lookupImpl

lookupTeamDomains :: (MonadClient m) => TeamId -> m [DomainKey]
lookupTeamDomains tid =
  fmap runIdentity <$> retry x1 (query cql (params LocalQuorum (Identity tid)))
  where
    cql :: PrepQuery R (Identity TeamId) (Identity DomainKey)
    cql = "SELECT domain FROM domain_registration_by_team WHERE team = ?"

upsertImpl :: (MonadClient m) => StoredDomainRegistration -> m ()
upsertImpl dr = do
  for_ dr.authorizedTeam $ flip upsertTeamIndex dr.domain
  retry x5 $ write cqlUpsert (params LocalQuorum (asTuple dr))

upsertTeamIndex :: (MonadClient m) => TeamId -> DomainKey -> m ()
upsertTeamIndex tid domain =
  retry x5 $ write cql (params LocalQuorum (tid, domain))
  where
    cql :: PrepQuery W (TeamId, DomainKey) ()
    cql = "INSERT INTO domain_registration_by_team (team, domain) VALUES (?,?)"

lookupImpl :: (MonadClient m) => DomainKey -> m (Maybe StoredDomainRegistration)
lookupImpl domain =
  fmap asRecord
    <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity domain)))

deleteImpl :: (MonadClient m) => DomainKey -> m ()
deleteImpl domain = retry x5 $ write cqlDelete (params LocalQuorum (Identity domain))

cqlUpsert :: PrepQuery W (TupleType StoredDomainRegistration) ()
cqlUpsert = "INSERT INTO domain_registration (domain, domain_redirect, team_invite, idp_id, backend_url, team, dns_verification_token, ownership_token_hash, authorized_team, webapp_url) VALUES (?,?,?,?,?,?,?,?,?,?)"

cqlSelect :: PrepQuery R (Identity DomainKey) (TupleType StoredDomainRegistration)
cqlSelect = "SELECT domain, domain_redirect, team_invite, idp_id, backend_url, team, dns_verification_token, ownership_token_hash, authorized_team, webapp_url FROM domain_registration WHERE domain = ?"

cqlDelete :: PrepQuery W (Identity DomainKey) ()
cqlDelete = "DELETE FROM domain_registration WHERE domain = ?"
