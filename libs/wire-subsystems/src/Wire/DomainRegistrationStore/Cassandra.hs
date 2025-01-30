{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.DomainRegistrationStore.Cassandra
  ( interpretDomainRegistrationStoreToCassandra,
  )
where

import Cassandra
import Database.CQL.Protocol (Record (..), TupleType, asTuple)
import Imports hiding (lookup)
import Polysemy
import SAML2.WebSSO qualified as SAML
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
      DeleteInternal domain -> deleteImpl domain

upsertImpl :: (MonadClient m) => StoredDomainRegistration -> m ()
upsertImpl dr = retry x5 $ write cqlUpsert (params LocalQuorum (asTuple dr))

lookupImpl :: (MonadClient m) => DomainKey -> m (Maybe StoredDomainRegistration)
lookupImpl domain =
  fmap asRecord
    <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity domain)))

deleteImpl :: (MonadClient m) => DomainKey -> m ()
deleteImpl domain = retry x5 $ write cqlDelete (params LocalQuorum (Identity domain))

cqlUpsert :: PrepQuery W (TupleType StoredDomainRegistration) ()
cqlUpsert = "INSERT INTO domain_registration (domain, domain_redirect, team_invite, idp_id, backend_url, team, dns_verification_token, ownership_token_hash, authorized_team) VALUES (?,?,?,?,?,?,?,?,?)"

cqlSelect :: PrepQuery R (Identity DomainKey) (TupleType StoredDomainRegistration)
cqlSelect = "SELECT domain, domain_redirect, team_invite, idp_id, backend_url, team, dns_verification_token, ownership_token_hash, authorized_team FROM domain_registration WHERE domain = ?"

cqlDelete :: PrepQuery W (Identity DomainKey) ()
cqlDelete = "DELETE FROM domain_registration WHERE domain = ?"
