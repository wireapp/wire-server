{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.DomainRegistrationStore.Cassandra
  ( interpretDomainRegistrationStoreToCassandra,
  )
where

import Cassandra
import Data.Domain
import Database.CQL.Protocol (Record (..), TupleType, asTuple)
import Imports hiding (lookup)
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.DomainRegistrationStore (DomainRegistrationStore (..), StoredDomainRegistration (..))

deriving instance Cql SAML.IdPId

interpretDomainRegistrationStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor DomainRegistrationStore r
interpretDomainRegistrationStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      Upsert dr -> upsert dr
      Lookup domain -> lookup domain
      Delete domain -> delete domain

upsert :: (MonadClient m) => StoredDomainRegistration -> m ()
upsert dr = retry x5 $ write cqlUpsert (params LocalQuorum (asTuple dr))

lookup :: (MonadClient m) => Domain -> m (Maybe StoredDomainRegistration)
lookup domain =
  fmap asRecord
    <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity domain)))

delete :: (MonadClient m) => Domain -> m ()
delete domain = retry x5 $ write cqlDelete (params LocalQuorum (Identity domain))

cqlUpsert :: PrepQuery W (TupleType StoredDomainRegistration) ()
cqlUpsert = "INSERT INTO domain_registration (domain, domain_redirect, idp_id, backend_url, team_invite, team, dns_verification_code) VALUES (?,?,?,?,?,?,?)"

cqlSelect :: PrepQuery R (Identity Domain) (TupleType StoredDomainRegistration)
cqlSelect = "SELECT (domain, domain_redirect, idp_id, backend_url, team_invite, team, dns_verification_code) FROM domain_registration WHERE domain = ?"

cqlDelete :: PrepQuery W (Identity Domain) ()
cqlDelete = "DELETE FROM domain_registration WHERE domain = ?"
