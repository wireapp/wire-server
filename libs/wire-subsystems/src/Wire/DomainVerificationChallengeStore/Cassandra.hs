{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.DomainVerificationChallengeStore.Cassandra
  ( interpretDomainVerificationChallengeStoreToCassandra,
  )
where

import Cassandra
import Data.Id
import Database.CQL.Protocol (Record (..), TupleType, asTuple)
import Imports hiding (lookup)
import Polysemy
import Wire.DomainVerificationChallengeStore (DomainVerificationChallengeStore (..), StoredDomainVerificationChallenge (..))

interpretDomainVerificationChallengeStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor DomainVerificationChallengeStore r
interpretDomainVerificationChallengeStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      Insert challenge -> insertImpl challenge
      Lookup challengeId -> lookupImpl challengeId
      Delete challengeId -> deleteImpl challengeId

insertImpl :: (MonadClient m) => StoredDomainVerificationChallenge -> m ()
insertImpl challenge = retry x5 $ write cqlInsert (params LocalQuorum (asTuple challenge))

cqlInsert :: PrepQuery W (TupleType StoredDomainVerificationChallenge) ()
cqlInsert = "INSERT INTO domain_registration_challenge (id, domain, challenge_token_hash, dns_verification_token) VALUES (?,?,?,?)"

lookupImpl :: (MonadClient m) => ChallengeId -> m (Maybe StoredDomainVerificationChallenge)
lookupImpl challengeId =
  fmap asRecord
    <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity challengeId)))

cqlSelect :: PrepQuery R (Identity ChallengeId) (TupleType StoredDomainVerificationChallenge)
cqlSelect = "SELECT id, domain, challenge_token_hash, dns_verification_token FROM domain_registration_challenge WHERE id = ?"

deleteImpl :: (MonadClient m) => ChallengeId -> m ()
deleteImpl challengeId = retry x5 $ write cqlDelete (params LocalQuorum (Identity challengeId))

cqlDelete :: PrepQuery W (Identity ChallengeId) ()
cqlDelete = "DELETE FROM domain_registration_challenge WHERE id = ?"
