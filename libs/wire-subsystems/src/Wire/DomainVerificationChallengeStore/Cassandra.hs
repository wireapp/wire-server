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
import Polysemy.Embed
import Polysemy.Input
import Util.Timeout
import Wire.DomainVerificationChallengeStore

interpretDomainVerificationChallengeStoreToCassandra ::
  forall r.
  ( Member (Embed IO) r
  ) =>
  ClientState ->
  Timeout ->
  InterpreterFor DomainVerificationChallengeStore r
interpretDomainVerificationChallengeStoreToCassandra casClient ttl =
  runInputConst ttl
    . runEmbedded (runClient casClient)
    . interpret
      ( \case
          Insert challenge -> insertImpl challenge
          Lookup challengeId -> lookupImpl challengeId
          Delete challengeId -> deleteImpl challengeId
      )
    . raiseUnder2

insertImpl ::
  (Member (Embed Client) r, Member (Input Timeout) r) =>
  StoredDomainVerificationChallenge ->
  Sem r ()
insertImpl challenge = do
  ttl <- input
  let q :: PrepQuery W (TupleType StoredDomainVerificationChallenge) ()
      q =
        fromString $
          "INSERT INTO domain_registration_challenge\
          \ (id, domain, challenge_token_hash, dns_verification_token)\
          \ VALUES (?,?,?,?) using ttl "
            <> show (round (nominalDiffTimeToSeconds (timeoutDiff ttl)) :: Integer)
  embed $ retry x5 $ write q (params LocalQuorum (asTuple challenge))

lookupImpl ::
  (Member (Embed Client) r) =>
  ChallengeId ->
  Sem r (Maybe StoredDomainVerificationChallenge)
lookupImpl challengeId =
  embed $
    fmap asRecord
      <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity challengeId)))

cqlSelect :: PrepQuery R (Identity ChallengeId) (TupleType StoredDomainVerificationChallenge)
cqlSelect = "SELECT id, domain, challenge_token_hash, dns_verification_token FROM domain_registration_challenge WHERE id = ?"

deleteImpl :: (Member (Embed Client) r) => ChallengeId -> Sem r ()
deleteImpl challengeId = embed $ retry x5 $ write cqlDelete (params LocalQuorum (Identity challengeId))

cqlDelete :: PrepQuery W (Identity ChallengeId) ()
cqlDelete = "DELETE FROM domain_registration_challenge WHERE id = ?"
