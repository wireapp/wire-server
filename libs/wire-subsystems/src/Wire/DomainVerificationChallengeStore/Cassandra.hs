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
