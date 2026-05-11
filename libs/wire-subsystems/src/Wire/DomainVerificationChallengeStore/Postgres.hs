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

module Wire.DomainVerificationChallengeStore.Postgres
  ( interpretDomainVerificationChallengeStoreToPostgres,
  )
where

import Data.Domain
import Data.Id
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports hiding (lookup)
import Polysemy
import Util.Timeout
import Wire.API.EnterpriseLogin
import Wire.API.PostgresMarshall
import Wire.DomainVerificationChallengeStore
import Wire.Postgres

interpretDomainVerificationChallengeStoreToPostgres ::
  forall r.
  (PGConstraints r) =>
  Timeout ->
  InterpreterFor DomainVerificationChallengeStore r
interpretDomainVerificationChallengeStoreToPostgres ttl =
  interpret $
    \case
      Insert challenge -> insertImpl ttl challenge
      Lookup challengeId -> lookupImpl challengeId
      Delete challengeId -> deleteImpl challengeId

deleteImpl :: (PGConstraints r) => ChallengeId -> Sem r ()
deleteImpl cid =
  runStatement cid deleteStmt
  where
    deleteStmt :: Hasql.Statement ChallengeId ()
    deleteStmt =
      lmapPG
        [resultlessStatement|DELETE FROM domain_registration_challenge
                             WHERE id = ($1 :: uuid) 
                            |]

lookupImpl :: (PGConstraints r) => ChallengeId -> Sem r (Maybe StoredDomainVerificationChallenge)
lookupImpl cid = do
  mRow <- runStatement cid select
  pure $ mk <$> mRow
  where
    mk :: (Token, DnsVerificationToken, Domain) -> StoredDomainVerificationChallenge
    mk (hash, token, domain) =
      StoredDomainVerificationChallenge
        { challengeId = cid,
          domain = domain,
          challengeTokenHash = hash,
          dnsVerificationToken = token
        }

    select :: Hasql.Statement ChallengeId (Maybe (Token, DnsVerificationToken, Domain))
    select =
      dimapPG
        [maybeStatement|SELECT 
                          (challenge_token_hash :: bytea),
                          (dns_verification_token :: text),
                          (domain :: text)
                        FROM domain_registration_challenge
                        WHERE id = ($1 :: uuid) AND expires_at > now ()
                       |]

insertImpl :: (PGConstraints r) => Timeout -> StoredDomainVerificationChallenge -> Sem r ()
insertImpl ttl ch =
  runStatement (ch.challengeId, ch.domain, ch.challengeTokenHash, ch.dnsVerificationToken, ttlSecs) insertStmt
  where
    ttlSecs = round (nominalDiffTimeToSeconds (timeoutDiff ttl)) :: Int32
    insertStmt :: Hasql.Statement (ChallengeId, Domain, Token, DnsVerificationToken, Int32) ()
    insertStmt =
      lmapPG
        [resultlessStatement|INSERT INTO domain_registration_challenge
                               (id, domain, challenge_token_hash, dns_verification_token, expires_at)
                             VALUES
                               ($1 :: uuid, $2 :: text, $3 :: bytea, $4 :: text, now() + make_interval(secs => $5 :: int))
                             ON CONFLICT (id) DO UPDATE
                             SET domain = ($2 :: text),
                                 challenge_token_hash = ($3 :: bytea),
                                 dns_verification_token = ($4 :: text),
                                 expires_at = now() + make_interval(secs => $5 :: int)
                          |]
