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

module Wire.DomainRegistrationStore.Migration
  ( migrateDomainRegistrationsLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Domain
import Data.Id
import Database.CQL.Protocol (Record (asRecord), TupleType)
import Hasql.Pool qualified as Hasql
import Imports hiding (lookup)
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Util.Timeout
import Wire.API.EnterpriseLogin
import Wire.DomainRegistrationStore
import Wire.DomainRegistrationStore.Cassandra ()
import Wire.DomainRegistrationStore.Postgres qualified as DomainRegistrationPostgres
import Wire.DomainVerificationChallengeStore
import Wire.DomainVerificationChallengeStore.Postgres qualified as ChallengePostgres
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateDomainRegistrationsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  IO ()
migrateDomainRegistrationsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed =
  migrationLoop
    logger
    "domain registrations"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "domain registrations")
    (migrateAllDomainRegistrations migOpts migCounter)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val name) .)
    . raiseUnder
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

migrateAllDomainRegistrations ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllDomainRegistrations migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllDomainRegistrations")
  withCount (paginateSem selectAllRegistrations (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize asRecord
    .| C.mapM_ (traverse_ (\row -> handleErrors (toByteString' (show row.domain)) (migrateDomainRegistrationRow migCounter row)))

  lift $ info $ Log.msg (Log.val "migrateAllDomainVerificationChallenges")
  withCount (paginateSem selectAllChallenges (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(cid, _, _, _, _) -> handleErrors (toByteString' cid) (migrateDomainVerificationChallengeRow migCounter row)))

migrateDomainRegistrationRow ::
  (PGConstraints r) =>
  Prometheus.Counter ->
  StoredDomainRegistration ->
  Sem r ()
migrateDomainRegistrationRow migCounter row = do
  DomainRegistrationPostgres.interpretDomainRegistrationStoreToPostgres $ upsertInternal row
  liftIO $ Prometheus.incCounter migCounter

migrateDomainVerificationChallengeRow ::
  (PGConstraints r) =>
  Prometheus.Counter ->
  (ChallengeId, Domain, Token, DnsVerificationToken, Int32) ->
  Sem r ()
migrateDomainVerificationChallengeRow migCounter (cid, domain, challengeTokenHash, dnsVerificationToken, ttlSecs) =
  when (ttlSecs > 0) $ do
    let ttl = Timeout (fromIntegral ttlSecs)
        row =
          StoredDomainVerificationChallenge
            { challengeId = cid,
              domain = domain,
              challengeTokenHash = challengeTokenHash,
              dnsVerificationToken = dnsVerificationToken
            }
    ChallengePostgres.interpretDomainVerificationChallengeStoreToPostgres ttl $ insert row
    liftIO $ Prometheus.incCounter migCounter

selectAllRegistrations :: PrepQuery R () (TupleType StoredDomainRegistration)
selectAllRegistrations =
  "SELECT domain, domain_redirect, team_invite, idp_id, backend_url, team, dns_verification_token, ownership_token_hash, authorized_team, webapp_url FROM domain_registration"

selectAllChallenges :: PrepQuery R () (ChallengeId, Domain, Token, DnsVerificationToken, Int32)
selectAllChallenges =
  "SELECT id, domain, challenge_token_hash, dns_verification_token, ttl(challenge_token_hash) FROM domain_registration_challenge"
