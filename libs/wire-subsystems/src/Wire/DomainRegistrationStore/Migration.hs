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
import Polysemy.Async
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.Time
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Util.Timeout
import Wire.API.EnterpriseLogin
import Wire.DomainRegistrationStore
import Wire.DomainRegistrationStore.Cassandra qualified as DomainRegistrationCassandra
import Wire.DomainRegistrationStore.Postgres qualified as DomainRegistrationPostgres
import Wire.DomainVerificationChallengeStore
import Wire.DomainVerificationChallengeStore.Postgres qualified as ChallengePostgres
import Wire.Migration
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    Async,
    Race,
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
    . interpretRace
    . asyncToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

migrateAllDomainRegistrations ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Async r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllDomainRegistrations migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllDomainVerificationChallenges")
  withCount (paginateSem selectAllChallenges (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(cid, _, _, _, _) -> handleErrors (toByteString' cid) (migrateDomainVerificationChallengeRow migCounter row)))

  lift $ info $ Log.msg (Log.val "migrateAllDomainRegistrations")
  withCount (paginateSem selectAllRegistrations (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize asRecord
    .| C.mapM_ (traverse_ (\row -> handleRegistrationErrors (toByteString' (show row.domain)) (migrateDomainRegistrationRow migCounter row)))

migrateDomainRegistrationRow ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r
  ) =>
  Prometheus.Counter ->
  StoredDomainRegistration ->
  Sem r ()
migrateDomainRegistrationRow migCounter row = do
  void . withMigrationLocks LockExclusive (Seconds 10) [row.domain] $ do
    isMigrated <- DomainRegistrationPostgres.exists row.domain
    unless isMigrated $ do
      cassClient <- input @ClientState
      mCurrentRow <-
        DomainRegistrationCassandra.interpretDomainRegistrationStoreToCassandra cassClient $
          lookupInternal row.domain
      for_ mCurrentRow $ \currentRow -> do
        DomainRegistrationPostgres.interpretDomainRegistrationStoreToPostgres $ upsertInternal currentRow
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

handleRegistrationErrors ::
  ( Member (State Int) r,
    Member TinyLog r
  ) =>
  ByteString ->
  (Sem (Error MigrationLockError : Error Hasql.UsageError : r) ()) ->
  Sem r ()
handleRegistrationErrors key action = do
  eithErr <- runError (runError action)
  case eithErr of
    Right (Right _) -> pure ()
    Right (Left e) -> logError (show e)
    Left e -> logError (show e)
  where
    logError e = do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field "key" (show key)
          . Log.field "error" e
      modify (+ 1)
