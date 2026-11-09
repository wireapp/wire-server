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

module Wire.OAuthAuthCodeStore.Migration
  ( migrateOAuthAuthCodeLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id (OAuthClientId, UserId)
import Data.Text qualified as T
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.OAuth
import Wire.Migration
import Wire.OAuthAuthCodeStore.Postgres qualified as Postgres
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Util (embedClientInput)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

-- | Copy-only migration of @oauth_auth_code@ rows. The table only holds
-- short-lived (a few hundred seconds) single-use codes, so the loop runs
-- best-effort: rows that expire mid-migration are skipped, and unconsumed
-- codes TTL out on their own.
--
-- The TTL of each row is read as @ttl(client)@ and converted to an absolute
-- deadline at page-read time; a worker-vs-Cassandra clock skew of NTP scale
-- is accepted. Between the Cassandra existence probe and the Postgres insert
-- a consume can still delete the row; the stale copy survives only until
-- @expires_at@ (bounded by the configured code TTL, default 5 min) and is
-- unreachable to every read path once the deadline passes — accepted for this
-- copy-only migration pattern.
migrateOAuthAuthCodeLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateOAuthAuthCodeLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "oauth auth code"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "oauth auth code")
    (migrateAllOAuthAuthCode migOpts migCounter migDuration)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val name) .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . resourceToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . atomicStateToIO 0

migrateAllOAuthAuthCode ::
  ( Member (Input Hasql.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllOAuthAuthCode migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllOAuthAuthCode")
  withCount (paginateSem selectAllOAuthAuthCode (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( \page -> do
          -- ttl() is evaluated by Cassandra when the page is read; fix the
          -- absolute deadline here so rows buffered behind earlier rows cannot
          -- outlive their source TTL.
          readTime <- liftIO getCurrentTime
          traverse_
            ( \row@(code, _, _, _, _, _, _) ->
                -- The error key must not carry the raw code (RFC 9700 §5.7); the
                -- Show instance redacts it.
                handleErrors
                  (toByteString' (T.pack (show code)))
                  (migrateOAuthAuthCodeRow migOpts migCounter migDuration readTime row)
            )
            page
      )

migrateOAuthAuthCodeRow ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  UTCTime ->
  (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge, Maybe Int32) ->
  Sem r ()
migrateOAuthAuthCodeRow migOpts migCounter migDuration readTime (code, cid, uid, scope, uri, mChal, mttl)
  | maybe True (<= 0) mttl =
      debug $
        Log.msg (Log.val "skipping expired oauth auth code") . Log.field "code" (T.pack (show code))
  | otherwise = do
      -- The guard above guarantees a positive TTL.
      let expiresAt = addUTCTime (fromIntegral (fromMaybe 1 mttl)) readTime
      now <- liftIO getCurrentTime
      -- Rows whose absolute deadline already passed while buffered in the
      -- page are expired; copying them would create rows valid on their own
      -- clock, so skip.
      when (expiresAt > now) $ do
        stillLive <-
          embedClientInput
            (isJust <$> retry x1 (query1 selectLiveOAuthAuthCode (params LocalQuorum (Identity code))))
        when stillLive $ do
          let codeText = T.pack (show code)
          outcomeRef <- liftIO $ IORef.newIORef @Text "error"
          bracket
            (liftIO getCurrentTime)
            (observeDuration migDuration outcomeRef)
            ( const $ do
                timeoutResult <-
                  Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $
                    Postgres.insertOAuthAuthCodePg expiresAt code cid uid scope uri mChal
                case timeoutResult of
                  Left timedOutAfter -> do
                    markOutcome outcomeRef "timeout"
                    liftIO . UnliftIO.throwIO $ MigrationTimedOut codeText timedOutAfter
                  Right () -> do
                    markOutcome outcomeRef "success"
                    liftIO $ Prometheus.incCounter migCounter
            )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "oauth auth code migration timed out")
          . Log.field "code" (show code)
          . Log.field "timeout" (show migOpts.timeout)
    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome
    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

selectAllOAuthAuthCode ::
  PrepQuery
    R
    ()
    (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge, Maybe Int32)
selectAllOAuthAuthCode =
  "SELECT code, client, user, scope, redirect_uri, code_challenge, ttl(client) FROM oauth_auth_code"

-- Existence probe: a buffered row may have been consumed (deleted from
-- both stores) before its insert runs; copying it anyway would resurrect
-- a redeemed code.
selectLiveOAuthAuthCode ::
  PrepQuery R (Identity OAuthAuthorizationCode) (Identity OAuthAuthorizationCode)
selectLiveOAuthAuthCode = "SELECT code FROM oauth_auth_code WHERE code = ?"
