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

module Wire.ScimTokenStore.Migration (migrateScimTokensLoop) where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
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
import SAML2.WebSSO qualified as SAML
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.PostgresMarshall (PostgresUnmarshall (..))
import Wire.API.User.Scim (ScimTokenInfo (..))
import {- instance Cql SAML.IdPId -} Wire.DomainRegistrationStore.Cassandra ()
import Wire.Migration
import Wire.Postgres
import Wire.ScimTokenStore.Postgres qualified as Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

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

migrateScimTokensLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateScimTokensLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "scim tokens"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "scim tokens")
    (migrateAllScimTokens migOpts migCounter migDuration)

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

-- | Full scan of the C* by-team table. The token lookup key (@token_@) is
-- copied verbatim so plaintext and hashed keys are preserved.
selectAllScimTokens :: PrepQuery R () (Text, UUID, UUID, UTCTime, Maybe SAML.IdPId, Text, Maybe Text)
selectAllScimTokens =
  "SELECT token_, team, id, created_at, idp, descr, name FROM team_provisioning_by_team"

migrateAllScimTokens ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllScimTokens migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllScimTokens")
  withCount (paginateSem selectAllScimTokens (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    -- NB: the row key (@token_@) is a live SCIM bearer credential for legacy
    -- rows, so it must never be used as a log field; identify rows by team/id.
    .| C.mapM_ (traverse_ (\row@(_, team, tid, _, _, _, _) -> handleErrors (encodeUtf8 (UUID.toText team <> "/" <> UUID.toText tid)) (migrateScimTokenRow migOpts migCounter migDuration row)))

migrateScimTokenRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Text, UUID, UUID, UTCTime, Maybe SAML.IdPId, Text, Maybe Text) ->
  Sem r ()
migrateScimTokenRow migOpts migCounter migDuration (tok, team, tid, createdAt, idp, descr, name) =
  for_ mkInfo $ \scimInfo ->
    for_ (hush (postgresUnmarshall tok)) $ \key -> do
      -- team/id pair: the token itself is a live bearer credential, never log it
      let rowId = UUID.toText team <> "/" <> UUID.toText tid
      outcomeRef <- liftIO $ IORef.newIORef @Text "error"
      bracket
        (liftIO getCurrentTime)
        (observeDuration migDuration outcomeRef)
        ( const $ do
            timeoutResult <-
              Conc.timeout (migOpts.timeout <$ handleTimeout rowId) migOpts.timeout $
                Postgres.upsertScimTokenByKey key scimInfo
            case timeoutResult of
              Left timedOutAfter -> do
                markOutcome outcomeRef "timeout"
                liftIO . UnliftIO.throwIO $ MigrationTimedOut rowId timedOutAfter
              Right () -> do
                markOutcome outcomeRef "success"
                liftIO $ Prometheus.incCounter migCounter
        )
  where
    mkInfo :: Maybe ScimTokenInfo
    mkInfo = do
      team' <- hush (postgresUnmarshall team)
      tid' <- hush (postgresUnmarshall tid)
      idp' <- hush (postgresUnmarshall idp)
      pure
        ScimTokenInfo
          { stiTeam = team',
            stiId = tid',
            stiCreatedAt = createdAt,
            stiIdP = idp',
            stiDescr = descr,
            stiName = fromMaybe (idToText tid') name
          }

    handleTimeout rowId =
      err $
        Log.msg (Log.val "scim token migration timed out")
          . Log.field "team_id" rowId
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
