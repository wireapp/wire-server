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

-- | Application-level data migration giving every meeting a live join code
-- (and therefore a join link; WPB-29361).
--
-- * The scan is idempotent: only meetings without a live join code are acted
--   on, and code creation is an atomic insert-if-absent on the deterministic
--   (meeting-id-derived) key, so concurrent replicas converge — the loser
--   observes the winner's code instead of replacing it. Existing codes are
--   never touched or rotated, so every previously issued join link keeps
--   working.
--
-- * The full-table keyset scan every pass is accepted for current meetings
--   table sizes; the existence check runs through the PK-indexed bulk code
--   query, so no new SQL access leaves the existing indices (and no DDL is
--   added).
--
-- * In Cassandra-only mode the worker is not started: meeting codes cannot
--   be stored in Cassandra, so galley logs a single configuration error
--   instead of scanning in vain.
module Wire.MeetingsSubsystem.LinkBackfill
  ( backfillMeetingJoinCodes,
    backfillPage,
    runMeetingJoinCodeBackfill,
  )
where

import Cassandra (ClientState)
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id (Id (..), MeetingId)
import Data.Map.Strict qualified as Map
import Data.Misc (HttpsUrl)
import Data.Time.Clock (NominalDiffTime)
import Data.UUID qualified as UUID
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import UnliftIO (throwIO, try)
import Wire.API.Error (ErrorS, runErrorS)
import Wire.API.Error.Galley (GalleyError (CodeStoreNotFound))
import Wire.CodeStore
import Wire.CodeStore qualified as CodeStore
import Wire.CodeStore.Cassandra (interpretCodeStoreToCassandra)
import Wire.CodeStore.DualWrite (interpretCodeStoreToCassandraAndPostgres)
import Wire.CodeStore.Postgres (interpretCodeStoreToPostgres)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsStore.Postgres (interpretMeetingsStoreToPostgres)
import Wire.MeetingsSubsystem.Interpreter (meetingCodeTimeout)
import Wire.PostgresMigrationOpts (StorageLocation (..))
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

-- | Forever-loop that gives every meeting without a live join code one,
-- paging keyset-wise through the meetings table. @interval@ is the pause
-- between full passes, @pageSize@ the scan page size. Codes are created with
-- no password and the standard 'meetingCodeTimeout'; existing codes are
-- never touched, so existing links keep working. Cassandra-only deployments
-- never start this worker (see 'runMeetingJoinCodeBackfill').
backfillMeetingJoinCodes ::
  ( Member Store.MeetingsStore r,
    Member CodeStore r,
    Member TinyLog r,
    Member (Embed IO) r
  ) =>
  NominalDiffTime ->
  Int ->
  Sem r a
backfillMeetingJoinCodes interval pageSize = loop firstCursor
  where
    -- The 'MeetingId' cursor is exclusive; start below every possible id.
    firstCursor = Id UUID.nil
    delayMicros = floor (realToFrac interval * 1000000 :: Double) :: Int
    loop cursor = do
      next <- backfillPage pageSize cursor
      case next of
        Nothing -> do
          embed (threadDelay delayMicros)
          loop firstCursor
        Just c -> loop c

-- | One keyset page of the backfill scan (see 'backfillMeetingJoinCodes').
-- Returns the next exclusive cursor, or 'Nothing' when the scan reached the
-- end of the meetings table. Separate from the loop so tests can drive a
-- single pass.
backfillPage ::
  ( Member Store.MeetingsStore r,
    Member CodeStore r,
    Member TinyLog r
  ) =>
  Int ->
  -- | Exclusive cursor.
  MeetingId ->
  Sem r (Maybe MeetingId)
backfillPage pageSize cursor = do
  batch <- Store.getMeetingIdsWithoutCode cursor pageSize
  case batch of
    [] -> pure Nothing
    _ -> do
      codes <- CodeStore.getMeetingCodes (map fst batch)
      forM_ batch $ \(mid, _convId) ->
        unless (mid `Map.member` codes) $ do
          -- Insert-if-absent, not createMeetingCode: the upsert would let a
          -- concurrent replica replace the winner's code value and
          -- invalidate an already-served join link.
          res <- CodeStore.backfillMeetingCode mid meetingCodeTimeout
          case res of
            BackfilledCode _ -> pure ()
            BackfillCodePresent -> pure ()
            BackfillCodeStoreUnsupported ->
              TinyLog.err $
                Log.msg ("cannot backfill meeting join code" :: ByteString)
                  . Log.field "meetingId" (toByteString' mid)
      pure (Just (fst (last batch)))

-- | Standalone runner mirroring 'Wire.CodeStore.Migration.migrateCodesLoop':
-- interprets 'Store.MeetingsStore' (Postgres) and 'CodeStore' with the same
-- interpreter galley itself selects for @postgresMigration.conversationCodes@,
-- plus 'TinyLog' over the given logger. Never returns: any failure of a pass
-- (transient 'UsageError', unexpected exception) is logged loudly and the
-- loop restarts after @interval@, mirroring the @safeForever@ convention of
-- galley's other background loops.
runMeetingJoinCodeBackfill ::
  StorageLocation ->
  ClientState ->
  HasqlPoolExt.Pool ->
  Log.Logger ->
  Either HttpsUrl (Map Domain HttpsUrl) ->
  ByteString ->
  NominalDiffTime ->
  Int ->
  IO ()
runMeetingJoinCodeBackfill storeMode cassClient pgPool logger convCodeURI name interval pageSize =
  case storeMode of
    CassandraStorage ->
      -- Meeting codes cannot be stored in Cassandra: the worker could never
      -- make progress (every pass would scan the whole table and log one
      -- error per meeting). Fail loudly once instead of looping in vain.
      Log.err logger $
        Log.msg ("meeting join code backfill disabled" :: ByteString)
          . Log.field "backfill" name
          . Log.field "reason" ("postgresMigration.conversationCodes=cassandra: meeting codes cannot be stored in Cassandra; use migration-to-postgresql or postgresql" :: ByteString)
    _ -> forever $ do
      result <-
        try
          $ runFinal
            . embedToFinal
            . loggerToTinyLog logger
            . mapLogger (Log.field "backfill" (Log.val name) .)
            . runError @UsageError
            . runErrorS @'CodeStoreNotFound
            . runInputConst @(Either HttpsUrl (Map Domain HttpsUrl)) convCodeURI
            . runInputConst @HasqlPoolExt.Pool pgPool
            . runInputConst @ClientState cassClient
            . interpretMeetingsStoreToPostgres
            . codeStoreInterpreter
          $ backfillMeetingJoinCodes interval pageSize
      let logFail e =
            Log.err logger $
              Log.msg ("meeting join code backfill pass failed" :: ByteString)
                . Log.field "backfill" name
                . Log.field "error" e
      case result of
        -- impossible: the loop never returns
        Right (Right (Just ())) -> pure ()
        -- 'CodeStoreNotFound' is never produced by this worker's code-store
        -- ops, but the interpreter chain must handle it.
        Right (Right Nothing) ->
          Log.err logger $
            Log.msg ("code store not found during meeting join code backfill" :: ByteString)
              . Log.field "backfill" name
        Right (Left usageError) -> logFail (show usageError)
        Left (exc :: SomeException) ->
          -- Never swallow async exceptions: 'withAsync' delivers cancellation
          -- at galley shutdown, and resuming the loop would hang shutdown.
          case fromException exc of
            Just (ae :: SomeAsyncException) -> throwIO ae
            Nothing -> logFail (displayException exc)
      threadDelay (floor (realToFrac interval * 1000000 :: Double))
  where
    codeStoreInterpreter ::
      ( Member (Input ClientState) r,
        Member (Input HasqlPoolExt.Pool) r,
        Member (Input (Either HttpsUrl (Map Domain HttpsUrl))) r,
        Member (Embed IO) r,
        Member (Error UsageError) r,
        Member (ErrorS 'CodeStoreNotFound) r
      ) =>
      InterpreterFor CodeStore r
    codeStoreInterpreter = case storeMode of
      -- unreachable from the runner (the gate above rejects CassandraStorage
      -- before the loop) but kept for exhaustiveness: 'StorageLocation' has
      -- no 'Eq'.
      CassandraStorage -> interpretCodeStoreToCassandra
      MigrationToPostgresql -> interpretCodeStoreToCassandraAndPostgres
      PostgresqlStorage -> interpretCodeStoreToPostgres
