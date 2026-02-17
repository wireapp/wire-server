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

module Wire.Migration where

import Cassandra
import Cassandra.Settings
import Data.Aeson
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import GHC.Generics (Generically (..))
import Hasql.Pool qualified as Hasql
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.Util (embedClient)

data MigrationOptions = MigrationOptions
  { pageSize :: Int32,
    parallelism :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via Generically MigrationOptions

migrationLoop ::
  Log.Logger ->
  ByteString ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  (Sem r () -> IO (Int, a)) ->
  ConduitT () Void (Sem r) () ->
  IO ()
migrationLoop logger name migFinished migFailed interpreter migration = do
  go 0 `UnliftIO.catch` handleIOError
  where
    handleIOError :: SomeException -> IO ()
    handleIOError exc = do
      Prometheus.incCounter migFailed
      Log.err logger $
        Log.msg (Log.val "migration failed, it won't restart unless the background-worker is restarted.")
          . Log.field "migration" name
          . Log.field "error" (displayException exc)
      UnliftIO.throwIO exc

    go :: Int -> IO ()
    go nIter = do
      runMigration >>= \case
        0 -> do
          Log.info logger $
            Log.msg (Log.val "finished migration")
              . Log.field "attempt" nIter
              . Log.field "migration" name
          Prometheus.incCounter migFinished
        n -> do
          Log.info logger $
            Log.msg (Log.val "finished migration with errors")
              . Log.field "migration" name
              . Log.field "errors" n
              . Log.field "attempt" nIter
          go (nIter + 1)

    runMigration :: IO Int
    runMigration =
      fmap fst
        . interpreter
        $ runConduit migration

logRetrievedPage :: (Member TinyLog r) => Int32 -> (a -> b) -> ConduitM (Int32, [a]) [b] (Sem r) ()
logRetrievedPage pageSize toRow =
  C.mapM
    ( \(i, rows) -> do
        let estimatedRowsSoFar = (i - 1) * pageSize + fromIntegral (length rows)
        info $ Log.msg (Log.val "retrieved page") . Log.field "estimatedRowsSoFar" estimatedRowsSoFar
        pure $ map toRow rows
    )

withCount :: (Monad m) => ConduitM () [a] m () -> ConduitM () (Int32, [a]) m ()
withCount = zipSources (C.sourceList [1 ..])

paginateSem ::
  forall a b q r.
  ( Tuple a,
    Tuple b,
    RunQ q,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (Embed IO) r
  ) =>
  q R a b ->
  QueryParams a ->
  RetrySettings ->
  ConduitT () [b] (Sem r) ()
paginateSem q p r = do
  go =<< lift getFirstPage
  where
    go page = do
      lift $ info $ Log.msg (Log.val "got a page")
      unless (null (result page)) $
        yield (result page)
      when (hasMore page) $
        go =<< lift (getNextPage page)

    getFirstPage :: Sem r (Page b)
    getFirstPage = do
      client <- input
      embedClient client $ retry r (paginate q p)

    getNextPage :: Page b -> Sem r (Page b)
    getNextPage page = do
      client <- input
      embedClient client $ retry r (nextPage page)

handleErrors ::
  forall r.
  ( Member (State Int) r,
    Member TinyLog r
  ) =>
  ByteString ->
  (Sem (Error Hasql.UsageError : r) ()) ->
  Sem r ()
handleErrors key action = do
  eithErr <- runError action
  case eithErr of
    Right _ -> pure ()
    Left e -> do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field "key" (show key)
          . Log.field "error" (show e)
      modify (+ 1)
