module Wire.Postgres where

import Hasql.Pipeline
import Hasql.Pool
import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions
import Hasql.Transaction.Sessions qualified as Transaction
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input

runStatement ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  a ->
  Statement a b ->
  Sem r b
runStatement a stmt = do
  pool <- input
  liftIO (use pool (Session.statement a stmt)) >>= either throw pure

runTransaction ::
  (Member (Input Pool) r, Member (Embed IO) r, Member (Error UsageError) r) =>
  IsolationLevel ->
  Mode ->
  Transaction a ->
  Sem r a
runTransaction isolationLevel mode t = do
  pool <- input
  liftIO (use pool $ Transaction.transaction isolationLevel mode t) >>= either throw pure

runPipeline ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Pipeline a ->
  Sem r a
runPipeline p = do
  pool <- input
  liftIO (use pool $ Session.pipeline p) >>= either throw pure
