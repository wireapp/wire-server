module Wire.Postgres where

import Hasql.Pool
import Hasql.Session
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
  liftIO (use pool (statement a stmt)) >>= either throw pure

runTransaction ::
  (Member (Input Pool) r, Member (Embed IO) r, Member (Error UsageError) r) =>
  IsolationLevel ->
  Mode ->
  Transaction a ->
  Sem r a
runTransaction isolationLevel mode t = do
  pool <- input
  liftIO (use pool $ Transaction.transaction isolationLevel mode t) >>= either throw pure
