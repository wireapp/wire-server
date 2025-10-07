module Wire.Postgres where

import Hasql.Pool
import Hasql.Session
import Hasql.Statement
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
