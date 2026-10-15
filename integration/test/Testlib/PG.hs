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

-- | Helpers for executing raw SQL against the galley Postgres database from
-- integration tests (via the @psql@ binary).
module Testlib.PG (execSql) where

import Control.Exception (try)
import qualified Data.ByteString.Lazy as BS
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process
import Testlib.Prelude

-- | Execute a SQL statement against galley's Postgres database by spawning
-- @psql@ with the connection parameters read from galley's configuration.
-- The optional top-level @postgresqlPassword@ secret file (a JSON-encoded
-- string, as consumed by galley itself) takes precedence over the optional
-- inline @postgresql.password@ value — same precedence as galley. Fails if
-- @psql@ exits non-zero.
execSql :: (HasCallStack) => Text -> App ()
execSql sql = do
  cfg <- readServiceConfig Galley
  host <- cfgStr cfg "postgresql.host"
  port <- cfgStr cfg "postgresql.port"
  dbname <- cfgStr cfg "postgresql.dbname"
  user <- cfgStr cfg "postgresql.user"
  mPassword <- lookupPassword cfg
  baseEnv <- liftIO getEnvironment
  let cp =
        ( proc
            "psql"
            ["-w", "-h", host, "-p", port, "-d", dbname, "-U", user, "-v", "ON_ERROR_STOP=1", "-c", T.unpack sql]
        )
          { env = Just (maybe baseEnv (\pw -> ("PGPASSWORD", pw) : filter ((/= "PGPASSWORD") . fst) baseEnv) mPassword)
          }
  (_, _, _, ph) <- liftIO $ createProcess cp
  exitCode <- liftIO $ waitForProcess ph
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      failApp
        $ "psql failed (exit "
        <> show code
        <> ") running: "
        <> T.unpack sql

-- | Resolve the DB password: the @postgresqlPassword@ secret file wins over
-- the inline @postgresql.password@ value. A configured but unreadable or
-- undecodable file is a hard error (mirroring galley's own behavior).
lookupPassword :: (HasCallStack) => Value -> App (Maybe String)
lookupPassword cfg =
  lookupField cfg "postgresqlPassword" >>= \case
    Just pathVal -> do
      path <- requireScalar "postgresqlPassword" pathVal
      contents <- liftIO (try (BS.readFile path)) >>= either (\e -> failApp ("Testlib.PG.execSql: cannot read postgresqlPassword file " <> path <> ": " <> show (e :: IOException))) pure
      case eitherDecode contents of
        Right pw -> pure (Just pw)
        Left err -> failApp ("Testlib.PG.execSql: postgresqlPassword file " <> path <> " does not contain a JSON string: " <> err)
    Nothing ->
      lookupField cfg "postgresql.password" >>= \case
        Just (String t) -> pure (Just (T.unpack t))
        Nothing -> pure Nothing
        Just v -> Just <$> requireScalar "postgresql.password" v
  where
    requireScalar k v = case v of
      String t -> pure (T.unpack t)
      _ -> failApp ("Testlib.PG.execSql: config key " <> show k <> " of galley is missing or not a scalar")

-- | Read a scalar (string or integer) config value.
cfgStr :: (HasCallStack) => Value -> String -> App String
cfgStr cfg key = do
  mv <- lookupField cfg key
  case mv of
    Just (String t) -> pure (T.unpack t)
    Just (Number n) -> case toBoundedInteger n of
      Just (i :: Int) -> pure (show i)
      Nothing -> scalarFail key
    _ -> scalarFail key
  where
    scalarFail k = failApp ("Testlib.PG.execSql: config key " <> show k <> " of galley is missing or not a scalar")
