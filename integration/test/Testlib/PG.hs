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
module Testlib.PG (execSql, queryFanInSql) where

import Control.Exception (try)
import qualified Data.ByteString.Lazy as BS
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process
import Testlib.Prelude

-- | Execute a SQL statement against the given backend's galley Postgres
-- database by spawning @psql@ with the connection parameters read from
-- galley's configuration. Dynamic backends share the static Postgres server
-- and differ only in @postgresql.dbname@, which is taken from the backend
-- resource. The optional top-level @postgresqlPassword@ secret file takes
-- precedence over the optional inline @postgresql.password@ value — same
-- precedence as galley (see 'lookupPassword' for the accepted file formats).
-- Fails if @psql@ exits non-zero.
execSql :: (HasCallStack) => BackendResource -> Text -> App ()
execSql resource sql = do
  cfg <- readServiceConfig Galley
  host <- cfgStr cfg "postgresql.host"
  port <- cfgStr cfg "postgresql.port"
  let dbname = resource.berPostgresqlDBName
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
-- the inline @postgresql.password@ value. The file is expected to hold a
-- JSON-encoded string (as consumed by galley itself); if it is not valid
-- JSON, its raw (trimmed) content is used as the password. A configured but
-- unreadable file is a hard error.
lookupPassword :: (HasCallStack) => Value -> App (Maybe String)
lookupPassword = lookupPasswordAt "postgresqlPassword" "postgresql.password"

lookupPasswordAt :: (HasCallStack) => String -> String -> Value -> App (Maybe String)
lookupPasswordAt secretKey inlineKey cfg =
  lookupField cfg secretKey >>= \case
    Just pathVal -> do
      path <- requireScalar secretKey pathVal
      contents <- liftIO (try (BS.readFile path)) >>= either (\e -> failApp ("Testlib.PG: cannot read password file " <> path <> ": " <> show (e :: IOException))) pure
      case eitherDecode contents of
        Right pw -> pure (Just pw)
        -- Secret files rendered by the helm charts (e.g. from a plain-text
        -- @secrets.pgPassword@ value) hold the raw password rather than a
        -- JSON string; accept the raw content as a fallback.
        Left _ -> do
          let raw = T.unpack (T.strip (decodeUtf8 (BS.toStrict contents)))
          when (null raw) $ failApp ("Testlib.PG: password file " <> path <> " is empty")
          pure (Just raw)
    Nothing ->
      lookupField cfg inlineKey >>= \case
        Just (String t) -> pure (Just (T.unpack t))
        Nothing -> pure Nothing
        Just v -> Just <$> requireScalar inlineKey v
  where
    requireScalar k v = case v of
      String t -> pure (T.unpack t)
      _ -> failApp ("Testlib.PG: config key " <> show k <> " is missing or not a scalar")

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
    scalarFail k = failApp ("Testlib.PG: config key " <> show k <> " is missing or not a scalar")

-- | Query the separate fan-in database, binding test values through psql
-- variables and :'name' SQL literals rather than interpolating SQL strings.
-- Query scripts must return exactly one JSON value. Connection secrets are
-- passed only through the child environment and are never printed.
-- TODO: Remove this temporary helper when Test.FanInNotifications can assert
-- delivery and replay through the complete fan-in WebSocket round trip.
queryFanInSql :: (HasCallStack) => Text -> [(String, String)] -> App Value
queryFanInSql sql params = do
  cfg <- readServiceConfig Gundeck
  host <- cfgStr cfg "notificationFanInPostgres.connection.host"
  port <- cfgStr cfg "notificationFanInPostgres.connection.port"
  database <- cfgStr cfg "notificationFanInPostgres.connection.dbname"
  user <- cfgStr cfg "notificationFanInPostgres.connection.user"
  password <- lookupPasswordAt "notificationFanInPostgres.password" "notificationFanInPostgres.connection.password" cfg
  baseEnv <- liftIO getEnvironment
  let args =
        ["-X", "-w", "-t", "-A", "-h", host, "-p", port, "-d", database, "-U", user, "-v", "ON_ERROR_STOP=1"]
          <> concatMap (\(name, val) -> ["-v", name <> "=" <> val]) params
      cp = (proc "psql" args) {env = Just (maybe baseEnv (\pw -> ("PGPASSWORD", pw) : filter ((/= "PGPASSWORD") . fst) baseEnv) password)}
  (status, out, _) <- liftIO $ readCreateProcessWithExitCode cp (T.unpack sql)
  case status of
    ExitFailure _ -> failApp "Fan-in database query failed"
    ExitSuccess -> either (const (failApp "Fan-in database query returned invalid JSON")) pure (eitherDecode (BS.fromStrict (encodeUtf8 (T.pack out))))
