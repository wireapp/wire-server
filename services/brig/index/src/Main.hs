{-# LANGUAGE OverloadedStrings #-}

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

-- | One-off backfill tool for @wire_user.name_normalized@ (required by
-- 'Wire.UserSearchStore' after the ElasticSearch user index was removed).
-- Run once per deployment before switching user search over to Postgres:
--
-- > brig-index backfill-normalized-names --pg-settings "host=... dbname=... user=... password=..."
module Main (main) where

import Data.Functor.Contravariant ((>$<))
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Settings qualified as HasqlSettings
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors (IsError (..), toDetailedText)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement
import Imports
import Options.Applicative
import System.Exit (exitFailure)
import Wire.UserSearch.Normalize (normalized)

data Opts = Opts
  { pgSettings :: Text,
    batchSize :: Int32
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption
      ( long "pg-settings"
          <> metavar "SETTINGS"
          <> help "libpq connection settings, e.g. \"host=... dbname=... user=... password=...\""
      )
    <*> option
      auto
      ( long "batch-size"
          <> metavar "N"
          <> value 500
          <> showDefault
          <> help "number of users per batch"
      )

-- | Fetches up to N users with a missing @name_normalized@.
selectBatch :: Statement Int32 [(UUID, Text)]
selectBatch =
  Statement.preparable
    "SELECT id :: uuid, name :: text FROM wire_user WHERE name_normalized IS NULL AND name IS NOT NULL ORDER BY id LIMIT ($1 :: int4)"
    (const (0 :: Int32) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
    (Decoders.rowList ((,) <$> Decoders.column (Decoders.nonNullable Decoders.uuid) <*> Decoders.column (Decoders.nonNullable Decoders.text)))

updateOne :: Statement (UUID, Text) ()
updateOne =
  Statement.preparable
    "UPDATE wire_user SET name_normalized = ($2 :: text) WHERE id = ($1 :: uuid)"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult

main :: IO ()
main = do
  opts <- execParser (info (optsParser <**> helper) (fullDesc <> progDesc "Backfill wire_user.name_normalized with the ICU-folded lowercase display name"))
  conn <- do
    r <- Hasql.acquire (HasqlSettings.connectionString opts.pgSettings)
    either (failWith "connecting to postgres") pure r
  let loop total = do
        rows <- runSession conn (Session.statement opts.batchSize selectBatch)
        case rows of
          [] ->
            TextIO.putStrLn
              ("backfill-normalized-names: done, updated " <> Text.pack (show (total :: Int)) <> " users")
          batch -> do
            forM_ batch $ \(uid, name) ->
              runSession conn (Session.statement (uid, normalized name) updateOne)
            loop (total + length batch)
  loop 0

failWith :: (IsError e) => Text -> e -> IO a
failWith context err = do
  TextIO.putStrLn ("backfill-normalized-names: " <> context <> ": " <> toDetailedText err)
  exitFailure

runSession :: Hasql.Connection -> Session.Session a -> IO a
runSession conn sess = do
  r <- Hasql.use conn sess
  either (failWith "postgres query") pure r
