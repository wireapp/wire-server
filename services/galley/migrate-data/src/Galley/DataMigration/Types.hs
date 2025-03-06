{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.DataMigration.Types where

import Cassandra qualified as C
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Imports
import Numeric.Natural (Natural)
import System.Logger qualified as Logger
import System.Logger.Class (MonadLogger (..))

data Migration = Migration
  { version :: MigrationVersion,
    text :: Text,
    action :: MigrationActionT IO ()
  }

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

newtype MigrationActionT m a = MigrationActionT {unMigrationAction :: ReaderT Env m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch
    )

instance MonadTrans MigrationActionT where
  lift = MigrationActionT . lift

instance (MonadIO m, MonadThrow m) => C.MonadClient (MigrationActionT m) where
  liftClient = liftCassandra
  localState f = local (\env -> env {cassandraClientState = f $ cassandraClientState env})

instance (MonadIO m) => MonadLogger (MigrationActionT m) where
  log level f = do
    env <- ask
    Logger.log (logger env) level f

data Env = Env
  { cassandraClientState :: C.ClientState,
    logger :: Logger.Logger
  }

runMigrationAction :: Env -> MigrationActionT m a -> m a
runMigrationAction env action =
  runReaderT (unMigrationAction action) env

liftCassandra :: (MonadIO m) => C.Client a -> MigrationActionT m a
liftCassandra m = do
  env <- ask
  lift $ C.runClient (cassandraClientState env) m

cleanup :: (MonadIO m) => Env -> m ()
cleanup env = do
  C.shutdown (cassandraClientState env)
  Logger.close (logger env)
