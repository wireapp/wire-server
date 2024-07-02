{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

module Brig.Index.Migrations.Types where

import Brig.User.Search.Index qualified as Search
import Cassandra qualified as C
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Credentials (Credentials)
import Database.Bloodhound qualified as ES
import Imports
import Network.HTTP.Client (Manager)
import Numeric.Natural (Natural)
import System.Logger qualified as Logger
import System.Logger.Class (MonadLogger (..), ToBytes (..))
import Util.Options (Endpoint)

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

instance ToJSON MigrationVersion where
  toJSON (MigrationVersion v) = object ["migration_version" .= v]

instance FromJSON MigrationVersion where
  parseJSON = withObject "MigrationVersion" $ \o -> MigrationVersion <$> o .: "migration_version"

instance ToBytes MigrationVersion where
  bytes = bytes . toInteger . migrationVersion

newtype MigrationActionT m a = MigrationActionT {unMigrationAction :: ReaderT Env m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader Env
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

instance (MonadIO m) => Search.MonadIndexIO (MigrationActionT m) where
  liftIndexIO m = do
    Env {..} <- ask
    let indexEnv = Search.IndexEnv logger bhEnv Nothing searchIndex Nothing Nothing galleyEndpoint httpManager searchIndexCredentials
    Search.runIndexIO indexEnv m

instance (MonadIO m) => ES.MonadBH (MigrationActionT m) where
  getBHEnv = bhEnv <$> ask

data Env = Env
  { bhEnv :: ES.BHEnv,
    cassandraClientState :: C.ClientState,
    logger :: Logger.Logger,
    searchIndex :: ES.IndexName,
    searchIndexCredentials :: Maybe Credentials,
    httpManager :: Manager,
    galleyEndpoint :: Endpoint
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
