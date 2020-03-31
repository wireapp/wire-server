{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Brig.Index.Migrations.Types where

import qualified Brig.User.Search.Index as Search
import qualified Cassandra as C
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader (..), ReaderT, lift, runReaderT)
import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object, withObject)
import Data.Metrics (Metrics)
import qualified Database.V5.Bloodhound as ES
import Imports
import Numeric.Natural (Natural)
import qualified System.Logger as Logger
import System.Logger.Class (MonadLogger (..))

data Migration
  = Migration
      { version :: MigrationVersion,
        text :: Text,
        action :: MigrationActionT IO ()
      }

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

instance ToJSON MigrationVersion where
  toJSON (MigrationVersion v) = object ["migration_version" .= v]

instance FromJSON MigrationVersion where
  parseJSON = withObject "MigrationVersion" $ \o -> MigrationVersion <$> o .: "migration_version"

newtype MigrationActionT m a
  = MigrationActionT {unMigrationAction :: ReaderT Env m a}
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

instance MonadIO m => MonadLogger (MigrationActionT m) where
  log level f = do
    env <- ask
    Logger.log (logger env) level f

instance MonadIO m => Search.MonadIndexIO (MigrationActionT m) where
  liftIndexIO m = do
    Env {..} <- ask
    let indexEnv = Search.IndexEnv metrics logger bhEnv Nothing searchIndex
    Search.runIndexIO indexEnv m

instance MonadIO m => ES.MonadBH (MigrationActionT m) where
  getBHEnv = bhEnv <$> ask

data Env
  = Env
      { bhEnv :: ES.BHEnv,
        cassandraClientState :: C.ClientState,
        logger :: Logger.Logger,
        metrics :: Metrics,
        searchIndex :: ES.IndexName
      }

runMigrationAction :: Env -> MigrationActionT m a -> m a
runMigrationAction env action =
  runReaderT (unMigrationAction action) env

liftCassandra :: MonadIO m => C.Client a -> MigrationActionT m a
liftCassandra m = do
  env <- ask
  lift $ C.runClient (cassandraClientState env) m

cleanup :: MonadIO m => Env -> m ()
cleanup env = do
  C.shutdown (cassandraClientState env)
