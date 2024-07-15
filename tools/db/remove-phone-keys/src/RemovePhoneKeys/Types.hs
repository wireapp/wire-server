-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module RemovePhoneKeys.Types where

import Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger, log)

data Env = Env
  { casClient :: C.ClientState,
    logger :: Log.Logger
  }

newtype AppT m a = AppT {unAppT :: ReaderT Env m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader Env,
      MonadUnliftIO
    )

instance MonadTrans AppT where
  lift = AppT . lift

instance (MonadIO m, MonadThrow m) => C.MonadClient (AppT m) where
  liftClient m = do
    env <- ask
    lift . C.runClient env.casClient $ m
  localState f = local (\env -> env {casClient = f $ env.casClient})

instance (MonadIO m) => MonadLogger (AppT m) where
  log level f = do
    env <- ask
    lift $ Log.log (env.logger) level f

mkEnv :: Opts -> IO Env
mkEnv opts = do
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  pure $ Env brigClient logger
  where
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings
  }

optsParser :: Parser Opts
optsParser =
  Opts <$> brigCassandraParser

brigCassandraParser :: Parser CassandraSettings
brigCassandraParser =
  CassandraSettings
    <$> strOption
      ( long "brig-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra Host for brig"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "brig-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port for brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace
            . view packed
            <$> strOption
              ( long "brig-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for brig"
                  <> value "brig_test"
                  <> showDefault
              )
        )

newtype IntSum = IntSum {unIntSum :: Int}
  deriving newtype (Num, Show)

instance Semigroup IntSum where
  IntSum a <> IntSum b = IntSum (a + b)

instance Monoid IntSum where
  mempty = IntSum 0
