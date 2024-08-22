{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module ConferenceFeature.Types where

import Cassandra as C hiding (Set)
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Monoid
import Data.Text.Strict.Lens
import Database.CQL.Protocol hiding (Result, Set)
import Imports hiding (Sum)
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger, log)
import Wire.API.Team.Feature (FeatureStatus)
import Wire.API.User

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
      ( long "host"
          <> metavar "HOST"
          <> help "Cassandra Host for brig"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help "Cassandra Port for brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace
            . view packed
            <$> strOption
              ( long "keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for brig"
                  <> value "brig_test"
                  <> showDefault
              )
        )

type Activated = Bool

data Result = Result
  { usersTotal :: Sum Int,
    activeFreeUsersWithConferenceCalling :: Sum Int,
    activeTeamUsersWithConferenceCalling :: Sum Int,
    inactiveUsersWithConferenceCalling :: Sum Int,
    usersWithConferenceCallingDisabled :: Sum Int,
    affectedTeams :: Set TeamId
  }
  deriving (Generic, Eq)

instance Semigroup Result where
  Result a1 a2 a3 a4 a5 a6 <> Result b1 b2 b3 b4 b5 b6 = Result (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance Monoid Result where
  mempty = Result mempty mempty mempty mempty mempty mempty

data UserRow = UserRow
  { id :: UserId,
    activated :: Activated,
    status :: Maybe AccountStatus,
    team :: Maybe TeamId,
    conferenceCallingFeatureStatus :: Maybe FeatureStatus
  }
  deriving (Generic)

recordInstance ''UserRow

instance A.ToJSON (Sum Int) where
  toJSON (Sum i) = A.toJSON i

instance A.ToJSON Result

instance Show Result where
  show = LC8.unpack . A.encodePretty
