{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Bots.Types where

import Cassandra as C hiding (Set)
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import Data.Set
import qualified Data.Set as Set
import Data.Text.Strict.Lens
import Database.CQL.Protocol hiding (Set)
import Imports
import Options.Applicative

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings,
    limit :: Maybe Int
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> cassandraSettingsParser
    <*> optional
      ( option
          auto
          ( long "limit"
              <> short 'l'
              <> metavar "INT"
              <> help "Limit the number of users to process"
          )
      )

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser =
  CassandraSettings
    <$> strOption
      ( long "cassandra-host"
          <> short 's'
          <> metavar "HOST"
          <> help "Cassandra Host for brig"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "cassandra-port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Cassandra Port for brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> short 'k'
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for brig"
                  <> value "brig_test"
                  <> showDefault
              )
        )

data TeamsWithService = TeamsWithService
  { entriesSearched :: Int,
    teams :: MonoidalMap TeamId (Set (ProviderId, ServiceId))
  }
  deriving (Eq, Generic)

instance Semigroup TeamsWithService where
  TeamsWithService a b <> TeamsWithService c d = TeamsWithService (a + c) (b <> d)

instance Monoid TeamsWithService where
  mempty = TeamsWithService 0 mempty

instance A.ToJSON TeamsWithService

instance Show TeamsWithService where
  show = LC8.unpack . A.encodePretty

data ServiceRow = ServiceRow
  { provider :: ProviderId,
    service :: ServiceId,
    team :: TeamId
  }
  deriving (Show, Generic)

recordInstance ''ServiceRow

toTeamsWithService :: ServiceRow -> TeamsWithService
toTeamsWithService sr =
  mempty
    & MM.insert sr.team (Set.fromList [(sr.provider, sr.service)])
    & TeamsWithService 1
