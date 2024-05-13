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

import Cassandra as C
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import Data.Misc
import qualified Data.Set as Set
import Data.Text.Strict.Lens
import Database.CQL.Protocol hiding (Set)
import Imports
import qualified Network.HTTP.Client as HTTP
import OpenSSL.Session as Ssl
import Options.Applicative
import Wire.API.Provider (ServiceToken)
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamStatus)

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings,
    galleyDb :: CassandraSettings,
    limit :: Maybe Int
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> brigCassandraParser
    <*> galleyCassandraParser
    <*> optional
      ( option
          auto
          ( long "limit"
              <> short 'l'
              <> metavar "INT"
              <> help "Limit the number of users to process"
          )
      )

galleyCassandraParser :: Parser CassandraSettings
galleyCassandraParser =
  CassandraSettings
    <$> strOption
      ( long "galley-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra Host for galley"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "galley-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port for galley"
          <> value 9043
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "galley-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for galley"
                  <> value "galley"
                  <> showDefault
              )
        )

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
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "brig-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for brig"
                  <> value "brig"
                  <> showDefault
              )
        )

data ExternalServiceSettings = ExternalServiceSettings
  { manager :: HTTP.Manager,
    verifyFingerprints :: [Fingerprint Rsa] -> Ssl.SSL -> IO ()
  }

data Service = Service
  { provider :: ProviderId,
    service :: ServiceId,
    name :: Text,
    baseUrl :: HttpsUrl,
    backendActive :: Bool
  }
  deriving (Eq, Ord, Generic)

instance A.ToJSON Service

data TeamsWithService = TeamsWithService
  { entriesSearched :: Int,
    teams :: MonoidalMap TeamId (Set.Set Service)
  }
  deriving (Eq, Generic)

instance Semigroup TeamsWithService where
  TeamsWithService a b <> TeamsWithService c d = TeamsWithService (a + c) (b <> d)

instance Monoid TeamsWithService where
  mempty = TeamsWithService 0 mempty

instance A.ToJSON TeamsWithService

instance Show TeamsWithService where
  show = LC8.unpack . A.encodePretty

data ServiceTeamRow = ServiceTeamRow
  { team :: TeamId,
    provider :: ProviderId,
    service :: ServiceId
  }
  deriving (Generic)

recordInstance ''ServiceTeamRow

instance A.ToJSON ServiceTeamRow

instance Show ServiceTeamRow where
  show = LC8.unpack . A.encodePretty

data TeamRow = TeamRow
  { teamId :: TeamId,
    deleted :: Bool,
    status :: Maybe TeamStatus
  }
  deriving (Generic)

recordInstance ''TeamRow

instance A.ToJSON TeamRow

instance Show TeamRow where
  show = LC8.unpack . A.encodePretty

data ServiceRow = ServiceRow
  { baseUrl :: HttpsUrl,
    authToken :: ServiceToken,
    fingerprints :: C.Set (Fingerprint Rsa),
    enabled :: Bool
  }
  deriving (Generic)

recordInstance ''ServiceRow

toTeamsWithService :: Text -> HttpsUrl -> Bool -> ServiceTeamRow -> TeamsWithService
toTeamsWithService name url isBackendActive sr =
  mempty
    & MM.insert sr.team (Set.fromList [Service sr.provider sr.service name url isBackendActive])
    & TeamsWithService 1
