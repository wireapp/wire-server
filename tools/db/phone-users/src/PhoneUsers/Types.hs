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

module PhoneUsers.Types where

import Cassandra as C
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Text.Strict.Lens
import Database.CQL.Protocol hiding (Result)
import Imports
import Options.Applicative
import Wire.API.User

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
    <*> ( C.Keyspace
            . view packed
            <$> strOption
              ( long "galley-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for galley"
                  <> value "galley_test"
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

data Result = Result
  { usersSearched :: Int,
    phoneUsersTotal :: Int,
    inactivePhoneUsers :: Int,
    activePersonalPhoneUsers :: Int,
    activeFreeTeamPhoneUsers :: Int,
    activePaidTeamPhoneUsers :: Int
  }
  deriving (Generic)

instance A.ToJSON Result

instance Show Result where
  show = LC8.unpack . A.encodePretty

instance Semigroup Result where
  r1 <> r2 =
    Result
      { usersSearched = r1.usersSearched + r2.usersSearched,
        phoneUsersTotal = r1.phoneUsersTotal + r2.phoneUsersTotal,
        inactivePhoneUsers = r1.inactivePhoneUsers + r2.inactivePhoneUsers,
        activePersonalPhoneUsers = r1.activePersonalPhoneUsers + r2.activePersonalPhoneUsers,
        activeFreeTeamPhoneUsers = r1.activeFreeTeamPhoneUsers + r2.activeFreeTeamPhoneUsers,
        activePaidTeamPhoneUsers = r1.activePaidTeamPhoneUsers + r2.activePaidTeamPhoneUsers
      }

instance Monoid Result where
  mempty =
    Result
      { usersSearched = 0,
        phoneUsersTotal = 0,
        inactivePhoneUsers = 0,
        activePersonalPhoneUsers = 0,
        activeFreeTeamPhoneUsers = 0,
        activePaidTeamPhoneUsers = 0
      }

type Activated = Bool

data UserRow = UserRow
  { id :: UserId,
    email :: Maybe Email,
    phone :: Maybe Phone,
    activated :: Activated,
    status :: Maybe AccountStatus,
    team :: Maybe TeamId
  }
  deriving (Generic)

instance A.ToJSON UserRow

recordInstance ''UserRow

instance Show UserRow where
  show = LC8.unpack . A.encodePretty

data TeamUser = Free | Paid
  deriving (Show)

data UserInfo = NoPhoneUser | PhoneUser PhoneUserInfo
  deriving (Show)

data PhoneUserInfo
  = InactiveLast90Days
  | ActivePersonalUser
  | ActiveTeamUser TeamUser
  deriving (Show)
