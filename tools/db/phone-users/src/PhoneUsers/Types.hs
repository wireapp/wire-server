{-# LANGUAGE DeriveGeneric #-}

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
import Data.Handle
import Data.Id
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import Wire.API.User

data Opts = Opts
  { cHost :: String,
    cPort :: Int,
    cKeyspace :: C.Keyspace,
    limit :: Maybe Int
  }

sampleParser :: Parser Opts
sampleParser =
  Opts
    <$> strOption
      ( long "cassandra-host"
          <> short 's'
          <> metavar "HOST"
          <> help "Cassandra Host"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "cassandra-port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Cassandra Port"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> short 'k'
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace"
                  <> value "brig_test"
                  <> showDefault
              )
        )
    <*> optional
      ( option
          auto
          ( long "limit"
              <> short 'l'
              <> metavar "INT"
              <> help "Limit the number of users to process"
          )
      )

data Result = Result
  { usersSearched :: Int,
    phoneUsersTotal :: Int,
    inactivePhoneUsers :: Int,
    activePhoneUsers :: Int,
    activeTeamPhoneUsers :: Int,
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
        activePhoneUsers = r1.activePhoneUsers + r2.activePhoneUsers,
        activeTeamPhoneUsers = r1.activeTeamPhoneUsers + r2.activeTeamPhoneUsers,
        activeFreeTeamPhoneUsers = r1.activeFreeTeamPhoneUsers + r2.activeFreeTeamPhoneUsers,
        activePaidTeamPhoneUsers = r1.activePaidTeamPhoneUsers + r2.activePaidTeamPhoneUsers
      }

instance Monoid Result where
  mempty =
    Result
      { usersSearched = 0,
        phoneUsersTotal = 0,
        inactivePhoneUsers = 0,
        activePhoneUsers = 0,
        activeTeamPhoneUsers = 0,
        activeFreeTeamPhoneUsers = 0,
        activePaidTeamPhoneUsers = 0
      }

type Activated = Bool

type UserRow =
  ( UserId,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    Activated,
    Maybe AccountStatus,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

data TeamUser = Free | Paid
  deriving (Show)

data UserInfo = NoPhoneUser | PhoneUser PhoneUserInfo
  deriving (Show)

data PhoneUserInfo
  = InactiveLast90Days
  | ActivePersonalUser
  | ActiveTeamUser TeamUser
  deriving (Show)
