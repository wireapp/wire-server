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

module DataUsagePermissions.Types where

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
    limit :: Maybe Int
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> brigCassandraParser
    <*> optional
      ( option
          auto
          ( long "limit"
              <> short 'l'
              <> metavar "INT"
              <> help "Limit the number of users to process"
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
                  <> value "brig_test"
                  <> showDefault
              )
        )

type Activated = Bool

data UserRow = UserRow
  { id :: UserId,
    activated :: Activated,
    status :: Maybe AccountStatus,
    team :: Maybe TeamId
  }
  deriving (Generic)

instance A.ToJSON UserRow

recordInstance ''UserRow

instance Show UserRow where
  show = LC8.unpack . A.encodePretty

data UserConsentInfo = UserConsentInfo
  { consentToCrashReports :: Bool,
    consentToUsageData :: Bool
  }
  deriving (Generic)

newtype ConsentToNewsLetter = ConsentToNewsLetter
  { consentToNewsLetter :: Bool
  }
  deriving (Generic)

data WebAppUser = NoWebUser ConsentToNewsLetter | WebAppUser ConsentToNewsLetter UserConsentInfo
  deriving (Generic)

data UserDataPermissionInfo = InactiveUser | Personal WebAppUser | Team WebAppUser
  deriving (Generic)

data ConsentTotals = ConsentTotals
  { usersTotal :: Int,
    consentToCrashReports :: Int,
    consentToUsageData :: Int,
    consentToNewsLetter :: Int
  }
  deriving (Generic, Show)

instance Semigroup ConsentTotals where
  ConsentTotals t1 c1 u1 n1 <> ConsentTotals t2 c2 u2 n2 =
    ConsentTotals (t1 + t2) (c1 + c2) (u1 + u2) (n1 + n2)

instance Monoid ConsentTotals where
  mempty = ConsentTotals 0 0 0 0

data ConsentResult = ConsentResult
  { entriesSearched :: Int,
    personalUsersTotal :: Int,
    personalUsersConsent :: ConsentTotals,
    teamUsersTotal :: Int,
    teamUsersConsent :: ConsentTotals
  }
  deriving (Generic, Show)

instance Semigroup ConsentResult where
  ConsentResult e1 p1 pc1 t1 tc1 <> ConsentResult e2 p2 pc2 t2 tc2 =
    ConsentResult
      (e1 + e2)
      (p1 + p2)
      (pc1 <> pc2)
      (t1 + t2)
      (tc1 <> tc2)

instance Monoid ConsentResult where
  mempty = ConsentResult 0 0 mempty 0 mempty

userToConsentResult :: UserDataPermissionInfo -> ConsentResult
userToConsentResult = \case
  InactiveUser -> mempty {entriesSearched = 1}
  Personal (NoWebUser (ConsentToNewsLetter nlConsent)) ->
    mempty
      { entriesSearched = 1,
        personalUsersTotal = 1,
        personalUsersConsent =
          ConsentTotals
            { usersTotal = 1,
              consentToCrashReports = 0,
              consentToUsageData = 0,
              consentToNewsLetter = if nlConsent then 1 else 0
            }
      }
  Personal (WebAppUser (ConsentToNewsLetter nlConsent) consents) ->
    ConsentResult
      { entriesSearched = 1,
        personalUsersTotal = 1,
        personalUsersConsent =
          ConsentTotals
            { usersTotal = 1,
              consentToCrashReports = if consents.consentToCrashReports then 1 else 0,
              consentToUsageData = if consents.consentToUsageData then 1 else 0,
              consentToNewsLetter = if nlConsent then 1 else 0
            },
        teamUsersTotal = 0,
        teamUsersConsent = mempty
      }
  Team (NoWebUser (ConsentToNewsLetter nlConsent)) ->
    mempty
      { entriesSearched = 1,
        teamUsersTotal = 1,
        teamUsersConsent =
          ConsentTotals
            { usersTotal = 1,
              consentToCrashReports = 0,
              consentToUsageData = 0,
              consentToNewsLetter = if nlConsent then 1 else 0
            }
      }
  Team (WebAppUser (ConsentToNewsLetter nlConsent) consents) ->
    ConsentResult
      { entriesSearched = 1,
        personalUsersTotal = 0,
        personalUsersConsent = mempty,
        teamUsersTotal = 1,
        teamUsersConsent =
          ConsentTotals
            { usersTotal = 1,
              consentToCrashReports = if consents.consentToCrashReports then 1 else 0,
              consentToUsageData = if consents.consentToUsageData then 1 else 0,
              consentToNewsLetter = if nlConsent then 1 else 0
            }
      }

data Privacy = Privacy
  { improveWire :: Bool,
    telemetrySharing :: Bool
  }
  deriving Generic

instance A.FromJSON Privacy where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}

data Settings = Settings
  { privacy :: Privacy
  }
  deriving (Generic)

instance A.FromJSON Settings where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}

data WebApp = WebApp
  { settings :: Settings
  }
  deriving (Generic)

instance A.FromJSON WebApp where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}
