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

module PhoneDataMigration.Types where

import Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Data.Attoparsec.Text (char, count, digit, endOfInput, parseOnly)
import Data.Id
import Data.Monoid.Generic
import qualified Data.Text as Text
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger, log)
import Wire.API.User (AccountStatus, Email)

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
        . Log.setLogLevel Log.Info
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

data Result = Result
  { total :: IntSum,
    inactive :: IntSum,
    phoneIdentity :: IntSum,
    emailIdentity :: IntSum,
    fullIdentity :: IntSum,
    ssoIdentity :: IntSum,
    ssoIdentityPhone :: IntSum,
    ssoIdentityEmail :: IntSum,
    ssoIdentityFull :: IntSum,
    noIdentity :: IntSum
  }
  deriving (Show, Generic)
  deriving (Semigroup) via GenericSemigroup Result
  deriving (Monoid) via GenericMonoid Result

data User = User
  { id :: UserId,
    phone :: Maybe Phone,
    email :: Maybe Email,
    hasSsoId :: Bool,
    activated :: Bool,
    status :: Maybe AccountStatus
  }
  deriving (Generic)

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p =
  let canonicalPhone = Text.filter (not . isSpace) p
   in if isValidPhone canonicalPhone
        then Just $ Phone canonicalPhone
        else Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput
