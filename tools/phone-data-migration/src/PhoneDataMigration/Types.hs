{-# OPTIONS_GHC -Wwarn #-}
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
import Control.Lens
import Data.Id
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import Wire.API.User (Email)
import qualified Data.Text as Text
import Data.Attoparsec.Text (parseOnly, char, digit, endOfInput, count)
import qualified System.Logger as Log
import qualified Cassandra.Settings as C
import Control.Monad.Catch (MonadThrow)
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
  { total :: Int,
    phoneIdentity :: Int,
    emailIdentity :: Int,
    fullIdentity :: Int,
    ssoIdentity :: Int,
    ssoIdentityPhone :: Int,
    ssoIdentityEmail :: Int,
    ssoIdentityFull :: Int,
    noIdentity :: Int
  }
 deriving (Show)

instance Semigroup Result where
  (Result t1 p1 e1 f1 s1 sp1 se1 sf1 ni1) <> (Result t2 p2 e2 f2 s2 sp2 se2 sf2 ni2) =
    Result (t1 + t2) (p1 + p2) (e1 + e2) (f1 + f2) (s1 + s2) (sp1 + sp2) (se1 + se2) (sf1 + sf2) (ni1 + ni2)


instance Monoid Result where
  mempty = Result 0 0 0 0 0 0 0 0 0



data User = User
  { id :: UserId,
    phone :: Maybe Phone,
    email :: Maybe Email,
    hasSsoId :: Bool
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
