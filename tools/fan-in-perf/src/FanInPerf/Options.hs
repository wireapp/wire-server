module FanInPerf.Options
  ( Isolation (..),
    GlobalOptions (..),
    ProduceOptions (..),
    Command (..),
    Options (..),
    optionsInfo,
  )
where

import Data.Domain
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import FanInPerf.Targets
import Imports
import Options.Applicative hiding (command)
import Options.Applicative qualified as O

data Isolation = ReadCommitted | Serializable
  deriving (Eq, Show)

-- | No 'Show' instance: 'db' contains the database password.
data GlobalOptions = GlobalOptions
  { db :: Text,
    poolSize :: Maybe Int,
    metricsPort :: Int,
    domain :: Domain,
    isolation :: Isolation
  }

data ProduceOptions = ProduceOptions
  { writers :: Int,
    targets :: NonEmpty TargetEntry,
    clientsPerUser :: Int,
    payloadBytes :: Int,
    duration :: Maybe Int,
    warmup :: Int
  }
  deriving (Eq, Show)

data Command = Reset | Produce ProduceOptions
  deriving (Eq, Show)

data Options = Options
  { global :: GlobalOptions,
    command :: Command
  }

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    (fullDesc <> progDesc "Benchmark the notification fan-in PostgreSQL store (WPB-26288)")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> globalParser
    <*> hsubparser
      ( O.command "reset" (info (pure Reset) (progDesc "Truncate all fan-in notification tables"))
          <> O.command "produce" (info (Produce <$> produceParser) (progDesc "Experiment A: maximal rate of adding notifications"))
      )

globalParser :: Parser GlobalOptions
globalParser =
  GlobalOptions
    <$> strOption (long "db" <> metavar "CONNSTR" <> help "PostgreSQL connection string")
    <*> optional (option positive (long "pool-size" <> metavar "N" <> help "Connection pool size (default: --writers for produce, 1 for reset)"))
    <*> option port (long "metrics-port" <> metavar "PORT" <> value 9400 <> showDefault <> help "Port of the /metrics endpoint")
    <*> option domainReader (long "domain" <> metavar "DOMAIN" <> value (Domain "example.com") <> showDefaultWith (T.unpack . domainText) <> help "Local backend domain")
    <*> option isolationReader (long "isolation" <> metavar "read-committed|serializable" <> value ReadCommitted <> showDefaultWith (const "read-committed") <> help "Isolation level of push transactions")

produceParser :: Parser ProduceOptions
produceParser =
  ProduceOptions
    <$> option positive (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> option targetsReader (long "targets" <> metavar "SPEC" <> help "Target mix KIND:STREAMS[xK],... e.g. user:1000x20,team:10")
    <*> option positive (long "clients-per-user" <> metavar "C" <> value 1 <> showDefault <> help "Client ids per 'clients' target")
    <*> option positive (long "payload-bytes" <> metavar "B" <> value 512 <> showDefault <> help "Approximate JSON payload size")
    <*> optional (option positive (long "duration" <> metavar "SECS" <> help "Run length (default: until Ctrl-C)"))
    <*> option nonNegative (long "warmup" <> metavar "SECS" <> value 5 <> showDefault <> help "Seconds ignored for max-rate tracking")

positive :: ReadM Int
positive = eitherReader $ \s -> case readMaybe s of
  Just n | n > 0 -> Right n
  _ -> Left "expected a positive integer"

nonNegative :: ReadM Int
nonNegative = eitherReader $ \s -> case readMaybe s of
  Just n | n >= 0 -> Right n
  _ -> Left "expected a non-negative integer"

port :: ReadM Int
port = eitherReader $ \s -> case readMaybe s of
  Just n | n > 0 && n <= 65535 -> Right n
  _ -> Left "expected a port between 1 and 65535"

domainReader :: ReadM Domain
domainReader = eitherReader (mkDomain . T.pack)

isolationReader :: ReadM Isolation
isolationReader = eitherReader $ \case
  "read-committed" -> Right ReadCommitted
  "serializable" -> Right Serializable
  _ -> Left "expected read-committed or serializable"

targetsReader :: ReadM (NonEmpty TargetEntry)
targetsReader = eitherReader (parseTargetSpec . T.pack)
