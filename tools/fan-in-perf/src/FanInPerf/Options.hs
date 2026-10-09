module FanInPerf.Options
  ( GlobalOptions (..),
    ProduceOptions (..),
    Command (..),
    Options (..),
    optionsInfo,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import FanInPerf.Targets
import Hasql.Transaction.Sessions (IsolationLevel (..))
import Imports
import Options.Applicative hiding (command)
import Options.Applicative qualified as O

-- | No 'Show' instance: 'db' contains the database password.
data GlobalOptions = GlobalOptions
  { db :: Text,
    poolSize :: Maybe Int,
    metricsPort :: Int,
    isolation :: IsolationLevel
  }

data ProduceOptions = ProduceOptions
  { writers :: Int,
    targets :: NonEmpty TargetEntry,
    clientsPerUser :: Int,
    payloadBytes :: Int,
    duration :: Maybe Int,
    warmup :: Int,
    seed :: Maybe Int
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
    <*> optional (option (positive maxPoolSize) (long "pool-size" <> metavar "N" <> help "Connection pool size (default: --writers for produce, 1 for reset)"))
    <*> option port (long "metrics-port" <> metavar "PORT" <> value 9400 <> showDefault <> help "Port of the /metrics endpoint")
    <*> option isolationReader (long "isolation" <> metavar "read-committed|repeatable-read|serializable" <> value ReadCommitted <> showDefaultWith (const "read-committed") <> help "Isolation level of push transactions")

produceParser :: Parser ProduceOptions
produceParser =
  ProduceOptions
    <$> option (positive maxWriters) (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> option targetsReader (long "targets" <> metavar "SPEC" <> help "Target mix KIND:STREAMS[xK],... e.g. user:1000x20,team:10")
    <*> option (positive maxClientsPerUser) (long "clients-per-user" <> metavar "C" <> value 1 <> showDefault <> help "Client ids per 'clients' target")
    <*> option (positive maxPayloadBytes) (long "payload-bytes" <> metavar "B" <> value 512 <> showDefault <> help "Approximate JSON payload size")
    <*> optional (option (positive maxDuration) (long "duration" <> metavar "SECS" <> help "Run length (default: until Ctrl-C)"))
    <*> option (nonNegative maxDuration) (long "warmup" <> metavar "SECS" <> value 5 <> showDefault <> help "Seconds ignored for max-rate tracking")
    <*> optional (option anyInt (long "seed" <> metavar "INT" <> help "RNG seed (default: random, printed at start)"))

-- | Parsed as 'Integer' first so huge inputs cannot wrap around 'Int'.
bounded :: Integer -> Integer -> String -> ReadM Int
bounded lo hi what = eitherReader $ \s -> case readMaybe @Integer s of
  Just n | n >= lo && n <= hi -> Right (fromInteger n)
  _ -> Left ("expected " <> what <> " between " <> show lo <> " and " <> show hi)

maxPoolSize, maxWriters, maxClientsPerUser, maxPayloadBytes, maxDuration :: Integer
maxPoolSize = 10_000
maxWriters = 10_000
maxClientsPerUser = 100_000
maxPayloadBytes = 10_000_000

-- | Seconds; ten years, so @d * 1_000_000@ cannot overflow 'Int'.
maxDuration = 315_360_000

positive :: Integer -> ReadM Int
positive hi = bounded 1 hi "an integer"

nonNegative :: Integer -> ReadM Int
nonNegative hi = bounded 0 hi "an integer"

anyInt :: ReadM Int
anyInt = bounded (toInteger (minBound @Int)) (toInteger (maxBound @Int)) "an integer"

port :: ReadM Int
port = bounded 1 65535 "a port"

isolationReader :: ReadM IsolationLevel
isolationReader = eitherReader $ \case
  "read-committed" -> Right ReadCommitted
  "repeatable-read" -> Right RepeatableRead
  "serializable" -> Right Serializable
  _ -> Left "expected read-committed, repeatable-read or serializable"

targetsReader :: ReadM (NonEmpty TargetEntry)
targetsReader = eitherReader (parseTargetSpec . T.pack)
