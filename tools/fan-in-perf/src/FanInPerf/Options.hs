module FanInPerf.Options
  ( GlobalOptions (..),
    ProduceOptions (..),
    ConsumeOptions (..),
    ReaderMode (..),
    experimentLabel,
    Command (..),
    Options (..),
    optionsInfo,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import FanInPerf.TargetSpecParser (parseTargetSpec)
import FanInPerf.Targets
import Hasql.Transaction.Sessions (IsolationLevel (..))
import Imports
import Options.Applicative hiding (command)
import Options.Applicative qualified as O
import Options.Applicative.Help (Doc, pretty, vsep)
import Wire.FanInNotificationsAdmin (SchemaShape (..))

-- | No 'Show' instance: 'db' contains the database password.
data GlobalOptions = GlobalOptions
  { db :: Text,
    poolSize :: Maybe Int,
    metricsPort :: Int,
    isolation :: IsolationLevel
  }

data ProduceOptions = ProduceOptions
  { writers :: Int,
    targets :: NonEmpty TargetConfig,
    clientsPerUser :: Int,
    payloadBytes :: Int,
    duration :: Maybe Int,
    warmup :: Int,
    seed :: Maybe Int
  }
  deriving (Eq, Show)

data ConsumeOptions = ConsumeOptions
  { writerOptions :: ProduceOptions,
    clients :: Int,
    epochsPerClient :: Int,
    pollIntervalMs :: Int,
    fetchLimit :: Int,
    readerPoolSize :: Int,
    drainTimeout :: Int,
    safetyWindowMs :: Int,
    mode :: ReaderMode
  }
  deriving (Eq, Show)

-- | Whether reader clients acknowledge what they received (experiment C) or
-- only keep cursors in memory (B). @Acks n@: ack every n-th notification.
data ReaderMode = NoAcks | Acks Int
  deriving (Eq, Show)

-- | Value of the @experiment@ metric label and prefix of terminal lines.
experimentLabel :: ReaderMode -> Text
experimentLabel = \case
  NoAcks -> "consume"
  Acks _ -> "consume-ack"

data Command = Migrate SchemaShape | DropDb | Reset | Produce ProduceOptions | Consume ConsumeOptions
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
      ( O.command "migrate" (info (Migrate <$> shapeParser) (progDesc "Create the experiment database if missing, with --partitioned configure pg_partman, and create the fan-in notification tables (idempotent)"))
          <> O.command "drop-db" (info (pure DropDb) (progDesc "Drop the experiment database"))
          <> O.command "reset" (info (pure Reset) (progDesc "Truncate all fan-in notification tables"))
          <> O.command "produce" (info (Produce <$> produceParser) (progDesc "Experiment A: maximal rate of adding notifications"))
          <> O.command "consume" (info (Consume <$> consumeParserWith consumeTargetsReader (pure NoAcks)) (progDesc "Experiment B: maximal rate of adding and consuming notifications by simulated clients"))
          <> O.command "consume-ack" (info (Consume <$> consumeParserWith consumeAckTargetsReader ackModeParser) (progDesc "Experiment C: like consume, but every client acknowledges the notifications it received"))
      )

globalParser :: Parser GlobalOptions
globalParser =
  GlobalOptions
    <$> strOption (long "db" <> metavar "CONNSTR" <> help "PostgreSQL connection string")
    <*> optional (option (positive maxPoolSize) (long "pool-size" <> metavar "N" <> help "Connection pool size (default: --writers for produce, 1 for reset)"))
    <*> option port (long "metrics-port" <> metavar "PORT" <> value 9400 <> showDefault <> help "Port of the /metrics endpoint")
    <*> option isolationReader (long "isolation" <> metavar "read-committed|repeatable-read|serializable" <> value ReadCommitted <> showDefaultWith (const "read-committed") <> help "Isolation level of push transactions")

produceParser :: Parser ProduceOptions
produceParser = produceParserWith targetsReader

produceParserWith :: ReadM (NonEmpty TargetConfig) -> Parser ProduceOptions
produceParserWith targetsRdr =
  ProduceOptions
    <$> option (positive maxWriters) (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> option targetsRdr (long "targets" <> metavar "SPEC" <> helpDoc (Just targetsHelp))
    <*> option (positive maxClientsPerUser) (long "clients-per-user" <> metavar "C" <> value 1 <> showDefault <> help "Client ids per 'clients' target")
    <*> option (positive maxPayloadBytes) (long "payload-bytes" <> metavar "B" <> value 512 <> showDefault <> help "Approximate JSON payload size")
    <*> optional (option (positive maxDuration) (long "duration" <> metavar "SECS" <> help "Run length (default: until Ctrl-C)"))
    <*> option (nonNegative maxDuration) (long "warmup" <> metavar "SECS" <> value 5 <> showDefault <> help "Seconds ignored for max-rate tracking")
    <*> optional (option anyInt (long "seed" <> metavar "INT" <> help "RNG seed (default: random, printed at start)"))

consumeParserWith :: ReadM (NonEmpty TargetConfig) -> Parser ReaderMode -> Parser ConsumeOptions
consumeParserWith targetsRdr modeParser =
  ConsumeOptions
    <$> produceParserWith targetsRdr
    <*> option (positive maxClients) (long "clients" <> metavar "N" <> value 2000 <> showDefault <> help "Simulated reader clients")
    <*> option (positive maxEpochsPerClient) (long "epochs-per-client" <> metavar "E" <> value 3 <> showDefault <> help "Epoch streams per client (needs an 'epoch' entry in --targets)")
    <*> option (nonNegative maxPollIntervalMs) (long "poll-interval" <> metavar "MS" <> value 100 <> showDefault <> help "Sleep in milliseconds after a fetch pass without a full page")
    <*> option (positive maxFetchLimit) (long "fetch-limit" <> metavar "L" <> value 100 <> showDefault <> help "Notifications per fetch")
    <*> option (positive maxPoolSize) (long "reader-pool-size" <> metavar "P" <> value 32 <> showDefault <> help "Connection pool size of the readers (writers: --pool-size)")
    <*> option (nonNegative maxDuration) (long "drain-timeout" <> metavar "SECS" <> value 10 <> showDefault <> help "Seconds to wait for readers to catch up after the writers stopped")
    <*> option (nonNegative maxSafetyWindowMs) (long "safety-window" <> metavar "MS" <> value 0 <> showDefault <> help "Readers re-fetch the last MS milliseconds (by UUIDv7 timestamp) of each stream to catch late commits; 0: plain > cursor")
    <*> modeParser

ackModeParser :: Parser ReaderMode
ackModeParser =
  Acks <$> option (positive maxAckEvery) (long "ack-every" <> metavar "N" <> value 100 <> showDefault <> help "A client acknowledges after every N-th notification it received (counted over all its streams)")

-- | A reader fetches and acks as one (user, client): its user and client
-- streams must come from the same users (see "FanInPerf.Population").
consumeAckTargetsReader :: ReadM (NonEmpty TargetConfig)
consumeAckTargetsReader = do
  ts <- consumeTargetsReader
  let streamsOf k = [t.streams | t <- toList ts, t.kind == k]
  case (streamsOf KindUser, streamsOf KindClients) of
    ([u], [c]) | u /= c -> readerError "'consume-ack' needs the same STREAMS for 'user' and 'clients' (one identity per reader)"
    _ -> pure ts

maxAckEvery :: Integer
maxAckEvery = 10_000

-- | Readers have no 'connections' streams (see the design spec's follow-ups).
consumeTargetsReader :: ReadM (NonEmpty TargetConfig)
consumeTargetsReader = do
  ts <- targetsReader
  when (any ((== KindConnections) . (.kind)) ts) $
    readerError "the 'connections' kind is not supported by 'consume'"
  pure ts

targetsHelp :: Doc
targetsHelp =
  vsep $
    map
      (pretty @String)
      [ "Target mix: comma-separated entries KIND:STREAMS[xK].",
        "KIND: user|clients|team|epoch|connections (each at most once).",
        "STREAMS: distinct stream keys pre-generated for that kind.",
        "K: targets of that kind per push (default 1, at most STREAMS).",
        "Each push picks one entry uniformly, then K distinct keys of it;",
        "a push never mixes kinds. For 'clients', K counts users",
        "(rows written = K x --clients-per-user).",
        "Example: user:1000x20,team:10",
        "  user: pool of 1000 keys, 20 per push",
        "  team: pool of 10 keys, 1 per push"
      ]

-- | Parsed as 'Integer' first so huge inputs cannot wrap around 'Int'.
bounded :: Integer -> Integer -> String -> ReadM Int
bounded lo hi what = eitherReader $ \s -> case readMaybe @Integer s of
  Just n | n >= lo && n <= hi -> Right (fromInteger n)
  _ -> Left ("expected " <> what <> " between " <> show lo <> " and " <> show hi)

maxClients, maxEpochsPerClient, maxPollIntervalMs, maxFetchLimit, maxSafetyWindowMs :: Integer
maxClients = 100_000
maxEpochsPerClient = 10_000
maxPollIntervalMs = 3_600_000
maxFetchLimit = 10_000
maxSafetyWindowMs = 60_000

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

targetsReader :: ReadM (NonEmpty TargetConfig)
targetsReader = eitherReader (parseTargetSpec . T.pack)

shapeParser :: Parser SchemaShape
shapeParser =
  flag Plain Partitioned (long "partitioned" <> help "Use pg_partman partitioned notification tables (experiment D) instead of the plain ones")
