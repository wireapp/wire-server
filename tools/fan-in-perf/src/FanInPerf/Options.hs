module FanInPerf.Options
  ( GlobalOptions (..),
    ProduceOptions (..),
    ConsumeOptions (..),
    ConsumeOnlyOptions (..),
    PopulationOptions (..),
    ReaderOptions (..),
    readerOptions,
    ResetOptions (..),
    ReaderMode (..),
    experimentLabel,
    consumeOnlyLabel,
    Command (..),
    Options (..),
    optionsInfo,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import FanInPerf.NotificationsAdmin (SchemaShape (..))
import FanInPerf.TargetSpecParser (parseTargetSpec)
import FanInPerf.Targets
import Hasql.Transaction.Sessions (IsolationLevel (..))
import Imports
import Options.Applicative hiding (command)
import Options.Applicative qualified as O
import Options.Applicative.Help (Doc, pretty, vsep)

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
    population :: PopulationOptions,
    pollIntervalMs :: Int,
    fetchLimit :: Int,
    readerPoolSize :: Int,
    drainTimeout :: Int,
    safetyWindowMs :: Int,
    mode :: ReaderMode
  }
  deriving (Eq, Show)

-- | Experiments E and F: readers only, on rows left by an earlier @produce@
-- run. The seed, targets and clients per user must be those of that run.
data ConsumeOnlyOptions = ConsumeOnlyOptions
  { targets :: NonEmpty TargetConfig,
    clientsPerUser :: Int,
    seed :: Int,
    population :: PopulationOptions,
    readerPoolSize :: Int,
    -- | seconds, 0: none
    timeout :: Int,
    warmup :: Int,
    reader :: ReaderOptions
  }
  deriving (Eq, Show)

-- | Who reads, shared by 'ConsumeOptions' and 'ConsumeOnlyOptions' (see
-- 'FanInPerf.Population.mkPopulation').
data PopulationOptions = PopulationOptions
  { onlineUsers :: Maybe Int,
    epochsPerUser :: Int
  }
  deriving (Eq, Show)

-- | What one reader client needs to know when it fetches.
data ReaderOptions = ReaderOptions
  { fetchLimit :: Int,
    safetyWindowMs :: Int,
    mode :: ReaderMode
  }
  deriving (Eq, Show)

readerOptions :: ConsumeOptions -> ReaderOptions
readerOptions c = ReaderOptions {fetchLimit = c.fetchLimit, safetyWindowMs = c.safetyWindowMs, mode = c.mode}

-- | Whether reader clients acknowledge what they received (experiment C) or
-- only keep cursors in memory (B). @Acks n@: ack every n-th notification.
data ReaderMode = NoAcks | Acks Int
  deriving (Eq, Show)

-- | Value of the @experiment@ metric label and prefix of terminal lines.
experimentLabel :: ReaderMode -> Text
experimentLabel = \case
  NoAcks -> "consume"
  Acks _ -> "consume-ack"

-- | Value of the @experiment@ metric label and prefix of terminal lines of
-- 'ConsumeOnly'.
consumeOnlyLabel :: ReaderMode -> Text
consumeOnlyLabel = \case
  NoAcks -> "consume-only"
  Acks _ -> "consume-only-ack"

data ResetOptions = ResetOptions
  { shape :: SchemaShape,
    -- | wait for pg_partman's first maintenance run ('Partitioned' only)
    settle :: Bool
  }
  deriving (Eq, Show)

data Command
  = ResetDb ResetOptions
  | Vacuum
  | DbInfo
  | Produce ProduceOptions
  | Consume ConsumeOptions
  | ConsumeOnly ConsumeOnlyOptions
  deriving (Eq, Show)

data Options = Options
  { global :: GlobalOptions,
    command :: Command,
    -- | file for the summary of a run as JSON; only the load commands take it
    summaryJson :: Maybe FilePath
  }

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    (fullDesc <> progDesc "Benchmark the notification fan-in PostgreSQL store (WPB-26288)")

optionsParser :: Parser Options
optionsParser =
  (\global (command, summaryJson) -> Options {..})
    <$> globalParser
    <*> hsubparser
      ( O.command "reset-db" (info (noSummary $ ResetDb <$> resetParser) (progDesc "Create the experiment database if missing, drop the fan-in notification tables and pg_partman if present, and create the tables from scratch (with --partitioned: partitioned, configures pg_partman and waits until its first maintenance run is done)"))
          <> O.command "vacuum" (info (noSummary $ pure Vacuum) (progDesc "VACUUM ANALYZE the experiment database, e.g. between 'produce' and 'consume-only'"))
          <> O.command "db-info" (info (noSummary $ pure DbInfo) (progDesc "Print the server version, the settings that matter for the numbers and the pg_partman version as JSON"))
          <> O.command "produce" (info (withSummary $ Produce <$> produceParser) (progDesc "Experiment A: maximal rate of adding notifications"))
          <> O.command "consume" (info (withSummary $ Consume <$> consumeParserWith (pure NoAcks)) (progDesc "Experiment B: maximal rate of adding and consuming notifications by simulated clients"))
          <> O.command "consume-ack" (info (withSummary $ Consume <$> consumeParserWith ackModeParser) (progDesc "Experiment C: like consume, but every client acknowledges the notifications it received"))
          <> O.command "consume-only" (info (withSummary $ ConsumeOnly <$> consumeOnlyParserWith (pure NoAcks)) (progDesc "Experiment E: consume only, what an earlier 'produce' run (same --seed, --targets, --clients-per-user) left in the database"))
          <> O.command "consume-only-ack" (info (withSummary $ ConsumeOnly <$> consumeOnlyParserWith ackModeParser) (progDesc "Experiment F: like consume-only, but every client acknowledges the notifications it received"))
      )

-- | Only the commands that run a load take @--summary-json@: for the others
-- the parser rejects it.
withSummary, noSummary :: Parser Command -> Parser (Command, Maybe FilePath)
withSummary p = (,) <$> p <*> optional (strOption (long "summary-json" <> metavar "FILE" <> help "Write the summary of the run to FILE as JSON"))
noSummary p = (,Nothing) <$> p

globalParser :: Parser GlobalOptions
globalParser =
  GlobalOptions
    <$> strOption (long "db" <> metavar "CONNSTR" <> help "PostgreSQL connection string")
    <*> optional (option (positive maxPoolSize) (long "pool-size" <> metavar "N" <> help "Connection pool size (default: --writers for produce, 1 for reset-db)"))
    <*> option port (long "metrics-port" <> metavar "PORT" <> value 9400 <> showDefault <> help "Port of the /metrics endpoint")
    <*> option isolationReader (long "isolation" <> metavar "read-committed|repeatable-read|serializable" <> value ReadCommitted <> showDefaultWith (const "read-committed") <> help "Isolation level of push transactions")

produceParser :: Parser ProduceOptions
produceParser = produceParserWith targetsReader

produceParserWith :: ReadM (NonEmpty TargetConfig) -> Parser ProduceOptions
produceParserWith targetsRdr =
  ProduceOptions
    <$> option (positive maxWriters) (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> targetsOption targetsRdr
    <*> clientsPerUserOption
    <*> option (positive maxPayloadBytes) (long "payload-bytes" <> metavar "B" <> value 512 <> showDefault <> help "Approximate JSON payload size")
    <*> optional (option (positive maxDuration) (long "duration" <> metavar "SECS" <> help "Run length (default: until Ctrl-C)"))
    <*> warmupOption
    <*> optional (option anyInt (long "seed" <> metavar "INT" <> help "RNG seed (default: random, printed at start)"))

targetsOption :: ReadM (NonEmpty TargetConfig) -> Parser (NonEmpty TargetConfig)
targetsOption targetsRdr = option targetsRdr (long "targets" <> metavar "SPEC" <> helpDoc (Just targetsHelp))

clientsPerUserOption :: Parser Int
clientsPerUserOption = option (positive maxClientsPerUser) (long "clients-per-user" <> metavar "C" <> value 1 <> showDefault <> help "Client ids (devices) per user (a 'clients' push writes K x C rows)")

warmupOption :: Parser Int
warmupOption = option (nonNegative maxDuration) (long "warmup" <> metavar "SECS" <> value 5 <> showDefault <> help "Seconds ignored for max-rate tracking")

populationParser :: Parser PopulationOptions
populationParser =
  PopulationOptions
    <$> optional (option (positive (toInteger (maxBound @Int))) (long "online-users" <> metavar "U" <> help "Users that are online, i.e. have readers (readers = U x --clients-per-user, must stay within the reader limit). Default: all users of the 'user'/'clients' pool, capped to the reader limit; required if --targets has no such entry"))
    <*> option (positive maxEpochsPerUser) (long "epochs-per-user" <> metavar "E" <> value 3 <> showDefault <> help "Epoch streams per user, shared by its clients (needs an 'epoch' entry in --targets)")

pollIntervalOption :: Parser Int
pollIntervalOption = option (nonNegative maxPollIntervalMs) (long "poll-interval" <> metavar "MS" <> value 100 <> showDefault <> help "Sleep in milliseconds after a fetch pass without a full page")

fetchLimitOption :: Parser Int
fetchLimitOption = option (positive maxFetchLimit) (long "fetch-limit" <> metavar "L" <> value 100 <> showDefault <> help "Notifications per fetch")

readerPoolSizeOption :: Parser Int
readerPoolSizeOption = option (positive maxPoolSize) (long "reader-pool-size" <> metavar "P" <> value 32 <> showDefault <> help "Connection pool size of the readers (writers: --pool-size)")

consumeParserWith :: Parser ReaderMode -> Parser ConsumeOptions
consumeParserWith modeParser =
  ConsumeOptions
    <$> produceParserWith consumeTargetsReader
    <*> populationParser
    <*> pollIntervalOption
    <*> fetchLimitOption
    <*> readerPoolSizeOption
    <*> option (nonNegative maxDuration) (long "drain-timeout" <> metavar "SECS" <> value 10 <> showDefault <> help "Seconds to wait for readers to catch up after the writers stopped")
    <*> option (nonNegative maxSafetyWindowMs) (long "safety-window" <> metavar "MS" <> value 0 <> showDefault <> help "Readers re-fetch the last MS milliseconds (by UUIDv7 timestamp) of each stream to catch late commits; 0: plain > cursor")
    <*> modeParser

-- | No safety window (nothing is written while the readers run, so there are no
-- late commits) and no poll interval (every reader catches up once and stops).
consumeOnlyParserWith :: Parser ReaderMode -> Parser ConsumeOnlyOptions
consumeOnlyParserWith modeParser =
  mk
    <$> targetsOption consumeTargetsReader
    <*> clientsPerUserOption
    <*> option anyInt (long "seed" <> metavar "INT" <> help "Seed of the 'produce' run (required: rebuilds its stream keys)")
    <*> populationParser
    <*> readerPoolSizeOption
    <*> option (nonNegative maxDuration) (long "timeout" <> metavar "SECS" <> value 600 <> showDefault <> help "Stop the run after SECS seconds, readers not yet caught up count as missing (0: no timeout)")
    <*> warmupOption
    <*> fetchLimitOption
    <*> modeParser
  where
    mk targets clientsPerUser seed population readerPoolSize timeout warmup fetchLimit mode =
      ConsumeOnlyOptions {reader = ReaderOptions {safetyWindowMs = 0, ..}, ..}

ackModeParser :: Parser ReaderMode
ackModeParser =
  Acks <$> option (positive maxAckEvery) (long "ack-every" <> metavar "N" <> value 100 <> showDefault <> help "A client acknowledges after every N-th notification it received (counted over all its streams)")

maxAckEvery :: Integer
maxAckEvery = 10_000

-- | Readers have no 'connections' streams (see the design spec's follow-ups).
consumeTargetsReader :: ReadM (NonEmpty TargetConfig)
consumeTargetsReader = do
  ts <- targetsReader
  when (any ((== KindConnections) . (.kind)) ts) $
    readerError "the 'connections' kind is not supported by 'consume'"
  -- a reader fetches and acks as one (user, client): its user and client
  -- streams must come from the same users (see "FanInPerf.Population")
  let streamsOf k = [t.streams | t <- toList ts, t.kind == k]
  case (streamsOf KindUser, streamsOf KindClients) of
    ([u], [c]) | u /= c -> readerError "'consume' needs the same STREAMS for 'user' and 'clients' (one identity per reader)"
    _ -> pure ts

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

maxEpochsPerUser, maxPollIntervalMs, maxFetchLimit, maxSafetyWindowMs :: Integer
maxEpochsPerUser = 10_000
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

resetParser :: Parser ResetOptions
resetParser =
  ResetOptions
    <$> flag Plain Partitioned (long "partitioned" <> help "Use pg_partman partitioned notification tables (experiment D) instead of the plain ones")
    <*> (not <$> switch (long "no-settle" <> help "With --partitioned: do not wait for pg_partman's first maintenance run (it drops the premade past partitions in bulk, which would disturb a run started right away)"))
