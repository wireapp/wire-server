{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Cassandra.Schema
    ( Migration           (..)
    , MigrationOpts       (..)
    , ReplicationStrategy (..)
    , ReplicationFactor   (..)
    , ReplicationMap      (..)
    , schemaVersion
    , versionCheck
    , createKeyspace
    , useKeyspace
    , migrateSchema
    , migrationOptsParser
    , schema'
    ) where

import Cassandra
import Cassandra.Settings
import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import Data.Aeson
import Data.Int
import Data.IORef
import Data.Functor.Identity
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Builder (fromText, fromString, toLazyText)
import Data.Time.Clock
import Data.UUID (UUID)
import Data.Word
import Database.CQL.IO
import Database.CQL.Protocol (Request (..), Query (..), Response(..), Result (..))
import GHC.Generics hiding (to, from, S, R)
import Options.Applicative hiding (info)
import Prelude hiding (log)
import System.Logger (Logger, Level (..), log, msg)

import qualified Data.Text.Lazy as LT
import qualified Data.List.NonEmpty as NonEmpty

data Migration = Migration
    { migVersion :: Int32
    , migText    :: Text
    , migAction  :: Client ()
    }

data MigrationOpts = MigrationOpts
    { migHost     :: String
    , migPort     :: Word16
    , migKeyspace :: Text
    , migRepl     :: ReplicationStrategy
    , migReset    :: Bool
    } deriving (Eq, Show, Generic)

data ReplicationStrategy
    = SimpleStrategy { replicationFactor :: ReplicationFactor }
    | NetworkTopologyStrategy { dataCenters :: ReplicationMap }
    deriving (Eq, Show, Generic)

newtype ReplicationFactor = ReplicationFactor Word16
    deriving (Eq, Show, Generic)

newtype ReplicationMap = ReplicationMap [(Text, ReplicationFactor)]
    deriving (Eq, Show, Generic)

instance FromJSON ReplicationMap
instance FromJSON ReplicationFactor
instance FromJSON ReplicationStrategy
instance FromJSON MigrationOpts

instance Read ReplicationMap where
    -- ReplicationMap ::= DataCenter [("," DataCenter)*]
    -- DataCenter     ::= Name ":" ReplFactor
    -- Name           ::= Text
    -- ReplFactor     ::= Word16
    readsPrec _ s = [(ReplicationMap (dcMap s), "")]
      where
        dcMap     = map dcEntry . splitOn ","
        dcEntry e = case splitOn ":" e of
            [k,v] -> (pack k, ReplicationFactor (read v))
            _     -> error $ "Failed reading: Invalid data center entry: " ++ e

schema' :: LT.Text -> Client ()
schema' q = void $ schema (QueryString q) (params All ())

schemaVersion :: Client (Maybe Int32)
schemaVersion = catch (fmap runIdentity <$> qry) h
  where
    qry = retry x5 $ query1 q (params One ())
    q = QueryString "select version from meta where id=1 order by version desc limit 1"

    h :: SomeException -> a
    h e = error $ "Failed to read schema version from meta table. Error was: "
                <> show e

versionCheck :: Int32 -> Client ()
versionCheck v = do
    v' <- schemaVersion
    unless (Just v <= v') $
        error $ "Schema Version too old! Expecting at least: "
              <> show v
              <> ", but got: "
              <> fromMaybe "" (show <$> v')

createKeyspace :: Keyspace -> ReplicationStrategy -> Client ()
createKeyspace (Keyspace k) rs = void $ schema (cql rs) (params All ())
  where
    cql (SimpleStrategy (ReplicationFactor n)) = QueryString . toLazyText $
           fromText "create keyspace if not exists " <> fromText k
        <> fromText " with replication = { "
        <> fromText "    'class': 'SimpleStrategy' "
        <> fromText "  , 'replication_factor': '"    <> fromString (show n) <> "'"
        <> fromText "};"

    cql (NetworkTopologyStrategy (ReplicationMap dcs)) = QueryString . toLazyText $
           fromText "create keyspace if not exists " <> fromText k
        <> fromText " with replication = { "
        <> fromText "    'class': 'NetworkTopologyStrategy' "
        <> fromText "  , " <> fromText (intercalate "," (map pair dcs))
        <> fromText "};"

    pair (dc, ReplicationFactor n) = "'" <> dc <> "': " <> pack (show n)

useKeyspace :: Keyspace -> Client ()
useKeyspace (Keyspace k) = do
    r <- qry
    case r of
        RsResult _ (SetKeyspaceResult _) -> return ()
        RsError _ e                      -> throwM e
        _                                -> throwM (UnexpectedResponse' r)
  where
    qry  = request (RqQuery (Query cql prms)) :: Client (Response () () ())
    prms = QueryParams One False () Nothing Nothing Nothing
    cql  = QueryString $ "use \"" <> fromStrict k <> "\""

migrateSchema :: Logger -> MigrationOpts -> [Migration] -> IO ()
migrateSchema l o ms = do
    -- if migHost is a DNS name, resolve it and connect to all nodes
    hosts <- initialContactsDNS $ pack (migHost o)
    p <- Database.CQL.IO.init l $
            setContacts (NonEmpty.head hosts) (NonEmpty.tail hosts)
          . setPortNumber (fromIntegral $ migPort o)
          . setMaxConnections 1
          . setPoolStripes 1
          -- 'migrationPolicy' ensures we only talk to one host for all queries
          -- required for correct functioning of 'waitForSchemaConsistency'
          . setPolicy migrationPolicy
          -- use higher timeouts on schema migrations to reduce the probability
          -- of a timeout happening during 'migAction' or 'metaInsert',
          -- as that can lead to a state where schema migrations cannot be re-run
          -- without manual action.
          -- (due to e.g. "cannot create table X, already exists" errors)
          . setConnectTimeout 20
          . setSendTimeout 20
          . setResponseTimeout 50
          . setProtocolVersion V3
          $ defSettings
    runClient p $ do
        let keyspace = Keyspace . migKeyspace $ o
        when (migReset o) $ do
            info "Dropping keyspace."
            void $ schema (dropKeyspace keyspace) (params All ())
        createKeyspace keyspace (migRepl o)
        useKeyspace keyspace
        void $ schema metaCreate (params All ())
        migrations <- newer <$> schemaVersion
        if null migrations
            then info "No new migrations."
            else info "New migrations found."
        forM_ migrations $ \Migration{..} -> do
            info $ "[" <> pack (show migVersion) <> "] " <> migText
            migAction
            now <- liftIO getCurrentTime
            write metaInsert (params All (migVersion, migText, now))
            info "Waiting for schema version consistency across peers..."
            waitForSchemaConsistency
            info "... done waiting."
  where
    newer v = dropWhile (maybe (const False) (>=) v . migVersion)
            . sortBy (\x y -> migVersion x `compare` migVersion y)
            $ ms

    info = liftIO . log l Info . msg

    dropKeyspace :: Keyspace -> QueryString S () ()
    dropKeyspace (Keyspace k) = QueryString $ "drop keyspace if exists \"" <>  fromStrict k <> "\""

    metaCreate :: QueryString S () ()
    metaCreate = "create columnfamily if not exists meta (id int, version int, descr text, date timestamp, primary key (id, version))"

    metaInsert :: QueryString W (Int32, Text, UTCTime) ()
    metaInsert = "insert into meta (id, version, descr, date) values (1,?,?,?)"

-- | Retrieve and compare local and peer system schema versions.
-- if they don't match, retry once per second for 30 seconds
waitForSchemaConsistency :: Client ()
waitForSchemaConsistency = do
    void $ retryWhileN 30 inDisagreement getSystemVersions
  where
    getSystemVersions :: Client (UUID, [UUID])
    getSystemVersions = do
        -- These two sub-queries must be made to the same node.
        -- (comparing local from node A and peers from node B wouldn't be correct)
        -- using the custom 'migrationPolicy' when connecting to cassandra ensures this.
        local <- systemLocalVersion
        peers <- systemPeerVersions
        case local of
            Just localVersion -> return $ (localVersion, peers)
            Nothing           -> error "No system_version in system.local (should never happen)"

    inDisagreement :: (UUID, [UUID]) -> Bool
    inDisagreement (localVersion, peers) = not $ all (== localVersion) peers

    systemLocalVersion :: Client (Maybe UUID)
    systemLocalVersion = fmap runIdentity <$> qry
      where
        qry = retry x1 (query1 cql (params One ()))

        cql :: PrepQuery R () (Identity UUID)
        cql = "select schema_version from system.local"

    systemPeerVersions :: Client [UUID]
    systemPeerVersions = fmap runIdentity <$> qry
      where
        qry = retry x1 (query cql (params One ()))

        cql :: PrepQuery R () (Identity UUID)
        cql = "select schema_version from system.peers"

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
                             (const (return . f))
                             (const m)

-- | The migrationPolicy selects only one and always the same host
migrationPolicy :: IO Policy
migrationPolicy = do
    h <- newIORef Nothing
    return $ Policy
      { setup      = setHost h
      , onEvent    = const $ return ()
      , select     = readIORef h
      , acceptable = const $ return True
      , hostCount  = fromIntegral . length . maybeToList <$> readIORef h
      , display    = ("migrationPolicy: " ++) . show <$> readIORef h
      , current    = maybeToList <$> readIORef h
      }
  where
    setHost h (a:_) _ = writeIORef h (Just a)
    setHost _    _  _ = return ()

migrationOptsParser :: Parser MigrationOpts
migrationOptsParser = MigrationOpts
    <$> (strOption $
            long "host"
            <> metavar "HOST"
            <> value "localhost"
            <> help "Cassandra host")

    <*> (option auto $
            long "port"
            <> metavar "PORT"
            <> value 9042
            <> help "Cassandra port")

    <*> ((fmap pack) . strOption $
            long "keyspace"
            <> metavar "STRING"
            <> help "Cassandra Keyspace")

    <*> ((fmap (SimpleStrategy . ReplicationFactor) . option auto $
            long "replication-factor"
            <> metavar "INT"
            <> help "Replication Factor")
        <|>
        (fmap NetworkTopologyStrategy . option auto $
            long "replication-map"
            <> metavar "STRING"
            <> help "Replication Map (i.e. \"eu-west:3,us-east:3\")"))

    <*> (switch $
            long "reset"
            <> help "Reset the keyspace before running migrations")
