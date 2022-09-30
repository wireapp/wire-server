{-# LANGUAGE RecordWildCards #-}

module ClientsWithoutPrekeys.Cache where

import Cassandra
import qualified Cassandra as C
import ClientsWithoutPrekeys.Data
import ClientsWithoutPrekeys.GrafanaLogs
import Control.Exception
import Control.Lens hiding (argument)
import Data.Id
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Lens (packed)
import Data.Time
import qualified Database.CQL.IO as C
import Imports hiding (ByteString)
import Options.Applicative
import System.FilePath
import System.IO (hGetContents, hPrint)
import UnliftIO.Async

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, UTCTimeMillis)
selectClient = "SELECT client, tstamp where user = ? and client = ?"

lookupClient :: MonadClient m => UserId -> ClientId -> m (Maybe UTCTimeMillis)
lookupClient u c =
  snd <$$> query1 selectClient (params LocalQuorum (u, c))

cacheByTimestamp :: C.ClientState -> [FilePath] -> IO (Map UserClient UTCTime)
cacheByTimestamp cassandra inputs = do
  (bads, goods) <- splitBadGood <$> mapM readCSV inputs
  report bads
  let userClients = Set.fromList $ mergeUnique goods

  cacheFN <- metaCache
  (cache :: Map UserClient UTCTime) <- read <$> withFile cacheFN ReadMode hGetContents
  let newUserClients = Set.toList $ userClients Set.\\ Map.keysSet cache

  newCacheItems <- pooledForConcurrentlyN 20 newUserClients $ \userClient@UserClient {..} ->
    (userClient,) . fromUTCTimeMillis <$$> runClient cassandra (lookupClient uid cid)
  let newCache = (cache `Map.union`) . Map.fromList . catMaybes $ newCacheItems
  withFile cacheFN WriteMode $ \h -> hPrint h $ newCache

  pure $ Map.filterWithKey (const . flip Set.member userClients) newCache

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "clients-without-prekeys"

metaCache :: IO FilePath
metaCache = flip (</>) "meta.hshow" <$> cacheDir

main :: IO ()
main = do
  Opts {..} <- execParser (info (helper <*> optParser) desc)
  client <-
    C.init
      . C.setContacts cHost []
      . C.setPortNumber (fromIntegral cPort)
      . C.setKeyspace cKeyspace
      $ C.defSettings
  creationTimes <- cacheByTimestamp client inputFNs `finally` shutdown client
  maybe
    (print creationTimes)
    (($ flip hPrint creationTimes) . flip withFile WriteMode)
    output
  where
    desc =
      header "clients-without-prekeys-from-logs-meta"
        <> progDesc "find metadata for clients without prekeys in Cassandra brig"
        <> fullDesc

data Opts = Opts
  { cHost :: String,
    cPort :: Int,
    cKeyspace :: C.Keyspace,
    output :: Maybe FilePath,
    inputFNs :: [FilePath]
  }

optParser :: Parser Opts
optParser =
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
                  <> metavar "KEYSPACE"
                  <> help "Cassandra Keyspace"
                  <> value "brig_test"
                  <> showDefault
              )
        )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "output file"
          )
      )
    <*> many (argument str (metavar "CSV..."))
