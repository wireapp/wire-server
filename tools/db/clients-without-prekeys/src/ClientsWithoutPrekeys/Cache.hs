{-# LANGUAGE RecordWildCards #-}

module ClientsWithoutPrekeys.Cache where

import Brig.Data.Client
import ClientsWithoutPrekeys.Data
import ClientsWithoutPrekeys.GrafanaLogs
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Database.CQL.IO
import qualified Database.CQL.IO as C
import Imports hiding (ByteString)
import System.FilePath
import System.IO (hGetContents, hPrint)
import UnliftIO.Async
import Wire.API.User.Client

cacheByTimestamp :: C.ClientState -> [FilePath] -> IO (Map UserClient UTCTime)
cacheByTimestamp cassandra inputs = do
  (bads, goods) <- splitBadGood <$> mapM readCSV inputs
  report bads
  let userClients = Set.fromList $ mergeUnique goods

  cacheFN <- metaCache
  (cache :: Map UserClient UTCTime) <- read <$> withFile cacheFN ReadMode hGetContents
  let newUserClients = Set.toList $ userClients Set.\\ Map.keysSet cache

  newCacheItems <- pooledForConcurrentlyN 20 newUserClients $ \userClient@UserClient {..} ->
    (userClient,) . fromUTCTimeMillis . clientTime <$$> runClient cassandra (lookupClient uid cid)
  let newCache = (cache `Map.union`) . Map.fromList . catMaybes $ newCacheItems
  withFile cacheFN WriteMode $ \handle -> hPrint handle $ newCache

  pure $ Map.filterWithKey (const . flip Set.member userClients) newCache

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "clients-without-prekeys"

metaCache :: IO FilePath
metaCache = flip (</>) "meta.hshow" <$> cacheDir
