{-# OPTIONS_GHC -Wno-orphans #-}

module ClientsWithoutPrekeys.Clients where

import Brig.Data.Instances ()
import Brig.User.Auth.DB.Instances ()
import Cassandra
import Conduit
import Data.ByteString.Lazy (hPut)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Csv (ToField (toField))
import Data.Csv.Incremental (encode, encodeRecord)
import Data.Id
import Data.Json.Util (UTCTimeMillis ())
import Imports
import System.IO (Handle)
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Client (ClientClass, ClientType)

clients :: Logger -> ClientState -> Handle -> ConduitT () () IO ()
clients l cassandra h =
  zipSources
    (C.sourceList [(1 :: Int32) ..])
    (transPipe (runClient cassandra) getClients)
    .| C.mapM
      ( \(i, p) ->
          Log.info l (Log.field "clients" (show (i * pageSize)))
            >> pure p
      )
    .| C.mapM
      ( runClient cassandra
          . pooledMapConcurrentlyN
            10
            ( \row@(u, c, _, _, _, _, _, _) ->
                countPrekeys u c
                  >>= \count -> pure (count, row)
            )
      )
    .| C.map (map snd . filter ((0 ==) . fst))
    -- .| C.mapM_ (mapM_ print)
    .| C.mapM_ (hPut h . encode . foldMap encodeRecord)

instance ToField UserId where
  toField u = toField $ show u

instance ToField ClientId where
  toField c = toField $ show c

instance ToField ClientType where
  toField c = toField $ show c

instance ToField ClientClass where
  toField c = toField $ show c

instance ToField CookieLabel where
  toField c = toField $ show c

instance ToField UTCTimeMillis where
  toField u = toField $ show u

pageSize :: Int32
pageSize = 1000

type ClientsRow = (UserId, ClientId, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe Text, UTCTimeMillis, ClientType)

getClients :: ConduitM () [ClientsRow] Client ()
getClients = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () ClientsRow
    cql = "SELECT user, client, class, cookie, label, model, tstamp, type FROM clients"

countPrekeys :: MonadClient m => UserId -> ClientId -> m Int64
countPrekeys u c = runIdentity . fromJust <$> retry x5 (query1 countPrekeysQ (params LocalQuorum (u, c)))
  where
    countPrekeysQ :: PrepQuery R (UserId, ClientId) (Identity Int64)
    countPrekeysQ = "SELECT COUNT(*) FROM prekeys WHERE user = ? and client = ?"
