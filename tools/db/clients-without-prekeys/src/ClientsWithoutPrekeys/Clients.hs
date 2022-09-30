{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ClientsWithoutPrekeys.Clients where

-- import Brig.Data.Instances ()
-- import Brig.User.Auth.DB.Instances ()
import Cassandra
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Conduit
import Control.Exception
import Control.Lens hiding (argument)
import Data.ByteString.Lazy (hPut)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Csv (EncodeOptions (encQuoting), Quoting (QuoteAll), ToField (toField), defaultEncodeOptions)
import Data.Csv.Incremental (encodeRecord, encodeWith)
import Data.Id
import qualified Data.Id as Id
import Data.Json.Util (UTCTimeMillis ())
import Data.Text.Lens (packed)
import qualified Data.Vector as V
import Imports
import Options.Applicative
import System.IO (Handle, openBinaryFile)
import System.Logger (Logger)
import qualified System.Logger as Log
import qualified System.Logger as Logger
import UnliftIO.Async
import Wire.API.User.Auth
import Wire.API.User.Client hiding (Client)

deriving instance Cql CookieLabel

instance Cql ClientType where
  ctype = Tagged IntColumn
  toCql TemporaryClientType = CqlInt 0
  toCql PermanentClientType = CqlInt 1
  toCql LegalHoldClientType = CqlInt 2

  fromCql (CqlInt 0) = pure TemporaryClientType
  fromCql (CqlInt 1) = pure PermanentClientType
  fromCql (CqlInt 2) = pure LegalHoldClientType
  fromCql _ = Left "ClientType: Int [0, 2] expected"

instance Cql ClientClass where
  ctype = Tagged IntColumn
  toCql PhoneClient = CqlInt 0
  toCql TabletClient = CqlInt 1
  toCql DesktopClient = CqlInt 2
  toCql LegalHoldClient = CqlInt 3

  fromCql (CqlInt 0) = pure PhoneClient
  fromCql (CqlInt 1) = pure TabletClient
  fromCql (CqlInt 2) = pure DesktopClient
  fromCql (CqlInt 3) = pure LegalHoldClient
  fromCql _ = Left "ClientClass: Int [0, 3] expected"

fetchClients :: Logger -> ClientState -> Handle -> ConduitT () Void IO ()
fetchClients l cassandra h =
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
          . pooledMapConcurrentlyN -- TODO change
            3
            ( \row@(u, c, _, _, _, _, _, _) ->
                countPrekeys u c
                  >>= \count -> pure (count, row)
            )
      )
    .| C.map (map (uncurry addCount))
    -- .| C.mapM_ (mapM_ print)
    .| C.mapM_
      ( (\x -> hPut h x >> hFlush h)
          . encodeWith (defaultEncodeOptions {encQuoting = QuoteAll})
          . foldMap
            ( \(a1, a2, a3, a4, a5, a6, a7, a8, a9) ->
                encodeRecord
                  ( V.fromList
                      ( [ toField a1,
                          toField a2,
                          toField a3,
                          toField a4,
                          toField a5,
                          toField a6,
                          toField a7,
                          toField a8,
                          toField a9
                        ]
                      )
                  )
            )
      )

-- .| C.mapM_ ((\x -> hPut h x >> hFlush h) . encode . foldMap encodeRecord)

instance ToField UserId where
  toField u = toField $ show u

instance ToField ClientId where
  toField c = toField $ Id.client c

instance ToField ClientType where
  toField c = toField $ show c

instance ToField ClientClass where
  toField c = toField $ show c

instance ToField CookieLabel where
  toField c = toField $ cookieLabelText c

instance ToField UTCTimeMillis where
  toField u = toField $ show u

pageSize :: Int32
pageSize = 1000

type ClientsRow =
  ( Maybe UserId,
    Maybe ClientId,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe Text,
    Maybe UTCTimeMillis,
    Maybe ClientType
  )

type ClientsRowPlusCount =
  ( Int64,
    Maybe UserId,
    Maybe ClientId,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe Text,
    Maybe UTCTimeMillis,
    Maybe ClientType
  )

addCount :: Int64 -> ClientsRow -> ClientsRowPlusCount
addCount cnt (a1, a2, a3, a4, a5, a6, a7, a8) = (cnt, a1, a2, a3, a4, a5, a6, a7, a8)

getClients :: ConduitM () [ClientsRow] Client ()
getClients = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () ClientsRow
    -- TODO: Remove LIMIT
    cql = "SELECT user, client, class, cookie, label, model, tstamp, type FROM clients LIMIT 2000"

countPrekeys :: MonadClient m => Maybe UserId -> Maybe ClientId -> m Int64
countPrekeys (Just u) (Just c) =
  runIdentity . fromJust <$> retry x5 (query1 countPrekeysQ (params LocalQuorum (u, c)))
  where
    countPrekeysQ :: PrepQuery R (UserId, ClientId) (Identity Int64)
    countPrekeysQ = "SELECT COUNT(*) FROM prekeys WHERE user = ? and client = ?"
countPrekeys _ _ = pure 0

data Opts = Opts
  { cHost :: String,
    cPort :: Int,
    cKeyspace :: C.Keyspace,
    output :: FilePath
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
    <*> ( strOption
            ( long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "output file"
            )
        )

main :: IO ()
main = do
  Opts {..} <- execParser (info (helper <*> optParser) fullDesc)
  client <-
    C.init
      . C.setContacts cHost []
      . C.setPortNumber (fromIntegral cPort)
      . C.setKeyspace cKeyspace
      $ C.defSettings
  logger <- liftIO $ Logger.new Logger.defSettings
  out <- openBinaryFile output WriteMode
  void (runConduit (fetchClients logger client out))
    `finally` do
      hClose out
      shutdown client
  pure ()
