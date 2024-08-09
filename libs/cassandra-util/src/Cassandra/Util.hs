-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Cassandra.Util
  ( defInitCassandra,
    initCassandraForService,
    initCassandra,
    Writetime (..),
    writetimeToInt64,
  )
where

import Cassandra.CQL
import Cassandra.Options
import Cassandra.Schema
import Cassandra.Settings (dcFilterPolicyIfConfigured, initialContactsDisco, initialContactsPlain, mkLogger)
import Control.Lens
import Data.Aeson
import Data.Fixed
import Data.List.NonEmpty qualified as NE
import Data.Text (pack, unpack)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Database.CQL.IO
import Database.CQL.IO.Tinylog qualified as CT
import Imports hiding (init)
import OpenSSL.Session qualified as OpenSSL
import System.Logger qualified as Log

defInitCassandra :: CassandraOpts -> Log.Logger -> IO ClientState
defInitCassandra opts logger = do
  let basicCasSettings =
        setLogger (CT.mkLogger logger)
          . setPortNumber (fromIntegral (opts ^. endpoint . port))
          . setContacts (unpack (opts ^. endpoint . host)) []
          . setKeyspace (Keyspace (opts ^. keyspace))
          . setProtocolVersion V4
          $ defSettings
  initCassandra basicCasSettings (opts ^. tlsCa) logger

-- | Create Cassandra `ClientState` ("connection") for a service
initCassandraForService ::
  CassandraOpts ->
  String ->
  Maybe Text ->
  Maybe Int32 ->
  Log.Logger ->
  IO ClientState
initCassandraForService opts serviceName discoUrl mbSchemaVersion logger = do
  c <-
    maybe
      (initialContactsPlain (opts ^. endpoint . host))
      (initialContactsDisco ("cassandra_" ++ serviceName) . unpack)
      discoUrl
  let basicCasSettings =
        setLogger (mkLogger (Log.clone (Just (pack ("cassandra." ++ serviceName))) logger))
          . setContacts (NE.head c) (NE.tail c)
          . setPortNumber (fromIntegral (opts ^. endpoint . port))
          . setKeyspace (Keyspace (opts ^. keyspace))
          . setMaxConnections 4
          . setPoolStripes 4
          . setSendTimeout 3
          . setResponseTimeout 10
          . setProtocolVersion V4
          . setPolicy (dcFilterPolicyIfConfigured logger (opts ^. filterNodesByDatacentre))
          $ defSettings
  p <- initCassandra basicCasSettings (opts ^. tlsCa) logger
  maybe (pure ()) (\v -> runClient p $ (versionCheck v)) mbSchemaVersion
  pure p

initCassandra :: Settings -> Maybe FilePath -> Log.Logger -> IO ClientState
initCassandra settings (Just tlsCaPath) logger = do
  sslContext <- createSSLContext tlsCaPath
  let settings' = setSSLContext sslContext settings
  init settings'
  where
    createSSLContext :: FilePath -> IO OpenSSL.SSLContext
    createSSLContext certFile = do
      void . liftIO $ Log.debug logger (Log.msg ("TLS cert file path: " <> show certFile))
      sslContext <- OpenSSL.context
      OpenSSL.contextSetCAFile sslContext certFile
      OpenSSL.contextSetVerificationMode
        sslContext
        OpenSSL.VerifyPeer
          { vpFailIfNoPeerCert = True,
            vpClientOnce = True,
            vpCallback = Nothing
          }
      pure sslContext
initCassandra settings Nothing logger = do
  void . liftIO $ Log.debug logger (Log.msg ("No TLS cert file path configured." :: Text))
  init settings

-- | Read cassandra's writetimes https://docs.datastax.com/en/dse/5.1/cql/cql/cql_using/useWritetime.html
-- as UTCTime values without any loss of precision
newtype Writetime a = Writetime {writetimeToUTC :: UTCTime}
  deriving (Functor)

instance Cql (Writetime a) where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . writetimeToInt64
  fromCql (CqlBigInt n) =
    pure
      . Writetime
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . MkFixed
      . (* 1_000_000)
      . fromIntegral @Int64 @Integer
      $ n
  fromCql _ = Left "Writetime: bigint expected"

-- | This yields the same int as it is returned by WRITETIME()
writetimeToInt64 :: Writetime a -> Int64
writetimeToInt64 =
  fromIntegral @Integer @Int64
    . (`div` 1_000_000)
    . unfixed
    . nominalDiffTimeToSeconds
    . utcTimeToPOSIXSeconds
    . writetimeToUTC
  where
    unfixed :: Fixed a -> Integer
    unfixed (MkFixed n) = n

instance ToJSON (Writetime a) where
  toJSON = toJSON . writetimeToInt64
