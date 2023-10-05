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
    Writetime (..),
    writetimeToInt64,
  )
where

import Cassandra (ClientState, init)
import Cassandra.CQL
import Cassandra.Settings (defSettings, setContacts, setKeyspace, setLogger, setPortNumber, setSSLContext)
import Data.Aeson
import Data.Fixed
import Data.Text (unpack)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Database.CQL.IO.Tinylog qualified as CT
import Debug.Trace
import Imports hiding (init)
import OpenSSL.Session qualified as OpenSSL
import System.Logger qualified as Log

defInitCassandra :: Text -> Text -> Word16 -> Maybe FilePath -> Log.Logger -> IO ClientState
defInitCassandra ks h p mbCertPath lg = do
  mbSSLContext <- createSSLContext mbCertPath
  let basicCasSettings =
        setLogger (CT.mkLogger lg)
          . setPortNumber (fromIntegral p)
          . setContacts (unpack h) []
          . setKeyspace (Keyspace ks)
          $ defSettings
      casSettings = maybe basicCasSettings (\sslCtx -> setSSLContext sslCtx basicCasSettings) mbSSLContext
  init casSettings
  where
    createSSLContext :: Maybe FilePath -> IO (Maybe OpenSSL.SSLContext)
    createSSLContext (Just tlsCertPath) = do
      traceM $ "cassandra-util: " ++ show tlsCertPath
      sslContext <- OpenSSL.context
      OpenSSL.contextSetCAFile sslContext tlsCertPath
      OpenSSL.contextSetVerificationMode
        sslContext
        OpenSSL.VerifyPeer
          { vpFailIfNoPeerCert = True,
            vpClientOnce = True,
            vpCallback = Nothing
          }
      pure $ Just sslContext
    createSSLContext Nothing = pure Nothing

-- | Read cassandra's writetimes https://docs.datastax.com/en/dse/5.1/cql/cql/cql_using/useWritetime.html
-- as UTCTime values without any loss of precision
newtype Writetime a = Writetime {writetimeToUTC :: UTCTime}

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
