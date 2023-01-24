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
{-# LANGUAGE NumericUnderscores #-}

module Cassandra.Util
  ( defInitCassandra,
    Writetime (..),
    writetimeToInt64,
  )
where

import Cassandra (ClientState, init)
import Cassandra.CQL
import Cassandra.Settings (defSettings, setContacts, setKeyspace, setLogger, setPortNumber)
import Data.Aeson
import Data.Fixed
import Data.Text (unpack)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import qualified Database.CQL.IO.Tinylog as CT
import Imports hiding (init)
import qualified System.Logger as Log

defInitCassandra :: Text -> Text -> Word16 -> Log.Logger -> IO ClientState
defInitCassandra ks h p lg =
  init
    $ setLogger (CT.mkLogger lg)
      . setPortNumber (fromIntegral p)
      . setContacts (unpack h) []
      . setKeyspace (Keyspace ks)
    $ defSettings

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
