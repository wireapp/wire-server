-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
  ( writeTimeToUTC,
    defInitCassandra,
    Writetime,
  )
where

import Cassandra (ClientState, Keyspace (Keyspace), init)
import Cassandra.Settings (defSettings, setContacts, setKeyspace, setLogger, setPortNumber)
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.CQL.IO.Tinylog as CT
import Imports hiding (init)
import qualified System.Logger as Log

type Writetime a = Int64

writeTimeToUTC :: Writetime a -> UTCTime
writeTimeToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000000)

defInitCassandra :: Text -> Text -> Word16 -> Log.Logger -> IO ClientState
defInitCassandra ks h p lg =
  init $
    setLogger (CT.mkLogger lg)
      . setPortNumber (fromIntegral p)
      . setContacts (unpack h) []
      . setKeyspace (Keyspace ks)
      $ defSettings
