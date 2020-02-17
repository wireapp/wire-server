module Cassandra.Util (writeTimeToUTC, defInitCassandra, Writetime) where

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
  init
    $ setLogger (CT.mkLogger lg)
      . setPortNumber (fromIntegral p)
      . setContacts (unpack h) []
      . setKeyspace (Keyspace ks)
    $ defSettings
