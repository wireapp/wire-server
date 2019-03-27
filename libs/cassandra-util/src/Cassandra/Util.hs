module Cassandra.Util (writeTimeToUTC, defInitCassandra, Writetime) where

import Imports hiding (init)
import Cassandra (ClientState, Keyspace(Keyspace), init)
import Cassandra.Settings (setPortNumber, setContacts, setKeyspace, defSettings)
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import System.Logger (Logger)

type Writetime a = Int64

writeTimeToUTC :: Writetime a -> UTCTime
writeTimeToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000000)

defInitCassandra :: Text -> Text -> Word16 -> Logger -> IO ClientState
defInitCassandra ks h p lg =
    init lg $ setPortNumber (fromIntegral p)
            . setContacts (unpack h) []
            . setKeyspace (Keyspace ks)
            $ defSettings
