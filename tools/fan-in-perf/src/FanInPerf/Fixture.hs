-- | Shared database access for the single experimental epoch.
module FanInPerf.Fixture where

import Data.Id
import Data.Misc (Duration (..))
import Data.Qualified (toLocalUnsafe)
import Data.UUID (UUID)
import FanInPerf.NotificationsStore
import FanInPerf.Store
import FanInPerf.Targets (localDomain)
import Hasql.Pool.Extended (PoolConfig (..), initPostgresPool)
import Hasql.TH
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted))
import Imports
import Util.Options (FilePathSecrets)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.Postgres (runStatement)

benchmarkGroup :: GroupId
benchmarkGroup = GroupId "fan-in-perf-experimental"

benchmarkEpoch :: Epoch
benchmarkEpoch = Epoch 1

benchmarkStream :: Stream
benchmarkStream = StreamEpoch benchmarkGroup benchmarkEpoch

configureStore :: Map Text Text -> Maybe PoolConfig -> Maybe FilePathSecrets -> IO Env
configureStore config options password = do
  pool <- initPostgresPool (fromMaybe (PoolConfig 32 (Duration 5) (Duration 600)) options) config password
  pure $ Env pool (toLocalUnsafe localDomain ()) ReadCommitted

-- Nothing means unregistered. A registered client with no acknowledgement starts
-- at the beginning of the retained synthetic fixture, never at the latest event.
resumePosition :: Env -> UserId -> ClientId -> IO (Maybe UUID)
resumePosition env user client =
  runOrAbort "load experimental cursor" env $
    runStatement
      (user.toUUID, fromIntegral client.clientToWord64 :: Int64, benchmarkGroup.unGroupId, fromIntegral benchmarkEpoch.epochNumber :: Int64)
      [maybeStatement|
      select coalesce(a.last_notification_id, '00000000-0000-0000-0000-000000000000'::uuid) :: uuid
      from fan_in_perf_participants p
      left join epoch_notification_acks a on a.user_id = p.user_id and a.client_id = p.client_id
        and a.group_id = $3 :: bytea and a.epoch = $4 :: int8
      where p.user_id = $1 :: uuid and p.client_id = $2 :: int8
    |]
