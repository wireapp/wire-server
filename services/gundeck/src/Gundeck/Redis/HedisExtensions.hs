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
module Gundeck.Redis.HedisExtensions
  ( ClusterInfoResponse (..),
    ClusterInfoResponseState (..),
    clusterInfo,
    checkedConnectCluster,
    ClusterDownError,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Database.Redis
import Imports hiding (Down)
import UnliftIO

-- https://redis.io/commands/cluster-info/
data ClusterInfoResponse = ClusterInfoResponse
  { clusterInfoResponseState :: ClusterInfoResponseState,
    clusterInfoResponseSlotsAssigned :: Integer,
    clusterInfoResponseSlotsOK :: Integer,
    clusterInfoResponseSlotsPfail :: Integer,
    clusterInfoResponseSlotsFail :: Integer,
    clusterInfoResponseKnownNodes :: Integer,
    clusterInfoResponseSize :: Integer,
    clusterInfoResponseCurrentEpoch :: Integer,
    clusterInfoResponseMyEpoch :: Integer,
    clusterInfoResponseStatsMessagesSent :: Integer,
    clusterInfoResponseStatsMessagesReceived :: Integer,
    clusterInfoResponseTotalLinksBufferLimitExceeded :: Integer,
    clusterInfoResponseStatsMessagesPingSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesPingReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesPongSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesPongReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesMeetSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesMeetReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesFailSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesFailReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesPublishSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesPublishReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesAuthReqSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesAuthReqReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesAuthAckSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesAuthAckReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesUpdateSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesUpdateReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesMfstartSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesMfstartReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesModuleSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesModuleReceived :: Maybe Integer,
    clusterInfoResponseStatsMessagesPublishshardSent :: Maybe Integer,
    clusterInfoResponseStatsMessagesPublishshardReceived :: Maybe Integer
  }
  deriving (Show, Eq)

data ClusterInfoResponseState
  = OK
  | Down
  deriving (Show, Eq)

defClusterInfoResponse :: ClusterInfoResponse
defClusterInfoResponse =
  ClusterInfoResponse
    { clusterInfoResponseState = Down,
      clusterInfoResponseSlotsAssigned = 0,
      clusterInfoResponseSlotsOK = 0,
      clusterInfoResponseSlotsPfail = 0,
      clusterInfoResponseSlotsFail = 0,
      clusterInfoResponseKnownNodes = 0,
      clusterInfoResponseSize = 0,
      clusterInfoResponseCurrentEpoch = 0,
      clusterInfoResponseMyEpoch = 0,
      clusterInfoResponseStatsMessagesSent = 0,
      clusterInfoResponseStatsMessagesReceived = 0,
      clusterInfoResponseTotalLinksBufferLimitExceeded = 0,
      clusterInfoResponseStatsMessagesPingSent = Nothing,
      clusterInfoResponseStatsMessagesPingReceived = Nothing,
      clusterInfoResponseStatsMessagesPongSent = Nothing,
      clusterInfoResponseStatsMessagesPongReceived = Nothing,
      clusterInfoResponseStatsMessagesMeetSent = Nothing,
      clusterInfoResponseStatsMessagesMeetReceived = Nothing,
      clusterInfoResponseStatsMessagesFailSent = Nothing,
      clusterInfoResponseStatsMessagesFailReceived = Nothing,
      clusterInfoResponseStatsMessagesPublishSent = Nothing,
      clusterInfoResponseStatsMessagesPublishReceived = Nothing,
      clusterInfoResponseStatsMessagesAuthReqSent = Nothing,
      clusterInfoResponseStatsMessagesAuthReqReceived = Nothing,
      clusterInfoResponseStatsMessagesAuthAckSent = Nothing,
      clusterInfoResponseStatsMessagesAuthAckReceived = Nothing,
      clusterInfoResponseStatsMessagesUpdateSent = Nothing,
      clusterInfoResponseStatsMessagesUpdateReceived = Nothing,
      clusterInfoResponseStatsMessagesMfstartSent = Nothing,
      clusterInfoResponseStatsMessagesMfstartReceived = Nothing,
      clusterInfoResponseStatsMessagesModuleSent = Nothing,
      clusterInfoResponseStatsMessagesModuleReceived = Nothing,
      clusterInfoResponseStatsMessagesPublishshardSent = Nothing,
      clusterInfoResponseStatsMessagesPublishshardReceived = Nothing
    }

parseClusterInfoResponse :: [[ByteString]] -> ClusterInfoResponse -> Maybe ClusterInfoResponse
parseClusterInfoResponse fields resp = case fields of
  [] -> pure resp
  (["cluster_state", state] : fs) -> parseState state >>= \s -> parseClusterInfoResponse fs $ resp {clusterInfoResponseState = s}
  (["cluster_slots_assigned", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseSlotsAssigned = v}
  (["cluster_slots_ok", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseSlotsOK = v}
  (["cluster_slots_pfail", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseSlotsPfail = v}
  (["cluster_slots_fail", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseSlotsFail = v}
  (["cluster_known_nodes", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseKnownNodes = v}
  (["cluster_size", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseSize = v}
  (["cluster_current_epoch", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseCurrentEpoch = v}
  (["cluster_my_epoch", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseMyEpoch = v}
  (["cluster_stats_messages_sent", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesSent = v}
  (["cluster_stats_messages_received", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesReceived = v}
  (["total_cluster_links_buffer_limit_exceeded", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseTotalLinksBufferLimitExceeded = fromMaybe 0 $ parseInteger value} -- this value should be mandatory according to the spec, but isn't necessarily set in Redis 6
  (["cluster_stats_messages_ping_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPingSent = parseInteger value}
  (["cluster_stats_messages_ping_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPingReceived = parseInteger value}
  (["cluster_stats_messages_pong_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPongSent = parseInteger value}
  (["cluster_stats_messages_pong_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPongReceived = parseInteger value}
  (["cluster_stats_messages_meet_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesMeetSent = parseInteger value}
  (["cluster_stats_messages_meet_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesMeetReceived = parseInteger value}
  (["cluster_stats_messages_fail_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesFailSent = parseInteger value}
  (["cluster_stats_messages_fail_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesFailReceived = parseInteger value}
  (["cluster_stats_messages_publish_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPublishSent = parseInteger value}
  (["cluster_stats_messages_publish_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPublishReceived = parseInteger value}
  (["cluster_stats_messages_auth_req_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesAuthReqSent = parseInteger value}
  (["cluster_stats_messages_auth_req_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesAuthReqReceived = parseInteger value}
  (["cluster_stats_messages_auth_ack_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesAuthAckSent = parseInteger value}
  (["cluster_stats_messages_auth_ack_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesAuthAckReceived = parseInteger value}
  (["cluster_stats_messages_update_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesUpdateSent = parseInteger value}
  (["cluster_stats_messages_update_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesUpdateReceived = parseInteger value}
  (["cluster_stats_messages_mfstart_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesMfstartSent = parseInteger value}
  (["cluster_stats_messages_mfstart_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesMfstartReceived = parseInteger value}
  (["cluster_stats_messages_module_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesModuleSent = parseInteger value}
  (["cluster_stats_messages_module_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesModuleReceived = parseInteger value}
  (["cluster_stats_messages_publishshard_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPublishshardSent = parseInteger value}
  (["cluster_stats_messages_publishshard_received", value] : fs) -> parseClusterInfoResponse fs $ resp {clusterInfoResponseStatsMessagesPublishshardReceived = parseInteger value}
  (_ : fs) -> parseClusterInfoResponse fs resp
  where
    parseState bs = case bs of
      "ok" -> Just OK
      "fail" -> Just Down
      _ -> Nothing
    parseInteger = fmap fst . Char8.readInteger

instance RedisResult ClusterInfoResponse where
  decode r@(Bulk (Just bulkData)) =
    maybe (Left r) Right
      . flip parseClusterInfoResponse defClusterInfoResponse
      . map (Char8.split ':' . Char8.takeWhile (/= '\r'))
      $ Char8.lines bulkData
  decode r = Left r

clusterInfo :: (RedisCtx m f) => m (f ClusterInfoResponse)
clusterInfo = sendRequest ["CLUSTER", "INFO"]

checkedConnectCluster :: ConnectInfo -> IO Connection
checkedConnectCluster connInfo = do
  conn <- connectCluster connInfo
  res <- runRedis conn clusterInfo
  case res of
    Right r -> case clusterInfoResponseState r of
      OK -> pure conn
      _ -> throwIO $ ClusterDownError r
    Left e -> throwIO $ ConnectSelectError e

newtype ClusterDownError = ClusterDownError ClusterInfoResponse deriving (Eq, Show, Typeable)

instance Exception ClusterDownError
