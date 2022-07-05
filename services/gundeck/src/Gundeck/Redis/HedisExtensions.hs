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
  )
where

import qualified Data.ByteString.Char8 as Char8
import Database.Redis
import Imports hiding (Down)
import UnliftIO

data ClusterInfoResponse = ClusterInfoResponse
  { clusterInfoResponseState :: ClusterInfoResponseState,
    clusterInfoResponseSlotsAssigned :: Integer,
    clusterInfoResponseSlotsOK :: Integer,
    clusterInfoResponseSlotsPfail :: Integer,
    clusterInfoResponseSlotsFail :: Integer,
    clusterInfoResponseKnownNodes :: Integer,
    clusterInfoResponseClusterSize :: Integer,
    clusterInfoResponseClusterCurrentEpoch :: Integer,
    clusterInfoResponseClusterMyEpoch :: Integer,
    clusterInfoResponseClusterStatsMessagesPingSent :: Integer,
    clusterInfoResponseClusterStatsMessagesPongSent :: Integer,
    clusterInfoResponseClusterStatsMessagesSent :: Integer,
    clusterInfoResponseClusterStatsMessagesPingReceived :: Integer,
    clusterInfoResponseClusterStatsMessagesPongReceived :: Integer,
    clusterInfoResponseClusterStatsMessagesMeetReceived :: Integer,
    clusterInfoResponseClusterStatsMessagesReceived :: Integer
  }
  deriving (Show, Eq)

data ClusterInfoResponseState
  = OK
  | Down
  deriving (Show, Eq)

instance RedisResult ClusterInfoResponse where
  decode r@(Bulk (Just bulkData)) = maybe (Left r) Right $
    case map (Char8.split ':' . Char8.takeWhile (/= '\r')) $ Char8.lines bulkData of
      ( ["cluster_state", state]
          : ["cluster_slots_assigned", slotsAssigned]
          : ["cluster_slots_ok", slotsOK]
          : ["cluster_slots_pfail", slotsPfail]
          : ["cluster_slots_fail", slotsFail]
          : ["cluster_known_nodes", knownNodes]
          : ["cluster_size", clusterSize]
          : ["cluster_current_epoch", clusterCurrentEpoch]
          : ["cluster_my_epoch", clusterMyEpoch]
          : ["cluster_stats_messages_ping_sent", clusterStatsMessagesPingSent]
          : ["cluster_stats_messages_pong_sent", clusterStatsMessagesPongSent]
          : ["cluster_stats_messages_sent", clusterStatsMessagesSent]
          : ["cluster_stats_messages_ping_received", clusterStatsMessagesPingReceived]
          : ["cluster_stats_messages_pong_received", clusterStatsMessagesPongReceived]
          : ["cluster_stats_messages_meet_received", clusterStatsMessagesMeetReceived]
          : ["cluster_stats_messages_received", clusterStatsMessagesReceived]
          : _
        ) ->
          ClusterInfoResponse
            <$> parseState state
            <*> parseInteger slotsAssigned
            <*> parseInteger slotsOK
            <*> parseInteger slotsPfail
            <*> parseInteger slotsFail
            <*> parseInteger knownNodes
            <*> parseInteger clusterSize
            <*> parseInteger clusterCurrentEpoch
            <*> parseInteger clusterMyEpoch
            <*> parseInteger clusterStatsMessagesPingSent
            <*> parseInteger clusterStatsMessagesPongSent
            <*> parseInteger clusterStatsMessagesSent
            <*> parseInteger clusterStatsMessagesPingReceived
            <*> parseInteger clusterStatsMessagesPongReceived
            <*> parseInteger clusterStatsMessagesMeetReceived
            <*> parseInteger clusterStatsMessagesReceived
          where
            parseState bs = case bs of
              "ok" -> Just OK
              "fail" -> Just Down
              _ -> Nothing
            parseInteger = fmap fst . Char8.readInteger
      _ -> Nothing
  decode r = Left r

clusterInfo :: RedisCtx m f => m (f ClusterInfoResponse)
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
