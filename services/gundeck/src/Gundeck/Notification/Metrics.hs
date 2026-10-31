-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Metrics about stored notifications and about how they are fetched. They
-- describe the load on the notification store (fan-in, payload size, reads).
--
-- Stored notifications include the internal push-token-removal notifications
-- (see "Gundeck.React" and "Gundeck.Push.Native"), as they are written to the
-- same store.
module Gundeck.Notification.Metrics
  ( observeStored,
    FetchStats (..),
    observeFetch,
  )
where

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty)
import Imports
import Prometheus qualified as Prom
import Wire.API.Internal.Notification

-- | Observe a push that is stored, with the size of its encoded payload.
observeStored :: (MonadIO m) => NonEmpty NotificationTarget -> Int64 -> m ()
observeStored tgts payloadBytes = liftIO $ do
  -- An empty `targetClients` list means that all clients should be targeted,
  -- non-empty selects the listed.
  let clientCounts = length . (^. targetClients) <$> toList tgts
      someClientCounts = filter (> 0) clientCounts
  Prom.observe pushStoredTargets (fromIntegral (length clientCounts))
  void $ Prom.addCounter pushStoredTargetsAllClients (fromIntegral (length clientCounts - length someClientCounts))
  void $ Prom.addCounter pushStoredTargetsSomeClients (fromIntegral (length someClientCounts))
  traverse_ (Prom.observe pushStoredTargetClients . fromIntegral) someClientCounts
  Prom.observe pushStoredPayloadBytes (fromIntegral payloadBytes)

-- | What one fetch of notifications looked like.
data FetchStats = FetchStats
  { hasClient :: Bool,
    hasSince :: Bool,
    gap :: Bool,
    hasMore :: Bool,
    returned :: Int
  }
  deriving (Eq, Show)

observeFetch :: (MonadIO m) => FetchStats -> m ()
observeFetch s = liftIO $ do
  Prom.incCounter notificationFetch
  unless s.hasClient $ Prom.incCounter notificationFetchWithoutClient
  unless s.hasSince $ Prom.incCounter notificationFetchWithoutSince
  when s.gap $ Prom.incCounter notificationFetchGap
  when s.hasMore $ Prom.incCounter notificationFetchHasMore
  Prom.observe notificationFetchReturned (fromIntegral s.returned)

{-# NOINLINE pushStoredTargets #-}
pushStoredTargets :: Prom.Histogram
pushStoredTargets =
  Prom.unsafeRegister $
    Prom.histogram
      Prom.Info
        { Prom.metricName = "push_stored_targets",
          Prom.metricHelp = "Number of targets (fan-in) of each stored push"
        }
      [1, 2, 3, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000]

{-# NOINLINE pushStoredTargetsAllClients #-}
pushStoredTargetsAllClients :: Prom.Counter
pushStoredTargetsAllClients =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_stored_targets_all_clients",
          Prom.metricHelp = "Number of stored push targets that address all clients of a user"
        }

{-# NOINLINE pushStoredTargetsSomeClients #-}
pushStoredTargetsSomeClients :: Prom.Counter
pushStoredTargetsSomeClients =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_stored_targets_some_clients",
          Prom.metricHelp = "Number of stored push targets that address specific clients of a user"
        }

{-# NOINLINE pushStoredTargetClients #-}
pushStoredTargetClients :: Prom.Histogram
pushStoredTargetClients =
  Prom.unsafeRegister $
    Prom.histogram
      Prom.Info
        { Prom.metricName = "push_stored_target_clients",
          Prom.metricHelp = "Number of clients of each stored push target that addresses specific clients"
        }
      -- `defUserMaxPermClients` is 7, but operators can override this value
      -- with `setUserMaxPermClients`.
      [1, 2, 3, 4, 5, 6, 7, 8, 16, 32]

{-# NOINLINE pushStoredPayloadBytes #-}
pushStoredPayloadBytes :: Prom.Histogram
pushStoredPayloadBytes =
  Prom.unsafeRegister $
    Prom.histogram
      Prom.Info
        { Prom.metricName = "push_stored_payload_bytes",
          Prom.metricHelp = "Size of the encoded payload of each stored push in bytes"
        }
      [128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 262144, 1048576, 5242880]

{-# NOINLINE notificationFetch #-}
notificationFetch :: Prom.Counter
notificationFetch =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "notification_fetch",
          Prom.metricHelp = "Number of notification fetches"
        }

{-# NOINLINE notificationFetchWithoutClient #-}
notificationFetchWithoutClient :: Prom.Counter
notificationFetchWithoutClient =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "notification_fetch_without_client",
          Prom.metricHelp = "Number of notification fetches without a client"
        }

{-# NOINLINE notificationFetchWithoutSince #-}
notificationFetchWithoutSince :: Prom.Counter
notificationFetchWithoutSince =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "notification_fetch_without_since",
          Prom.metricHelp = "Number of notification fetches without a starting notification id"
        }

{-# NOINLINE notificationFetchGap #-}
notificationFetchGap :: Prom.Counter
notificationFetchGap =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "notification_fetch_gap",
          Prom.metricHelp = "Number of notification fetches whose starting notification id was not found"
        }

{-# NOINLINE notificationFetchHasMore #-}
notificationFetchHasMore :: Prom.Counter
notificationFetchHasMore =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "notification_fetch_has_more",
          Prom.metricHelp = "Number of notification fetches that have more notifications to fetch"
        }

{-# NOINLINE notificationFetchReturned #-}
notificationFetchReturned :: Prom.Histogram
notificationFetchReturned =
  Prom.unsafeRegister $
    Prom.histogram
      Prom.Info
        { Prom.metricName = "notification_fetch_returned",
          Prom.metricHelp = "Number of notifications returned by each fetch"
        }
      [0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
