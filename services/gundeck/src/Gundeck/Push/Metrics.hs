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

-- | Metrics about pushes to websocket presences.
module Gundeck.Push.Metrics
  ( monitorBadCannons,
    monitorDelivered,
    pushWsUnreachableCounter,
  )
where

import Data.Set qualified as Set
import Imports
import Prometheus qualified as Prom
import Wire.API.Internal.Notification
import Wire.API.Presence

-- | log all cannons with response status @/= 200@.
monitorBadCannons ::
  (Prom.MonadMonitor m) =>
  (uri, (error, [Presence])) ->
  m ()
monitorBadCannons (_uri, (_err, prcs)) =
  void $ Prom.addCounter pushWsUnreachableCounter (fromIntegral $ length prcs)

-- | Count successful deliveries, and how many of them were for transient notifications.
monitorDelivered :: (Prom.MonadMonitor m) => [(Notification, [Presence])] -> [(NotificationId, Presence)] -> m ()
monitorDelivered notifs successes = do
  void $ Prom.addCounter pushWsDeliveredPresencesCounter (fromIntegral delivered)
  void $ Prom.addCounter pushWsDeliveredPresencesTransientCounter (fromIntegral transient)
  where
    delivered = length successes
    transient = length (filter ((`Set.member` transientIds) . fst) successes)
    transientIds = Set.fromList [ntfId n | (n, _) <- notifs, ntfTransient n]

{-# NOINLINE pushWsDeliveredPresencesCounter #-}
pushWsDeliveredPresencesCounter :: Prom.Counter
pushWsDeliveredPresencesCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_ws_delivered_presences",
          Prom.metricHelp = "Number of websocket presences that were successfully pushed to (including transient pushes)"
        }

{-# NOINLINE pushWsDeliveredPresencesTransientCounter #-}
pushWsDeliveredPresencesTransientCounter :: Prom.Counter
pushWsDeliveredPresencesTransientCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_ws_delivered_presences_transient",
          Prom.metricHelp = "Number of websocket presences that were successfully pushed to with a transient push"
        }

{-# NOINLINE pushWsUnreachableCounter #-}
pushWsUnreachableCounter :: Prom.Counter
pushWsUnreachableCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_ws_unreachable",
          Prom.metricHelp = "Number of times websocket pushes were not pushed due cannon being unreachable"
        }
