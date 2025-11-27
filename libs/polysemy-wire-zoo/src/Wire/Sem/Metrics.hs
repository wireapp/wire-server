{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Sem.Metrics where

import Imports
import Polysemy
import Prometheus (Counter, Gauge)

-- | NOTE: Vectors would require non trival changes because
-- 'Prometheus.withLabel' take a paramter of type 'metric -> IO ()'.
data Metrics m a where
  AddCounter :: Counter -> Double -> Metrics m ()
  AddGauge :: Gauge -> Double -> Metrics m ()

makeSem ''Metrics

incCounter :: (Member Metrics r) => Counter -> Sem r ()
incCounter c = addCounter c 1

incGauge :: (Member Metrics r) => Gauge -> Sem r ()
incGauge c = addGauge c 1
