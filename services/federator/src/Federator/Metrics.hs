{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Federator.Metrics
  ( Metrics (..),
    interpretMetrics,
    outgoingCounterIncr,
    incomingCounterIncr,
  )
where

import Control.Lens (view)
import Data.Domain (Domain, domainText)
import Federator.Env
import Imports
import Polysemy
import Polysemy.Input (Input, inputs)
import Prometheus

data Metrics m a where
  OutgoingCounterIncr :: Domain -> Metrics m ()
  IncomingCounterIncr :: Domain -> Metrics m ()

makeSem ''Metrics

interpretMetrics ::
  ( Member (Input Env) r,
    Member (Embed IO) r
  ) =>
  Sem (Metrics ': r) a ->
  Sem r a
interpretMetrics = interpret $ \case
  OutgoingCounterIncr targetDomain -> do
    m <- inputs (view federatorMetrics)
    liftIO $ withLabel m.outgoingRequests (domainText targetDomain) incCounter
  IncomingCounterIncr originDomain -> do
    m <- inputs (view federatorMetrics)
    liftIO $ withLabel m.incomingRequests (domainText originDomain) incCounter
