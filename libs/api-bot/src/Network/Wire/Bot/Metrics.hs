{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wire.Bot.Metrics where

import Data.Metrics
import Imports
import Network.Wire.Client.API.Push (EventType, eventTypeText)

assertionsTotal :: Path
assertionsTotal = path "assertions.total"

assertionsFailed :: Path
assertionsFailed = path "assertions.failed"

exceptionsTotal :: Path
exceptionsTotal = path "exceptions.total"

botsCreatedNew :: Path
botsCreatedNew = path "bots.created_new"

botsCreatedCached :: Path
botsCreatedCached = path "bots.created_cached"

botsAlive :: Path
botsAlive = path "bots.alive"

eventsTotalRcvd :: Path
eventsTotalRcvd = path "events.total.received"

eventsTotalAckd :: Path
eventsTotalAckd = path "events.total.acknowledged"

eventsTotalIgnd :: Path
eventsTotalIgnd = path "events.total.ignored"

eventsTotalMssd :: Path
eventsTotalMssd = path "events.total.missed"

eventTypeRcvd :: EventType -> Path
eventTypeRcvd e = path $ "events." <> eventTypeText e <> ".received"

eventTypeAckd :: EventType -> Path
eventTypeAckd e = path $ "events." <> eventTypeText e <> ".acknowledged"

eventTypeIgnd :: EventType -> Path
eventTypeIgnd e = path $ "events." <> eventTypeText e <> ".ignored"

eventTypeMssd :: EventType -> Path
eventTypeMssd e = path $ "events." <> eventTypeText e <> ".missed"
