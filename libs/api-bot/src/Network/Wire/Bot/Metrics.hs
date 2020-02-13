{-# LANGUAGE OverloadedStrings #-}

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
