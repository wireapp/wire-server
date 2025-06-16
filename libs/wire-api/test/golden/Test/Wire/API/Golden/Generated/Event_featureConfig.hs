{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.Event_featureConfig where

import Data.Aeson
import Data.Id (Id (Id))
import Data.UUID qualified as UUID
import GHC.Exts (IsList (fromList))
import Imports
import Wire.API.Event.FeatureConfig

testObject_Event_featureConfig_1 :: Event
testObject_Event_featureConfig_1 = Event {_eventType = Update, _eventFeatureName = "", _eventData = Object (fromList [("config", Object (fromList [("domains", Array [])])), ("lockStatus", String "unlocked"), ("status", String "enabled"), ("ttl", String "unlimited")]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_2 :: Event
testObject_Event_featureConfig_2 = Event {_eventType = Update, _eventFeatureName = "\DLE", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", String "unlimited")]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_3 :: Event
testObject_Event_featureConfig_3 = Event {_eventType = Update, _eventFeatureName = "4\988540%\ETX", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", String "unlimited")]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_4 :: Event
testObject_Event_featureConfig_4 = Event {_eventType = Update, _eventFeatureName = "n(\1041492>\b", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", Number 6.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_5 :: Event
testObject_Event_featureConfig_5 = Event {_eventType = Update, _eventFeatureName = "\1002596T\n\1092227X", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", String "unlimited")]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_6 :: Event
testObject_Event_featureConfig_6 = Event {_eventType = Update, _eventFeatureName = "\1039478\1022562)TXC\52414\174655K", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "enabled"), ("ttl", Number 7.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_7 :: Event
testObject_Event_featureConfig_7 = Event {_eventType = Update, _eventFeatureName = "\DEL+\1070185\190816\&9\52178\nW", _eventData = Object (fromList [("lockStatus", String "unlocked"), ("status", String "enabled"), ("ttl", Number 9.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_8 :: Event
testObject_Event_featureConfig_8 = Event {_eventType = Update, _eventFeatureName = "7\DLEq-w\11345\DLE B\1028119H\n\DC2R\b", _eventData = Object (fromList [("config", Object (fromList [("enforcedTimeoutSeconds", Number 32.0)])), ("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", Number 3.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_9 :: Event
testObject_Event_featureConfig_9 = Event {_eventType = Update, _eventFeatureName = "\18889\20273d\1004321r\GSd'L\63854\SUB,\26907\SOH{@", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "disabled"), ("ttl", Number 16.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}

testObject_Event_featureConfig_10 :: Event
testObject_Event_featureConfig_10 = Event {_eventType = Update, _eventFeatureName = "\1105858'\1002071\&1S=\1058029u\1103881\10729:}\SUB#f<z\97563", _eventData = Object (fromList [("lockStatus", String "locked"), ("status", String "enabled"), ("ttl", Number 3.0)]), _eventTeam = Id (fromJust $ UUID.fromString "c5b9b5bc-eda1-49fa-810b-4a253ada5e70")}
