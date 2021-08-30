{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.UserConnection_user where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Connection
  ( Relation (Accepted, Blocked, Cancelled, Ignored, Pending, Sent),
    UserConnection (..),
  )

testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000002"))),
      ucStatus = Pending,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T21:52:21.955Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000004"))),
      ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000000"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T10:43:38.227Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000004")))
    }

testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),
      ucStatus = Pending,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T19:25:59.201Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000001"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T14:39:50.322Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000")))
    }

testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000000"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000000"))),
      ucStatus = Blocked,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T06:10:28.560Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
    }

testObject_UserConnection_user_6 :: UserConnection
testObject_UserConnection_user_6 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000004"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),
      ucStatus = Sent,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:04:01.926Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000001")))
    }

testObject_UserConnection_user_7 :: UserConnection
testObject_UserConnection_user_7 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000003"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000002"))),
      ucStatus = Pending,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T17:21:11.586Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_8 :: UserConnection
testObject_UserConnection_user_8 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000003"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),
      ucStatus = Accepted,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T21:27:56.433Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000003")))
    }

testObject_UserConnection_user_9 :: UserConnection
testObject_UserConnection_user_9 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),
      ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))),
      ucStatus = Accepted,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T01:59:33.405Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_10 :: UserConnection
testObject_UserConnection_user_10 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000002"))),
      ucStatus = Ignored,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:20:11.508Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000002")))
    }

testObject_UserConnection_user_11 :: UserConnection
testObject_UserConnection_user_11 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000002"))),
      ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000002"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T01:03:59.594Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_12 :: UserConnection
testObject_UserConnection_user_12 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000000"))),
      ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000000"))),
      ucStatus = Sent,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T04:35:47.647Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000003")))
    }

testObject_UserConnection_user_13 :: UserConnection
testObject_UserConnection_user_13 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000001"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:20:36.319Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004")))
    }

testObject_UserConnection_user_14 :: UserConnection
testObject_UserConnection_user_14 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002"))),
      ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000002"))),
      ucStatus = Ignored,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T15:50:31.413Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000003")))
    }

testObject_UserConnection_user_15 :: UserConnection
testObject_UserConnection_user_15 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000003"))),
      ucStatus = Ignored,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T01:23:07.786Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_16 :: UserConnection
testObject_UserConnection_user_16 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),
      ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000000"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T18:59:24.504Z")),
      ucConvId = Nothing
    }

testObject_UserConnection_user_17 :: UserConnection
testObject_UserConnection_user_17 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000003"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),
      ucStatus = Pending,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T23:56:52.951Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000000")))
    }

testObject_UserConnection_user_18 :: UserConnection
testObject_UserConnection_user_18 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000001"))),
      ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000003"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T22:44:27.192Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000000")))
    }

testObject_UserConnection_user_19 :: UserConnection
testObject_UserConnection_user_19 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000"))),
      ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000002"))),
      ucStatus = Accepted,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T10:25:12.601Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000")))
    }

testObject_UserConnection_user_20 :: UserConnection
testObject_UserConnection_user_20 =
  UserConnection
    { ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000004"))),
      ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),
      ucStatus = Cancelled,
      ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T04:26:30.406Z")),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000004")))
    }
