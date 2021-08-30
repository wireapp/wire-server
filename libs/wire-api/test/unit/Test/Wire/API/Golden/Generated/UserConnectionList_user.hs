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
module Test.Wire.API.Golden.Generated.UserConnectionList_user where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (..), Maybe (..), fromJust)
import Wire.API.Connection
  ( Relation (..),
    UserConnection (..),
    UserConnectionList (..),
  )

testObject_UserConnectionList_user_1 :: UserConnectionList
testObject_UserConnectionList_user_1 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:44:37.367Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:43:52.049Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:40:27.247Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:29:04.518Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:26:54.689Z")),
              ucConvId = Nothing
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_2 :: UserConnectionList
testObject_UserConnectionList_user_2 = UserConnectionList {clConnections = [], clHasMore = True}

testObject_UserConnectionList_user_3 :: UserConnectionList
testObject_UserConnectionList_user_3 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T02:11:50.603Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T18:09:42.397Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_4 :: UserConnectionList
testObject_UserConnectionList_user_4 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T10:45:56.744Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T09:59:06.024Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_5 :: UserConnectionList
testObject_UserConnectionList_user_5 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:48:55.532Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:06:01.784Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:03:25.908Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:10:46.357Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:25:25.817Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:12:01.153Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:01:44.742Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:15:38.478Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:21:07.732Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:01:49.115Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:47:40.390Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:21:31.150Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:13:33.637Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:43:15.087Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:30:28.519Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T10:24:31.073Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:15:55.011Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:54:32.384Z")),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:12:52.624Z")),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            }
        ],
      clHasMore = False
    }
