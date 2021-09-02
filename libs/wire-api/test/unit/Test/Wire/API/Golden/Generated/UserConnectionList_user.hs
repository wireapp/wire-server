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
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Connection
  ( Message (Message, messageText),
    Relation (Accepted, Pending),
    UserConnection
      ( UserConnection,
        ucConvId,
        ucFrom,
        ucLastUpdate,
        ucMessage,
        ucStatus,
        ucTo
      ),
    UserConnectionList (..),
  )

testObject_UserConnectionList_user_1 :: UserConnectionList
testObject_UserConnectionList_user_1 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              ucTo = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              ucStatus = Pending,
              ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-09T06:44:37.367Z"),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\CAN\DEL8\STX\1035092-\1004074z.a&Kho\1002576\1027966}9nF\173234\1056412l\DC4q}\SUB\ETB\1024717m8#\39185A\1099890\1051825}Z\52677\&6r-/D*\USL\1024293\1113794D\1074633\EM\1023212\41115%PZ5\1069612\\}\54494\SI\60922\5521`\1009755'1x\DELNcFhN|*\986465\DLE\t\29903\1024334\&6\DC2/IK\SO\1053777\1017375s\147291\EMBM\60429*a\1036814~j\v#\DC1-D\186618\98625\ENQ7{\189669N\ETBC!~\128543EGS=\ENQ\1000128\SI\1013327\2908b\1018074'K1\NULE\58916\STX\163444J1\NULAI%\53820QV&Z`O\EM~Vu6t\"\1018406_I}a\1031578\&5\1009784Y#\ETB:U\140341\a\1033389\&29^2t\1073180\51546dC\95215\ENQ\EOT\FS,\"5.9;@\b`gl\GSEp5\NAK\EM.VYE_\f\SUB\EOT\FS\ENQbRb;=\1068524\990222a\SOn\NAK1\63452fF"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              ucTo = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              ucStatus = Accepted,
              ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-09T00:43:52.049Z"),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\20301\NAKO\DEL \RSz\133473\GS\991007\140213\CAN8:\1025282bQ(XKw}\f\t\1111001\&0\17916kx\a0=\1005888;L\f*\DLEko\EMz^x2(\1046242\\ep\46530w\63639\183850\&71\n 3\GS\EOT\EOT?z\154271\17377->\128707a,\133082j;-\64635\DC3\993804(L\DC2\999665\t{\1094858\51353<P\EMB\7044T$\1025619\62812\1022653\1047249G\1005846)c\5707\1088238a\"\38644\148359\1094293KS\1093022\&3\13287\EMzO\SI\1023445\31665L@b\94261\nW\ETX\SOH\NAKq\58173\1045716\185704\1069194AK."
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_2 :: UserConnectionList
testObject_UserConnectionList_user_2 = UserConnectionList {clConnections = [], clHasMore = True}
