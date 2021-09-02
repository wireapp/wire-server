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
  ( Message (Message, messageText),
    Relation (Cancelled, Pending),
    UserConnection (..),
  )

testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 =
  UserConnection
    { ucFrom = Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001")),
      ucTo = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000002")),
      ucStatus = Pending,
      ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-07T21:52:21.955Z"),
      ucMessage = Nothing,
      ucConvId = Nothing
    }

testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 =
  UserConnection
    { ucFrom = Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000004")),
      ucTo = Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000000")),
      ucStatus = Cancelled,
      ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-11T10:43:38.227Z"),
      ucMessage =
        Just
          ( Message
              { messageText =
                  " D\ETB*\1111732\1045722\ACK&*`\bR\12652\177451\FS\143803\25129\EM\ENQ\n-yc&t\t3E\27186:\170285t\DEL\DC11\STXN\EOT\EM$:\"\EM\EM\SUB\162168\46118y\SOHe|\DLE\156824o[\78645<56Tm\146830&3\50454!x<y\"\161399|m88\159184N{\STX9\1038504kK\ESCa\NULr\EM\1007066+h\EOT(L\GS\bdD\ACKu\992141\\\21510\59850\156479K`9;\ESC\bx!\DEL/K,\SOH\26358\DC3\n/\1096058uU\ACK{\992363+qA\1072067z\ACK\SIf|s(\18260~^\171221P\STX*U\68767\15049J~\US\1111717i\9220\EM.\CAN\1001863\1080354Kl|B\ETX\1018680d\ESCg.6n \24343\NAK\111031s=\1065416J`#\1060577\78510{\159244h`\1072205O\NAK\1085355~\7378\139390t2t\78696B2\t\20659\&4"
              }
          ),
      ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000004")))
    }

testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 =
  UserConnection
    { ucFrom = Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003")),
      ucTo = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001")),
      ucStatus = Pending,
      ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-07T19:25:59.201Z"),
      ucMessage =
        Just
          ( Message
              { messageText =
                  "=\150784N5\r@NT1d\1092153\167388\99461\1111339\127861\995254\NUL\1046197\n\1048626aSH\SOHB\ETBc\US\50502[E\160938\48230\n\178369\r\ENQ\43663\24872H\NUL`\1009984\9485\173731_:\v}\\|\1408\29765\CANm\EOT\STX\120550\b\SOH0\31634p'ap\DEL\vwo9\"z|hVZ\rQBV\ETX@\FSA+\182717C\1011224T\73008\EOTs\SYNP\37375\984849h\999741@<5\1054596Ql\164052\179986\EM\36999\t\SIV-\DC2@\17401\USwi\27376)\ETB\1082133\r\vYt\1077868OZ\1062261\36015\FSp\bPS\DELly3X\ESC\RSU\131841(\99574t\35738\NAK\8694`"
              }
          ),
      ucConvId = Nothing
    }

testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 =
  UserConnection
    { ucFrom = Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001")),
      ucTo = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000001")),
      ucStatus = Cancelled,
      ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-08T14:39:50.322Z"),
      ucMessage = Nothing,
      ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000")))
    }
