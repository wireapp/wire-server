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

module Test.Wire.API.Golden.Generated.TeamConversationList_team where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
import Wire.API.Team.Conversation (TeamConversationList, newTeamConversation, newTeamConversationList)

testObject_TeamConversationList_team_1 :: TeamConversationList
testObject_TeamConversationList_team_1 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0018-0000-00260000002b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0063-0000-006900000013"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-003c-0000-00440000000e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000067-0000-003a-0000-006100000049"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-0003-0000-005a00000075"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007f-0000-0018-0000-00250000007c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006a-0000-0020-0000-001a00000073"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-006a-0000-005f00000003"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-0021-0000-00330000005b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000048-0000-0011-0000-002a00000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-0018-0000-00060000001a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-000e-0000-004300000028"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000067-0000-007f-0000-003600000031"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-0053-0000-006a00000034"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000f-0000-0071-0000-001b00000057"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-0035-0000-00210000003b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-000d-0000-002100000067")))))
      ]
  )

testObject_TeamConversationList_team_2 :: TeamConversationList
testObject_TeamConversationList_team_2 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-0045-0000-007d00000023"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000d-0000-0080-0000-00550000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-0053-0000-004600000056"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006e-0000-003c-0000-003200000071"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000067-0000-002f-0000-007a0000007f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000a-0000-0027-0000-004e0000005f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0026-0000-000000000054"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006e-0000-007e-0000-001600000035"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002c-0000-0057-0000-007e00000070"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-0053-0000-005f00000006"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-005c-0000-00050000006b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-0061-0000-004a00000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005a-0000-007b-0000-000800000033"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000027-0000-0043-0000-006800000068"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0018-0000-003f00000001"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000014-0000-0066-0000-00440000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007f-0000-0071-0000-007f0000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-004d-0000-005000000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-003e-0000-00140000006e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-005c-0000-001e0000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004d-0000-0021-0000-00360000000e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-003f-0000-003700000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-003e-0000-000300000051"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0025-0000-00030000003b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003c-0000-0069-0000-005000000035"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-006b-0000-00260000004e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001c-0000-001c-0000-00530000000c")))))
      ]
  )

testObject_TeamConversationList_team_3 :: TeamConversationList
testObject_TeamConversationList_team_3 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0026-0000-005600000014"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0042-0000-002c00000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-006d-0000-006100000027"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000079-0000-0024-0000-004600000011"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000044-0000-0005-0000-003800000008"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-005e-0000-00200000001a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0038-0000-001b00000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000029-0000-0045-0000-004500000078"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001e-0000-0036-0000-006400000045"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000050-0000-0066-0000-000500000075")))))
      ]
  )

testObject_TeamConversationList_team_4 :: TeamConversationList
testObject_TeamConversationList_team_4 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000076-0000-0038-0000-003c00000043"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-001f-0000-005800000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0070-0000-006f00000077"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0031-0000-004700000053"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0041-0000-001600000013"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007b-0000-003c-0000-004800000063"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-0009-0000-004c00000009"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-007b-0000-00460000007f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-002e-0000-001000000064"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003d-0000-002a-0000-00290000007b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0033-0000-00780000005e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-007f-0000-001d0000002c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000017-0000-0079-0000-001c00000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-0024-0000-001000000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000010-0000-000c-0000-001700000046"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0049-0000-003100000022"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000011-0000-0051-0000-003300000061"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003e-0000-0077-0000-004c00000022"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007e-0000-0048-0000-007200000056"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-0007-0000-00190000004f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0048-0000-001c0000007e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004c-0000-0071-0000-007a00000071"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-002000000068"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-0037-0000-005e00000027"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-006d-0000-004d00000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004a-0000-0038-0000-001e0000003b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-001a-0000-004a0000001a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-0070-0000-007000000019"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0013-0000-004a00000018")))))
      ]
  )

testObject_TeamConversationList_team_5 :: TeamConversationList
testObject_TeamConversationList_team_5 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000073-0000-005a-0000-00250000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-005c-0000-006e00000014"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000017-0000-005d-0000-003b00000023")))))
      ]
  )

testObject_TeamConversationList_team_6 :: TeamConversationList
testObject_TeamConversationList_team_6 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-007f-0000-00730000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-0037-0000-000b00000016"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000021-0000-0064-0000-003900000002"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-001f-0000-00350000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-007b-0000-00770000003e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-0068-0000-007700000068"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000061-0000-000b-0000-00170000005c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005c-0000-0001-0000-004e00000003"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000008-0000-002b-0000-002d00000022")))))
      ]
  )

testObject_TeamConversationList_team_7 :: TeamConversationList
testObject_TeamConversationList_team_7 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0010-0000-002700000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-0036-0000-000e00000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0068-0000-000000000006"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-0018-0000-005d00000050"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000040-0000-0001-0000-00670000002e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-0016-0000-004300000052"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007b-0000-0073-0000-002700000048"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003b-0000-0048-0000-002500000015"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-007c-0000-001500000051")))))
      ]
  )

testObject_TeamConversationList_team_8 :: TeamConversationList
testObject_TeamConversationList_team_8 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000026-0000-0066-0000-00170000007b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-0015-0000-001f00000071"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000063-0000-0049-0000-004100000018"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000050-0000-002b-0000-000300000001"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-006e-0000-002f00000057"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-0064-0000-003b0000002d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003e-0000-0009-0000-00630000001d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-004d-0000-001b00000036"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0073-0000-007d00000010"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000016-0000-0007-0000-00690000002d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-001f-0000-007500000002"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-0012-0000-006200000028"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000019-0000-003a-0000-002300000023"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000050-0000-006d-0000-00610000000c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0048-0000-003200000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0024-0000-002000000015"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000027-0000-0003-0000-007600000028"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-005d-0000-00100000005d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000071-0000-0075-0000-000a0000002c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0071-0000-004d00000010"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-003f-0000-005a00000026"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000016-0000-0069-0000-00500000000a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-000b-0000-003000000046"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-005f-0000-007f0000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0050-0000-002100000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000077-0000-0063-0000-00360000000e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000058-0000-0011-0000-001200000005"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004a-0000-0037-0000-003000000034"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000029-0000-0043-0000-006700000030"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000039-0000-003e-0000-008000000051")))))
      ]
  )

testObject_TeamConversationList_team_9 :: TeamConversationList
testObject_TeamConversationList_team_9 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000058-0000-007c-0000-002a0000005f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-0009-0000-006500000038"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-000a-0000-004e00000039"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000062-0000-001e-0000-004c00000058"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0021-0000-00670000000a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004f-0000-0063-0000-004a0000004b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000044-0000-0017-0000-006300000067"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006a-0000-0070-0000-002e0000000a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-0080-0000-006000000025"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007d-0000-0040-0000-001700000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0045-0000-00610000006c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-0042-0000-005b00000057"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000048-0000-0032-0000-000000000069"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0022-0000-00370000005b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000007-0000-0068-0000-00150000001f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003a-0000-0067-0000-00060000003e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001e-0000-0043-0000-002800000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-001f-0000-001700000006"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0024-0000-004900000037"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-0019-0000-00670000005c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000029-0000-0003-0000-00520000004c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-002f-0000-002b0000006f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000021-0000-002e-0000-004f0000005e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006a-0000-0023-0000-00560000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-007b-0000-00160000005c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-0008-0000-006b00000049"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-0020-0000-005000000006"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-0038-0000-003400000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000067-0000-006f-0000-00370000002e")))))
      ]
  )

testObject_TeamConversationList_team_10 :: TeamConversationList
testObject_TeamConversationList_team_10 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-007d-0000-001400000009"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-0057-0000-00190000004a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-0030-0000-006b00000005"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-0065-0000-001100000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0039-0000-000400000071"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003e-0000-0053-0000-007f0000003c")))))
      ]
  )

testObject_TeamConversationList_team_11 :: TeamConversationList
testObject_TeamConversationList_team_11 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0030-0000-006700000067"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-006a-0000-00220000007c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-004f-0000-005500000047"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-003d-0000-006500000060")))))
      ]
  )

testObject_TeamConversationList_team_12 :: TeamConversationList
testObject_TeamConversationList_team_12 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0042-0000-00120000004e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000010-0000-002b-0000-002600000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0054-0000-005300000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000e-0000-006f-0000-000c00000038"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0021-0000-005500000008"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-007a-0000-00230000002d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000078-0000-000e-0000-004300000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0003-0000-000500000011"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-0032-0000-005200000069"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000c-0000-0003-0000-001400000018"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-0020-0000-005200000053"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-007b-0000-00670000000b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-005b-0000-00250000000c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-005b-0000-004200000001"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0073-0000-003d00000006"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-0038-0000-006600000048"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000d-0000-0022-0000-00800000006f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005e-0000-0023-0000-000700000012"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0071-0000-005f00000070"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0024-0000-003400000018"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000054-0000-0056-0000-007000000058"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0011-0000-001500000007")))))
      ]
  )

testObject_TeamConversationList_team_13 :: TeamConversationList
testObject_TeamConversationList_team_13 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "0000006a-0000-0043-0000-007f00000048"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007d-0000-005f-0000-000a00000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-0046-0000-003800000023"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-006b-0000-002000000068"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000041-0000-0000-0000-007000000005"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-0075-0000-00200000007a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0023-0000-001a00000022"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-004f-0000-000400000072"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-001a-0000-00680000004d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-0037-0000-00020000000f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0040-0000-005b0000001c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-0074-0000-007b00000019"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-0025-0000-006900000014"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000063-0000-0000-0000-002100000043"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-0018-0000-004d0000003a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-004e-0000-002700000075"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0014-0000-000100000040"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-0004-0000-00280000000a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0012-0000-00150000006e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-003c-0000-006400000055"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000008-0000-003d-0000-003c00000003")))))
      ]
  )

testObject_TeamConversationList_team_14 :: TeamConversationList
testObject_TeamConversationList_team_14 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000039-0000-005c-0000-000e00000044"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-0061-0000-005d00000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000011-0000-0009-0000-006c00000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-0026-0000-001e00000007"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-005e-0000-007300000058"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-006a-0000-004100000045"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-0027-0000-00080000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-0028-0000-007700000051"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-001c-0000-004c00000073"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-002f-0000-003400000023"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005d-0000-0057-0000-00580000006a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-0016-0000-002500000036"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-006c-0000-00420000003d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000008-0000-005d-0000-004600000002"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006a-0000-002b-0000-005800000035"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006e-0000-0007-0000-005800000075"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-002b-0000-000100000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-001b-0000-003200000000"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0013-0000-004d0000006e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-0041-0000-007200000079")))))
      ]
  )

testObject_TeamConversationList_team_15 :: TeamConversationList
testObject_TeamConversationList_team_15 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-0013-0000-006400000036"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-007e-0000-002f00000057"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-006e-0000-006800000040"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-005a-0000-000e00000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000069-0000-007c-0000-00550000002f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0041-0000-000e0000003e")))))
      ]
  )

testObject_TeamConversationList_team_16 :: TeamConversationList
testObject_TeamConversationList_team_16 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0066-0000-003800000061"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-0007-0000-003f0000001d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-0038-0000-005f00000072"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-0069-0000-005b00000011"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-0073-0000-00280000005d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0068-0000-004f00000042"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0056-0000-00780000000f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0064-0000-001b00000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-0052-0000-004000000072"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-0080-0000-005100000029"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000079-0000-0018-0000-000600000047"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0029-0000-003100000043"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000048-0000-002e-0000-00220000005b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003b-0000-004d-0000-001700000055"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000006c-0000-0028-0000-002100000076"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000033-0000-0052-0000-003300000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004c-0000-005f-0000-00390000004d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-004b-0000-00440000003e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-007a-0000-003d00000036"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-0058-0000-003700000019"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-0011-0000-007c00000011"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0057-0000-00630000002b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000051-0000-0018-0000-00590000007a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-0011-0000-002100000014"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000060-0000-0003-0000-00490000001b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-006e-0000-001e0000001a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005d-0000-0065-0000-004b00000045")))))
      ]
  )

testObject_TeamConversationList_team_17 :: TeamConversationList
testObject_TeamConversationList_team_17 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-0070-0000-007f0000001c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0017-0000-002a00000076"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-004f-0000-00710000002d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000067-0000-0037-0000-004d0000007b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0071-0000-000800000015"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-0062-0000-002900000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000072-0000-0027-0000-001300000046"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000f-0000-0034-0000-00720000000f"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000021-0000-005d-0000-003300000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-000b-0000-00160000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000022-0000-0042-0000-003400000043"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000020-0000-0033-0000-00780000006b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-0067-0000-005f00000042"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000f-0000-0079-0000-00630000007e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-0045-0000-003900000053"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-003e-0000-003d00000000"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000039-0000-0052-0000-000500000034"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-002d-0000-00030000005c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0067-0000-007400000054"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-0075-0000-001200000054"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-003d-0000-000700000080"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005d-0000-0006-0000-00010000001a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0073-0000-002000000058"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000073-0000-0015-0000-005e0000006e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-0019-0000-00510000005a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-0074-0000-007000000021"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-0040-0000-006f00000075")))))
      ]
  )

testObject_TeamConversationList_team_18 :: TeamConversationList
testObject_TeamConversationList_team_18 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-000d-0000-007600000068"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-0033-0000-006400000019"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-0075-0000-00400000004e"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000062-0000-0073-0000-002a00000051"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003b-0000-004b-0000-005c00000064"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000016-0000-001a-0000-00430000003d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-0005-0000-004f00000031"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-0043-0000-001a0000000c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-001c-0000-003a0000002b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001c-0000-007b-0000-00170000000a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000073-0000-0073-0000-000000000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-0069-0000-00490000002d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003c-0000-0012-0000-000400000000"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000016-0000-004e-0000-003800000057"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000008-0000-0022-0000-002000000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-0011-0000-00260000004a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002c-0000-007a-0000-00340000006e")))))
      ]
  )

testObject_TeamConversationList_team_19 :: TeamConversationList
testObject_TeamConversationList_team_19 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-0041-0000-007b00000060"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003f-0000-0059-0000-000700000073"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-0056-0000-007e00000066"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002b-0000-000b-0000-007a00000065"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000071-0000-003a-0000-001b00000027"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-004f-0000-008000000008"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003c-0000-000d-0000-00510000005a"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-006e-0000-004200000072"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001b-0000-003b-0000-007900000004"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0077-0000-006400000054"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-005e-0000-003e00000012"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-000c-0000-00370000003b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-0010-0000-006500000077"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-004b-0000-00460000007b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-0040-0000-006400000024"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-005b-0000-002d00000031"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-0067-0000-00610000006d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000007f-0000-0036-0000-00770000000d"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000058-0000-0042-0000-003700000054"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-0001-0000-000700000015"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-003c-0000-003b00000000"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-0049-0000-00720000006c"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000f-0000-0021-0000-004c00000055"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-002e-0000-00140000003d")))))
      ]
  )

testObject_TeamConversationList_team_20 :: TeamConversationList
testObject_TeamConversationList_team_20 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000007-0000-0017-0000-007500000074"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000003b-0000-0055-0000-003f00000059")))))
      ]
  )
