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
module Test.Wire.API.Golden.Generated.UserIdList_user where

import Data.Domain (Domain (..))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (..))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Event.Conversation (UserIdList (..))

testObject_UserIdList_user_1 :: UserIdList
testObject_UserIdList_user_1 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "0000304a-0000-0d5e-0000-3fac00003993"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003c90-0000-2207-0000-5249000018b1"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000016ee-0000-1c33-0000-6684000050e6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000366d-0000-7f19-0000-4153000039a6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002f85-0000-30dc-0000-4cb700001c44"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000056c8-0000-0828-0000-0a31000012b6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001d2d-0000-74ae-0000-44fc00000eba"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001b2c-0000-651e-0000-12d9000068dd"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006a07-0000-7703-0000-6c1000002889"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001e50-0000-2dd8-0000-0c7a000053f0"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003842-0000-2193-0000-275c00004421"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_2 :: UserIdList
testObject_UserIdList_user_2 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "000065bd-0000-36ec-0000-6d69000056cd"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000017b3-0000-4bb2-0000-70df00006059"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000ef4-0000-64ca-0000-53a2000040ba"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004d4c-0000-595a-0000-7f410000146a"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_3 :: UserIdList
testObject_UserIdList_user_3 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00007725-0000-1cfb-0000-5ccd00005f2b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005045-0000-7682-0000-32cf000006db"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000058aa-0000-2239-0000-246700006d6f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006294-0000-1e40-0000-32a100003817"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_4 :: UserIdList
testObject_UserIdList_user_4 =
  UserIdList {mUsers = [Qualified (Id (fromJust (UUID.fromString "00003024-0000-5b6f-0000-5b5a00000e85"))) (Domain "faraway.example.com")]}

testObject_UserIdList_user_5 :: UserIdList
testObject_UserIdList_user_5 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00007801-0000-01b3-0000-0d2f00005be3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000003ce-0000-1a79-0000-752700005b02"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001f7c-0000-059d-0000-39ee000073cc"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004418-0000-5515-0000-298000006573"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000799e-0000-6e81-0000-653000006f06"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002130-0000-005e-0000-4e7800007786"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000325-0000-28f9-0000-0cf9000001d7"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000644b-0000-32a4-0000-760000003737"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000532-0000-631a-0000-2270000040e8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001158-0000-50f3-0000-064300001f60"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001777-0000-6e74-0000-121400005612"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005a0f-0000-4797-0000-238500005185"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007112-0000-45ce-0000-797000001a8b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006734-0000-45ec-0000-09a3000033e0"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004c31-0000-4fcd-0000-6b570000114a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000044b0-0000-77f3-0000-560800001772"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000452d-0000-5f1d-0000-27d400002f2e"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_6 :: UserIdList
testObject_UserIdList_user_6 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00005e38-0000-026b-0000-71b500006886"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000d99-0000-0db3-0000-2fdb00003e84"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001e6f-0000-0335-0000-779200001e18"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000076f4-0000-5ca9-0000-38c000007caa"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003f84-0000-22f1-0000-13a0000072a0"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000c2a-0000-231b-0000-02db000071ac"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000875-0000-2878-0000-3de200003108"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000041be-0000-3438-0000-4d7c0000794d"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_7 :: UserIdList
testObject_UserIdList_user_7 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00002171-0000-52a2-0000-797e00000c42"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000703d-0000-74d7-0000-22dc00004f28"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006668-0000-7583-0000-5a310000383a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004545-0000-6a1e-0000-50bb00000663"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000029af-0000-4b5b-0000-016100007494"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006ce2-0000-6ff4-0000-41f20000578a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001901-0000-279b-0000-108100002ccf"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000e0a-0000-300a-0000-0d52000076df"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002c4e-0000-6562-0000-227f00001576"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000016c3-0000-26d3-0000-422400003b01"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000031d0-0000-2a7d-0000-132e000010f6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004896-0000-01b7-0000-700e00007564"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004d3c-0000-7dd8-0000-217e00006aef"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000027bc-0000-158f-0000-65d100002c2e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003cf9-0000-7625-0000-199500004ccd"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005d7b-0000-32e6-0000-1cc40000120f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004d0c-0000-4875-0000-1b8600001b22"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007ff8-0000-3356-0000-4910000043cf"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000027c1-0000-1e7a-0000-00e40000144a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005246-0000-6305-0000-41ed000000ae"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006f45-0000-37b9-0000-16be00001949"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006c17-0000-389d-0000-3b5e000038ff"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001dd7-0000-1cf0-0000-7ea700005304"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000042ed-0000-56be-0000-592c00005fbb"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006dc6-0000-5604-0000-5d8f00004873"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000069e4-0000-77dd-0000-4bea000005dd"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006905-0000-4b28-0000-4f8000006bc4"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002b89-0000-6331-0000-2454000078ed"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_8 :: UserIdList
testObject_UserIdList_user_8 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00006018-0000-11b4-0000-5ec9000055b8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003821-0000-3e6a-0000-3aa700004795"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000568c-0000-0356-0000-256800003649"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006785-0000-22d1-0000-28b600005ad4"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004e5a-0000-4a75-0000-7cd9000070f3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004f38-0000-634b-0000-592a000052ce"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000018ef-0000-6096-0000-4c27000077a9"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006b68-0000-1635-0000-00f500005881"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000705a-0000-4cd5-0000-52b800003a1e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003472-0000-10e6-0000-1d090000296f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000067de-0000-6f44-0000-737200006fa5"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000052d9-0000-5da4-0000-1fd500001ed9"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000360d-0000-6b29-0000-077400006824"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_9 :: UserIdList
testObject_UserIdList_user_9 =
  UserIdList {mUsers = [Qualified (Id (fromJust (UUID.fromString "0000570a-0000-5a69-0000-1c9800004362"))) (Domain "faraway.example.com")]}

testObject_UserIdList_user_10 :: UserIdList
testObject_UserIdList_user_10 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00007d3a-0000-274d-0000-60d30000649e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000076b-0000-3498-0000-201000006c19"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001127-0000-360e-0000-200800005676"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004b6f-0000-117f-0000-753a000059af"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004e1e-0000-5c17-0000-2e9b00003c3c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000049f3-0000-357d-0000-08d100007d04"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007911-0000-165f-0000-65cf000042c4"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005806-0000-60de-0000-69a800001c33"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001f13-0000-136d-0000-09c700001d28"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002ad6-0000-0ac3-0000-487300006508"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001a5f-0000-2abd-0000-269b000060c8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000353f-0000-2e6c-0000-2e34000054ed"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001e1c-0000-459c-0000-15e30000794b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000438f-0000-648c-0000-74e80000312c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001066-0000-6ae8-0000-0f6d0000425e"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_11 :: UserIdList
testObject_UserIdList_user_11 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "000065a9-0000-3824-0000-1ed6000057c6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005b8d-0000-1869-0000-680700005032"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000365f-0000-551a-0000-7d0900001d6e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000039c-0000-7b9d-0000-7aa000001451"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002513-0000-3d17-0000-421a00003bfc"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000046d5-0000-732d-0000-59a200006a59"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000014a8-0000-5605-0000-13e900001592"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000c47-0000-33b7-0000-22e800003986"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003535-0000-16cc-0000-3aff000023de"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007306-0000-331a-0000-35b700005dda"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000622d-0000-4ae3-0000-097d00004749"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000079eb-0000-4569-0000-5f6300003edd"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_12 :: UserIdList
testObject_UserIdList_user_12 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00005029-0000-72f0-0000-336b00006f4f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006963-0000-6a5c-0000-6324000004da"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006b5c-0000-0d3a-0000-67ee00004dc1"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000460c-0000-2a56-0000-675700006f01"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_13 :: UserIdList
testObject_UserIdList_user_13 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00007e2c-0000-5526-0000-56800000687c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000016e5-0000-1850-0000-292500002219"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001468-0000-5564-0000-543600003ac1"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006b03-0000-167e-0000-7b8e00002ee5"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000043f5-0000-6b28-0000-0a7c00007696"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000058e0-0000-6cd1-0000-234f0000285e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000063a3-0000-7ec0-0000-3fd8000016ba"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003b25-0000-41cc-0000-1dbd000043c3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002d8e-0000-68eb-0000-6002000054eb"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000010a2-0000-09ce-0000-1aa400001a6c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003d21-0000-21dc-0000-6bff00004d6b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000102e-0000-29ed-0000-1cff00005b6e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002291-0000-26bb-0000-797c000059ac"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003e11-0000-5333-0000-5f6000000c6a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000029c2-0000-7b08-0000-081d000023b2"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000042ac-0000-76e5-0000-2c5d00007bb7"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005cff-0000-7936-0000-718400003158"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000e72-0000-60bd-0000-1bbd000008a8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004c5b-0000-7c3e-0000-613000002c7a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004e00-0000-6f46-0000-241400001912"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000040a6-0000-5656-0000-15c4000060c9"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001763-0000-1497-0000-0f0e0000100a"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_14 :: UserIdList
testObject_UserIdList_user_14 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00005371-0000-333e-0000-046b00003ee8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000066f7-0000-68de-0000-05a40000453a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003195-0000-0b96-0000-688400007308"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_15 :: UserIdList
testObject_UserIdList_user_15 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "0000383c-0000-2fc6-0000-355a00007abe"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006d0d-0000-0165-0000-0350000057e7"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006569-0000-5731-0000-14e600003715"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000063bc-0000-17c0-0000-615500007af1"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000067d2-0000-1718-0000-300900007c08"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004db5-0000-7e5c-0000-40cc00003bdc"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001670-0000-7f3c-0000-31ed00003328"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005de1-0000-248d-0000-5ea800000a69"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003fac-0000-25c3-0000-39400000248e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007b41-0000-5aea-0000-445700006bda"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002087-0000-6b5a-0000-23570000290b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006845-0000-7619-0000-310000001832"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006a49-0000-1378-0000-4e0e000049f5"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006036-0000-7f5e-0000-628400001f05"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001266-0000-3242-0000-194400005728"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000079b9-0000-5069-0000-79830000595f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005496-0000-3751-0000-54f600006784"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000400a-0000-7b4a-0000-559500007ef3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000061e1-0000-4949-0000-34b200006b28"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000005e6-0000-1d9e-0000-1c3300001caf"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005e3b-0000-5b40-0000-01bb00006c1c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007c72-0000-28d6-0000-11e300007b78"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_16 :: UserIdList
testObject_UserIdList_user_16 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "000026c7-0000-0033-0000-2014000031e7"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003cdb-0000-53ee-0000-144200006978"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001249-0000-1c38-0000-18a5000004c8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002679-0000-291d-0000-4ca000007e7d"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004619-0000-7bb1-0000-6c45000075a6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000059cf-0000-3ac0-0000-4894000010d4"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000014cc-0000-22ec-0000-2d550000621a"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003881-0000-564d-0000-6622000055da"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000043d4-0000-72b6-0000-6ae90000353a"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_17 :: UserIdList
testObject_UserIdList_user_17 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00002b7d-0000-5bc9-0000-035000007afb"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000021be-0000-40a5-0000-5db300004d94"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000470a-0000-222a-0000-1568000003e3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002450-0000-39d6-0000-4a67000052a8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007d85-0000-3ef9-0000-2f0500000643"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000052b0-0000-4b58-0000-543a00003878"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001990-0000-31fe-0000-5c93000049b8"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00002581-0000-5a19-0000-4d8f00000e45"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006737-0000-2cce-0000-44d200003bbd"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000cf1-0000-28ff-0000-044b00006008"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007520-0000-7c57-0000-7bad00007dc1"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005377-0000-60ab-0000-04ca00005b16"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000039ec-0000-76ff-0000-6b6c000068c0"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007cf6-0000-6c44-0000-2d1300007bfa"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000618-0000-2eb8-0000-252100006a8b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000504e-0000-2e31-0000-2ea80000515e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000029f7-0000-14ba-0000-31be000077e6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003583-0000-6dfa-0000-0f4b00004456"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000eb3-0000-194b-0000-70a500004525"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003776-0000-5375-0000-178300003d0e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000012bf-0000-2aca-0000-257b00007eae"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00003a60-0000-4129-0000-5d53000038b2"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000057c0-0000-5741-0000-540500006241"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000465b-0000-7f77-0000-4489000011dc"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_18 :: UserIdList
testObject_UserIdList_user_18 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00005eaf-0000-0a0c-0000-708200004f52"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00000a66-0000-0e2f-0000-50bd00000f87"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000076cd-0000-6e88-0000-7770000063f6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000778c-0000-5664-0000-794f0000043b"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007208-0000-3872-0000-02ed00000f4f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005e23-0000-63aa-0000-79ce000057f7"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000070c8-0000-7458-0000-60aa00001369"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000066a6-0000-1ef7-0000-067a00004ffe"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007803-0000-07ad-0000-5b870000060e"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000378c-0000-3f22-0000-18dd00004d2e"))) (Domain "faraway.example.com"))
        ]
    }

testObject_UserIdList_user_19 :: UserIdList
testObject_UserIdList_user_19 =
  UserIdList {mUsers = [Qualified (Id (fromJust (UUID.fromString "00000a5c-0000-1b8a-0000-40540000722c"))) (Domain "faraway.example.com")]}

testObject_UserIdList_user_20 :: UserIdList
testObject_UserIdList_user_20 =
  UserIdList
    { mUsers =
        [ (Qualified (Id (fromJust (UUID.fromString "00002a17-0000-1192-0000-1abc00002c72"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00007465-0000-4fc4-0000-65d800005f03"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000070d0-0000-39dd-0000-77e500002b92"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006ab3-0000-39de-0000-46bb00005b6f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000574d-0000-70b6-0000-4d7f00002f31"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000354b-0000-19be-0000-01a60000559c"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00005874-0000-10bf-0000-2103000005c6"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006ff2-0000-27ae-0000-277300004981"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00004ed4-0000-7160-0000-6c8800000920"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "0000670f-0000-0657-0000-6b4400002b61"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000078e9-0000-1cd5-0000-545c00004e6d"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000072b4-0000-0476-0000-23e900005e9f"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00001462-0000-5092-0000-183800005cf3"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000012f0-0000-7993-0000-787b00003a59"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000046b4-0000-2d69-0000-1d91000065dc"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000040f4-0000-3b05-0000-002800001adf"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "00006feb-0000-20d6-0000-69b700006097"))) (Domain "faraway.example.com")),
          (Qualified (Id (fromJust (UUID.fromString "000013e5-0000-185e-0000-39f300007306"))) (Domain "faraway.example.com"))
        ]
    }
