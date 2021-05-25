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
module Test.Wire.API.Golden.Generated.Invite_user where

import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Conversation (Invite (..))
import Wire.API.Conversation.Role (parseRoleName)

testObject_Invite_user_1 :: Invite
testObject_Invite_user_1 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000002-0000-0058-0000-003c00000079"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                ]
            )
        ),
      invRoleName =
        ( fromJust
            (parseRoleName "t0xs1a2pemtt5f133cklsuqsxvrq25q5awgxjbuf5m2hf679oxxjcop794lmnuj2rd3t1sp5qya0tmn4qhpw2wxepd")
        )
    }

testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000003-0000-0038-0000-000000000030")))])),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "c8wyxwm1r2tphgh4b9yirte060ts3y5tywdisg_uyon1gpkoyahu0cg6e0n0n1d0799qoozn4gdb5nt3ll1cpe7u_rx8vbs9jgk1z8yuw2voczlfnf8a5"
            )
        )
    }

testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000001f-0000-005b-0000-00760000005e")))])),
      invRoleName = (fromJust (parseRoleName "57v38roxp_86gpipmiu"))
    }

testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000034-0000-0024-0000-004b00000050")))])),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "079hquxvhsq317aiqzaqh_xtnlwq2ob5gorilte_6xb7ifqlteayz265glnx4l2nyebn3q41mhd_zaiuqp5v2lg1oel7x1agaten1ejt9smdjbqtzk91bl_ony6m"
            )
        )
    }

testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000074-0000-0065-0000-000c00000019")))])),
      invRoleName = (fromJust (parseRoleName "nepk6pn_rztw8dwdfm"))
    }

testObject_Invite_user_6 :: Invite
testObject_Invite_user_6 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "0000004f-0000-0003-0000-007200000021"))),
                  (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000003"))),
                  (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000007")))
                ]
            )
        ),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "ay8e96nli_o326ctneqxefsl8o06fxn80lsdbfbxceb466ny7tlmq9xk_0eo145ijc0tmazk5ebmpvo91ds1nb5qx_yvecm1eb8prhx2wvic2"
            )
        )
    }

testObject_Invite_user_7 :: Invite
testObject_Invite_user_7 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "0000000d-0000-003e-0000-007d00000011"))),
                  (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000000"))),
                  (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000004"))),
                  (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000003")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "te16fbrbv_frjr7z1cobf_qw3226r09h2ip6"))
    }

testObject_Invite_user_8 :: Invite
testObject_Invite_user_8 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000072-0000-007b-0000-003b00000053"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000100000002"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000300000003")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "ne1ke5cw8eebnhze97"))
    }

testObject_Invite_user_9 :: Invite
testObject_Invite_user_9 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "0000000a-0000-0059-0000-004500000064"))),
                  (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000007"))),
                  (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000001")))
                ]
            )
        ),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "fpi6k1c5xsutublh8en17cdzfeucj3jj0xqydhrniuwh59w9dcousk6qpgmyxfz2yf33nzuvnbfu694jsk6nxo37x7dtmz4jt1qdtpr5ko09seqnrktdh9x2a"
            )
        )
    }

testObject_Invite_user_10 :: Invite
testObject_Invite_user_10 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000000d-0000-006e-0000-003c0000002b")))])),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "kcir_y3vbqzd21zk8mni9ykpdcvv0lgw7yn8hsn9d10a4buteq0dget5f4spyn2dty21rs91bl27eimkuuysbo803zpoi8ujls2hl96_mazcr6zx2iwcd63jcwf"
            )
        )
    }

testObject_Invite_user_11 :: Invite
testObject_Invite_user_11 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000023-0000-0058-0000-005e00000038"))),
                  (Id (fromJust (UUID.fromString "00000055-0000-0062-0000-006a00000054")))
                ]
            )
        ),
      invRoleName =
        (fromJust (parseRoleName "mi2o6aijp5vw63fptcb056791v9b0veom0q072oogofh70yepntd0nihi6bwgle87b1rtzpwrrfpv__"))
    }

testObject_Invite_user_12 :: Invite
testObject_Invite_user_12 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "0000006d-0000-0070-0000-005900000006"))),
                  (Id (fromJust (UUID.fromString "00000078-0000-007d-0000-001c0000007d")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "5fjm"))
    }

testObject_Invite_user_13 :: Invite
testObject_Invite_user_13 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000055-0000-002e-0000-00210000000f"))),
                  (Id (fromJust (UUID.fromString "0000003b-0000-0027-0000-000a00000067")))
                ]
            )
        ),
      invRoleName =
        (fromJust (parseRoleName "47kpu6zrm055rmo2oacehdtninwcsdrzixki1u3s3xyjvqns1g4qpipguuc8t5ttplttmv8zgjozuyy_z"))
    }

testObject_Invite_user_14 :: Invite
testObject_Invite_user_14 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000064-0000-0050-0000-002b00000006"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                ]
            )
        ),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "dez5nywlp2h2um8aygzz7sa4zs6pid50gry3c29_e6yaz3ssycowlty8sqgq8ng25v3g04lpz3yshfjyv6e0y7gw82h317kkmfvh_hr8sf_kt2s"
            )
        )
    }

testObject_Invite_user_15 :: Invite
testObject_Invite_user_15 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000045-0000-0033-0000-003d0000001d"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000005"))),
                  (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000100000005")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "z41w"))
    }

testObject_Invite_user_16 :: Invite
testObject_Invite_user_16 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000043-0000-005b-0000-006100000032"))),
                  (Id (fromJust (UUID.fromString "00000074-0000-0002-0000-004800000046")))
                ]
            )
        ),
      invRoleName =
        ( fromJust
            ( parseRoleName
                "c3a16_lnrlljruno4q6z3qqx01kz1lmkyvuiif0zxrpf5pk0fyg5hpklnhndvdh05blk60mdwcrpy_5a1ng97rq1_a0ukeke5fhbq"
            )
        )
    }

testObject_Invite_user_17 :: Invite
testObject_Invite_user_17 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "0000001c-0000-0067-0000-006400000024"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                  (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                ]
            )
        ),
      invRoleName =
        (fromJust (parseRoleName "sa3ntyz5lxg170v0ayym1u7lcqye396ecvluh4il1yseihpxa_atjcvkotby82epy3brf71ehgqyazwkkr"))
    }

testObject_Invite_user_18 :: Invite
testObject_Invite_user_18 =
  Invite
    { invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000042-0000-0029-0000-005d00000023")))])),
      invRoleName =
        (fromJust (parseRoleName "irsnx2fz8pnkdkf8mbgdb7jresq80op5m3956474u_jm_nq8gcab4sq3jw_ucihknqx081s2zx1t784r7fb1"))
    }

testObject_Invite_user_19 :: Invite
testObject_Invite_user_19 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000054-0000-006e-0000-005900000052"))),
                  (Id (fromJust (UUID.fromString "00000034-0000-0050-0000-00220000001a")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "bt524if4uaj9czwp_1xl72"))
    }

testObject_Invite_user_20 :: Invite
testObject_Invite_user_20 =
  Invite
    { invUsers =
        ( List1
            ( NonEmpty.fromList
                [ (Id (fromJust (UUID.fromString "00000025-0000-0034-0000-007900000003"))),
                  (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000000000000"))),
                  (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000300000001")))
                ]
            )
        ),
      invRoleName = (fromJust (parseRoleName "s2tpmghgh0ydx5rb41fjx9imc9p57hcodqazij95qub"))
    }
