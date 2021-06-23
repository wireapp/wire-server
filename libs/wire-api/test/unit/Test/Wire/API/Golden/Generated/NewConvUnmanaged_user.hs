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
module Test.Wire.API.Golden.Generated.NewConvUnmanaged_user where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified (Qualified (Qualified))
import qualified Data.Set as Set (fromList)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( Access (CodeAccess, InviteAccess, LinkAccess, PrivateAccess),
    AccessRole
      ( ActivatedAccessRole,
        NonActivatedAccessRole,
        PrivateAccessRole,
        TeamAccessRole
      ),
    ConvTeamInfo (ConvTeamInfo, cnvManaged, cnvTeamId),
    NewConv
      ( NewConv,
        newConvAccess,
        newConvAccessRole,
        newConvMessageTimer,
        newConvName,
        newConvQualifiedUsers,
        newConvReceiptMode,
        newConvTeam,
        newConvUsers,
        newConvUsersRole
      ),
    NewConvUnmanaged (..),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Role (parseRoleName)

testDomain :: Domain
testDomain = Domain "testdomain.example.com"

testObject_NewConvUnmanaged_user_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_1 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Nothing,
          newConvAccess = Set.fromList [PrivateAccess, InviteAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 3320987366258987}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = (fromJust (parseRoleName "8tp2gs7b6"))
        }
    )

testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) testDomain],
          newConvName = Just "\128527\1061495",
          newConvAccess = Set.fromList [],
          newConvAccessRole = Just PrivateAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 2406292360203739}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "vmao7psxph3fenvbpsu1u57fns5pfo53d67k98om378rnxr0crcpak_mpspn8q_3m1b02n2n133s1d7q5w3qgmt_5e_dgtvzon8an7dtauiecd32"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just "f",
          newConvAccess = Set.fromList [InviteAccess, LinkAccess, CodeAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 6764297310186120}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "y3otpiwu615lvvccxsq0315jj75jquw01flhtuf49t6mzfurvwe3_sh51f4s257e2x47zo85rif_xyiyfldpan3g4r6zr35rbwnzm0k"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just "\135359\70751z",
          newConvAccess = Set.fromList [CodeAccess],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "q7sqqur0wu2xui3uemxhzds4w3edw4yin7cuukmu7d7l9v9dw181q7wugi7q87lzzw405pkphgit2g969hqb4n9kcvm0eg0pems55xdfqyhxbe948vhof"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "X9",
          newConvAccess = Set.fromList [InviteAccess, LinkAccess],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Nothing,
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "cr2g48i6xjo49qdm04jig5teset_g6kt14u9az9jj5xhxoic55pown5d_rkw_3mrevrm37fosq08fhlsq8l259aio80f6cio"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_6 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_6 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just "`3",
          newConvAccess = Set.fromList [LinkAccess],
          newConvAccessRole = Just TeamAccessRole,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 3993332602038581}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}),
          newConvUsersRole = (fromJust (parseRoleName "5zlsxm_95e5j1lk04d6rka_1svnnk65pov7tqs"))
        }
    )

testObject_NewConvUnmanaged_user_7 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_7 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000004")))],
          newConvQualifiedUsers = [],
          newConvName = Just "\1038759\b\1057989'",
          newConvAccess = Set.fromList [],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 5300164242243961}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -3}),
          newConvUsersRole = (fromJust (parseRoleName "hdvd1wsqebgfamlgxdaoq7or2__7_dg5xg53v3ur9en91guk"))
        }
    )

testObject_NewConvUnmanaged_user_8 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_8 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))],
          newConvQualifiedUsers = [],
          newConvName = Just "",
          newConvAccess = Set.fromList [InviteAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 5317293791913533}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "eyywm70536valjr5fwpiodgan70f9bw21os6a9q965y_hpww2hirwfm4lbe6220ltzpb8lifi2kd1q2w4qtq5t6bhzctw27b4k09offys"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_9 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_9 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "L",
          newConvAccess = Set.fromList [PrivateAccess, InviteAccess, LinkAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 7179840365742041}),
          newConvReceiptMode = Nothing,
          newConvUsersRole = (fromJust (parseRoleName "n8cjajmyhnw3hqv8sohb8674nwnpsv7g57i2hjhexg9tww"))
        }
    )

testObject_NewConvUnmanaged_user_10 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_10 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "",
          newConvAccess = Set.fromList [PrivateAccess, CodeAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 5041503034744095}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}),
          newConvUsersRole =
            ( fromJust
                (parseRoleName "30mnzwj79jo9ear300qs4k_x2262nyaqxt9qga1_zaqmto43q2935t4dzaan_qnlstgjix7efmqfljkpww2lz")
            )
        }
    )

testObject_NewConvUnmanaged_user_11 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_11 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Nothing,
          newConvAccess = Set.fromList [],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 6019134025424754}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = (fromJust (parseRoleName "1ewdfj36vw"))
        }
    )

testObject_NewConvUnmanaged_user_12 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_12 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just ">+",
          newConvAccess = Set.fromList [],
          newConvAccessRole = Nothing,
          newConvTeam = Nothing,
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = (fromJust (parseRoleName "wqhaeljk9zpp5nmspwl"))
        }
    )

testObject_NewConvUnmanaged_user_13 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_13 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just ".L'",
          newConvAccess = Set.fromList [],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 211460552735402}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "40iudwo9123uutd1ppbq2sd5aybain45r_mdb4caukkc6vvu4xdivyg23jl5vigsbq4q8zm4ua9yly3mxygytnv8wuf9__550amkunox7fpxw03b_y_lm86cahubkq"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_14 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_14 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "",
          newConvAccess = Set.fromList [CodeAccess],
          newConvAccessRole = Just NonActivatedAccessRole,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 854777662274030}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
          newConvUsersRole = (fromJust (parseRoleName "fga_hfm9uzn_5z883y6r_kumb"))
        }
    )

testObject_NewConvUnmanaged_user_15 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_15 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000003")))],
          newConvQualifiedUsers = [],
          newConvName = Just "b\1008988",
          newConvAccess = Set.fromList [PrivateAccess, InviteAccess, CodeAccess],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 4005602882980532}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = (fromJust (parseRoleName "_wje4g3_kcquzyoms0q4cwzz8"))
        }
    )

testObject_NewConvUnmanaged_user_16 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_16 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just "!",
          newConvAccess = Set.fromList [PrivateAccess, CodeAccess],
          newConvAccessRole = Just PrivateAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = (fromJust (parseRoleName "04ukg5i2nomsgwiphznmsrk1ou3ukxemisi9g"))
        }
    )

testObject_NewConvUnmanaged_user_17 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_17 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "\ETXB\119338",
          newConvAccess = Set.fromList [PrivateAccess, LinkAccess, CodeAccess],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 880163555151907}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}),
          newConvUsersRole =
            ( fromJust
                ( parseRoleName
                    "88mkdi8ivd_o3150rhc9dc7gyf_246m7xjrqwz7kt9vc7h5sgkuukgorx26y6uo7hj2lfe63pkeyva9tfivn08amsydb_i5vb4xn4870v44y0cwe3uk6sli5kqg"
                )
            )
        }
    )

testObject_NewConvUnmanaged_user_18 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_18 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            ],
          newConvQualifiedUsers = [],
          newConvName = Just "sd\ACK",
          newConvAccess = Set.fromList [PrivateAccess, CodeAccess],
          newConvAccessRole = Nothing,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 3120553871655858}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
          newConvUsersRole =
            (fromJust (parseRoleName "ugehcdyu_ob9_woawlths95ez8cgtb6wjqypp7vbjaooiczerb5zpc6srxszgkrdu8l24ygz_"))
        }
    )

testObject_NewConvUnmanaged_user_19 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_19 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002")))],
          newConvQualifiedUsers = [],
          newConvName = Just "Cu\DC1",
          newConvAccess = Set.fromList [InviteAccess, LinkAccess],
          newConvAccessRole = Nothing,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 864918593306344}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
          newConvUsersRole =
            (fromJust (parseRoleName "xik7vc3wp82gw4r934rad_bhmf2orany3qgu_tx9huwfrlxy8m0id71x20uddebps30zdahe_ffcxxhc"))
        }
    )

testObject_NewConvUnmanaged_user_20 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_20 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Just "\SI\1070774",
          newConvAccess = Set.fromList [PrivateAccess],
          newConvAccessRole = Nothing,
          newConvTeam = Nothing,
          newConvMessageTimer = Just (Ms {ms = 3641984282941906}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
          newConvUsersRole = (fromJust (parseRoleName "udhi2sbf7tzyshrh"))
        }
    )

testObject_NewConvUnmanaged_user_21 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_21 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) testDomain],
          newConvName = Nothing,
          newConvAccess = Set.fromList [],
          newConvAccessRole = Nothing,
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Nothing,
          newConvTeam = Nothing,
          newConvUsersRole = fromJust (parseRoleName "udhi2sbf7tzyshrh")
        }
    )
