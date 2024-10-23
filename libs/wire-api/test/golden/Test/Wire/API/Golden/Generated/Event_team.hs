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

module Test.Wire.API.Golden.Generated.Event_team where

import Control.Lens ((.~), (?~))
import Data.ByteString.Conversion (fromByteString')
import Data.Id (Id (Id))
import Data.Range (unsafeRange)
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Maybe (Just, Nothing), fromJust, read, (&))
import Wire.API.Asset
import Wire.API.Event.Team
import Wire.API.Team
  ( Icon (..),
    TeamBinding (Binding, NonBinding),
    TeamUpdateData
      ( TeamUpdateData,
        _iconKeyUpdate,
        _iconUpdate,
        _nameUpdate,
        _splashScreenUpdate
      ),
    newTeam,
    teamIconKey,
  )
import Wire.API.Team.Permission

testObject_Event_team_1 :: Event
testObject_Event_team_1 =
  newEvent
    (Id (fromJust (UUID.fromString "0000103e-0000-62d6-0000-7840000079b9")))
    (read "1864-05-15 23:16:24.423381912958 UTC")
    ( EdTeamCreate
        ( newTeam
            (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000002")))
            "\EOTX\996492h"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey .~ Nothing
        )
    )

testObject_Event_team_2 :: Event
testObject_Event_team_2 =
  newEvent
    (Id (fromJust (UUID.fromString "000019fb-0000-03a5-0000-009c00006067")))
    (read "1864-05-06 06:03:20.68447167825 UTC")
    ( EdTeamUpdate
        ( TeamUpdateData
            { _nameUpdate =
                Just
                  ( unsafeRange
                      "i5\EOT\1002575\1097973\1066101\&1u\1105430\&1\41840U*/*\999102\1001662\DC3\994167d\1096830\&4uG\173887\fUh09\\\1028574\vPy\t\171003\SI\GS0bV\CAN]\17049\96404\15202\RS\SYNX\ESC3[\CANf\NAK"
                  ),
              _iconUpdate = fromByteString' "3-1-f595b8ed-6dcf-41f2-8a2f-f662a9c0fce4",
              _iconKeyUpdate =
                Just (unsafeRange "\131355Pp\1067299\987603\ENQS\22773S\ACK\NAKmM\19084\&0\19257\31361$rL,XvJ"),
              _splashScreenUpdate = fromByteString' "3-1-6fcf77a2-f58f-464c-a0bd-20fc4aa686ac"
            }
        )
    )

testObject_Event_team_3 :: Event
testObject_Event_team_3 =
  newEvent
    (Id (fromJust (UUID.fromString "00000bfa-0000-53cd-0000-2f8e00004e38")))
    (read "1864-04-20 19:30:43.065358805164 UTC")
    (EdMemberJoin (Id (fromJust (UUID.fromString "000030c1-0000-1c28-0000-71af000036f3"))))

testObject_Event_team_4 :: Event
testObject_Event_team_4 =
  newEvent
    (Id (fromJust (UUID.fromString "000060cd-0000-2fae-0000-3620000011d4")))
    (read "1864-06-07 17:44:20.841616476784 UTC")
    ( EdTeamUpdate
        ( TeamUpdateData
            { _nameUpdate =
                Just
                  ( unsafeRange
                      "d\SI\172132@o\988798s&na\136232\1090952\149487|\83503\1016948/\989099v\NAKu\DC2f\1093640\1011936KC\47338\1066997\1059386\&9_\v_^\1045398K\155463\SO Y*T\CAN\1086598<\1056774>\171907\4929\rt\1038163\1072126w2E\127366hS>\ACK_PQN,Vk\SYN\1083970=90\EM2e\984550\USVA!\EM\FS\EOTe;\189780\&1<H\STX\188937-\ETB8{0\1004484\r\ETB\v~\1060588\EOT!Jg8\US,L\1110751\1075966\1093284\SI\SIkU\143468\1052059\DC3\71711TM{Hzq\r\983328^\ACK'\rUD1etTd4qcW]\154666J|s#(\1003055.\1083560f\10371\DC2\STX)Th\CAN\aI)QvQ\1101685\1017867\160406t'\995737\&5\NAK\145601\1057120e@nS:vB/\135299\173384/\94788\r\1102961*n\RS\917893\1059056\182978lvv53S\990514j\DC4\96540/.!6\1044119.\USg&\NULO,upd$Zz'Y!\"e0\GS\33604\a"
                  ),
              _iconUpdate = fromByteString' "3-1-f595b8ed-6dcf-41f2-8a2f-f662a9c0fce4",
              _iconKeyUpdate =
                Just
                  ( unsafeRange
                      "\SIL\SYN~\DC1'](W\CAN\45506\EOTx\1092062Z\SOH\48440\"\FS=\164314%\25471x$\1011017\1065117Y@\1062986\3941\42139\&7\1057737\1017489|rI\1010932\165452[\RS\tz\DC3r1g\97610~\23897\1065053\&1\r\99706p\14666\191125i3$\1036879#\1001325f'\"\15248TK\ETBJ\ETXo\171892\1079312\67176\1015160\SOpij\189451\1032788?`\182403oR\1086731~vi\27413\&3Mc|\a^\"\14396kK\189875c\1088348\135445oiL\1086249~\ESC*\156657\SI{*,\58564=\173470\131357g\DC3G=\EM|\SYNA~\1057264qZ!\159271\ETBM.kZBV\1031669\DC4\139088\vI{\99861q\RS7\142485B[e\128249x<98{\1006760I?\1035850\135028\145811A\ETB,"
                  ),
              _splashScreenUpdate = Nothing
            }
        )
    )

testObject_Event_team_5 :: Event
testObject_Event_team_5 =
  newEvent
    (Id (fromJust (UUID.fromString "00004a61-0000-6721-0000-393c0000557b")))
    (read "1864-05-09 21:15:29.037488409172 UTC")
    EdTeamDelete

testObject_Event_team_6 :: Event
testObject_Event_team_6 =
  newEvent
    (Id (fromJust (UUID.fromString "00001122-0000-75f2-0000-199f000005de")))
    (read "1864-05-16 06:23:02.245944146361 UTC")
    (EdMemberLeave (Id (fromJust (UUID.fromString "00005828-0000-7c47-0000-28ca00002f72"))))

testObject_Event_team_7 :: Event
testObject_Event_team_7 =
  newEvent
    (Id (fromJust (UUID.fromString "00005ca1-0000-57cd-0000-657100003904")))
    (read "1864-05-12 13:33:11.712478663779 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "00006249-0000-4204-0000-559700001694"))))

testObject_Event_team_8 :: Event
testObject_Event_team_8 =
  newEvent
    (Id (fromJust (UUID.fromString "00003eac-0000-0c8d-0000-4c9400002023")))
    (read "1864-04-19 01:15:18.509437360517 UTC")
    ( EdTeamUpdate
        ( TeamUpdateData
            { _nameUpdate = Nothing,
              _iconUpdate = Nothing,
              _iconKeyUpdate =
                Just
                  ( unsafeRange
                      "t\NUL{1w{\\;\1048307\&6\77982t?H\DC2\50270+,\166489\1020286\1090873\11657p'S\1053650Hv_Q\DC3\FS\138390W\SYN\SOr\NAK3:\ETB\1104464X\142962\65208\ENQy\22451l\SI\1072578\&1Lnu\1109309M\135887,j\a-\6363\DC1D\FSL\ETB\69663{I\DEL,#\t\RS\r\171350\5377iN\DC1\GSV\1012890\NUL\177724\1090396\1075299\v[N\FS\SUB"
                  ),
              _splashScreenUpdate = Nothing
            }
        )
    )

testObject_Event_team_9 :: Event
testObject_Event_team_9 =
  newEvent
    (Id (fromJust (UUID.fromString "000007e2-0000-025d-0000-4e57000052ad")))
    (read "1864-05-05 12:42:00.165920284853 UTC")
    EdTeamDelete

testObject_Event_team_10 :: Event
testObject_Event_team_10 =
  newEvent
    (Id (fromJust (UUID.fromString "00000efc-0000-67f3-0000-33bd00000cc1")))
    (read "1864-06-08 20:37:32.993020874753 UTC")
    (EdMemberLeave (Id (fromJust (UUID.fromString "00004649-0000-6535-0000-5d2b00005924"))))

testObject_Event_team_11 :: Event
testObject_Event_team_11 =
  newEvent
    (Id (fromJust (UUID.fromString "00005156-0000-0690-0000-531500001b8f")))
    (read "1864-06-07 21:49:06.242261128063 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "0000572e-0000-2452-0000-2a8300006d6b"))))

testObject_Event_team_12 :: Event
testObject_Event_team_12 =
  newEvent
    (Id (fromJust (UUID.fromString "00006c75-0000-7a03-0000-2c52000004f3")))
    (read "1864-04-11 07:04:35.939055292667 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "000041d3-0000-6993-0000-080100000fa8"))))

testObject_Event_team_13 :: Event
testObject_Event_team_13 =
  newEvent
    (Id (fromJust (UUID.fromString "000000a2-0000-56a4-0000-1a9f0000402b")))
    (read "1864-04-14 05:25:05.00980826325 UTC")
    ( EdTeamCreate
        ( newTeam
            (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000000")))
            "\b\DELh0\1027380"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ ",7\aS"
        )
    )

testObject_Event_team_14 :: Event
testObject_Event_team_14 =
  newEvent
    (Id (fromJust (UUID.fromString "00006c11-0000-76d2-0000-09da000047d8")))
    (read "1864-05-02 18:02:02.563349061703 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "000071e4-0000-24dd-0000-41dd000013e5"))))

testObject_Event_team_15 :: Event
testObject_Event_team_15 =
  newEvent
    (Id (fromJust (UUID.fromString "00007fe4-0000-5f5d-0000-140500001c24")))
    (read "1864-06-04 00:19:07.663093674023 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "000074e6-0000-1d53-0000-7d6400001363"))))

testObject_Event_team_16 :: Event
testObject_Event_team_16 =
  newEvent
    (Id (fromJust (UUID.fromString "00000ea7-0000-0ab2-0000-36120000290d")))
    (read "1864-04-23 09:55:44.855155072596 UTC")
    (EdConvDelete (Id (fromJust (UUID.fromString "00007c20-0000-6564-0000-046c00004725"))))

testObject_Event_team_17 :: Event
testObject_Event_team_17 =
  newEvent
    (Id (fromJust (UUID.fromString "00006611-0000-7382-0000-5ca500006e9f")))
    (read "1864-05-26 12:52:34.967254218092 UTC")
    (EdConvCreate (Id (fromJust (UUID.fromString "0000713e-0000-6f9d-0000-40e2000036e7"))))

testObject_Event_team_18 :: Event
testObject_Event_team_18 =
  newEvent
    (Id (fromJust (UUID.fromString "00001705-0000-202b-0000-578a000056d0")))
    (read "1864-05-05 05:53:46.446463823554 UTC")
    ( EdMemberUpdate
        (Id (fromJust (UUID.fromString "00007783-0000-7d60-0000-00d30000396e")))
        ( Just
            ( Permissions
                { self =
                    fromList
                      [ CreateConversation,
                        DeleteConversation,
                        AddTeamMember,
                        RemoveTeamMember,
                        AddRemoveConvMember,
                        ModifyConvName,
                        GetBilling,
                        SetBilling,
                        SetTeamData,
                        GetMemberPermissions,
                        SetMemberPermissions,
                        GetTeamConversations,
                        DeleteTeam
                      ],
                  copy =
                    fromList
                      [ CreateConversation,
                        DeleteConversation,
                        AddTeamMember,
                        RemoveTeamMember,
                        AddRemoveConvMember,
                        ModifyConvName,
                        GetBilling,
                        GetMemberPermissions,
                        SetMemberPermissions,
                        DeleteTeam
                      ]
                }
            )
        )
    )

testObject_Event_team_19 :: Event
testObject_Event_team_19 =
  newEvent
    (Id (fromJust (UUID.fromString "00004e8a-0000-7afa-0000-61ad00000f71")))
    (read "1864-05-28 17:18:44.856809552438 UTC")
    ( EdMemberUpdate
        (Id (fromJust (UUID.fromString "0000382c-0000-1ce7-0000-568b00001fe9")))
        ( Just
            ( Permissions
                { self =
                    fromList
                      [ DeleteConversation,
                        RemoveTeamMember,
                        AddRemoveConvMember,
                        ModifyConvName,
                        GetBilling,
                        SetBilling,
                        GetMemberPermissions,
                        GetTeamConversations
                      ],
                  copy =
                    fromList
                      [ DeleteConversation,
                        RemoveTeamMember,
                        AddRemoveConvMember,
                        GetBilling,
                        SetBilling,
                        GetMemberPermissions,
                        GetTeamConversations
                      ]
                }
            )
        )
    )

testObject_Event_team_20 :: Event
testObject_Event_team_20 =
  newEvent
    (Id (fromJust (UUID.fromString "00001872-0000-568f-0000-2ad400004faf")))
    (read "1864-06-02 05:36:57.222646120353 UTC")
    EdTeamDelete
