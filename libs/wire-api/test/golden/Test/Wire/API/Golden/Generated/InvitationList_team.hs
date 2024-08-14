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

module Test.Wire.API.Golden.Generated.InvitationList_team where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust, fromRight')
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.Team.Invitation
import Wire.API.Team.Role (Role (RoleAdmin, RoleExternalPartner, RoleMember, RoleOwner))
import Wire.API.User.Identity
import Wire.API.User.Profile (Name (Name, fromName))

testObject_InvitationList_team_1 :: InvitationList
testObject_InvitationList_team_1 = InvitationList {ilInvitations = [], ilHasMore = False}

testObject_InvitationList_team_2 :: InvitationList
testObject_InvitationList_team_2 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T09:28:36.729Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "\153442" "w",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "fuC9p\1098501A\163554\f\ENQ\SO\21027N\47326_?oCX.U\r\163744W\33096\58996\1038685\DC3\t[\37667\SYN/\8408A\145025\173325\DC4H\135001\STX\166880\EOT\165028o\DC3"
                      }
                  ),
              inInviteeUrl = Just (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_3 :: InvitationList
testObject_InvitationList_team_3 = InvitationList {ilInvitations = [], ilHasMore = False}

testObject_InvitationList_team_4 :: InvitationList
testObject_InvitationList_team_4 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T19:46:50.121Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "R6\133444\134053VQ\187682\SUB\SOH\180538\&0C\1088909\ESCR\185800\125002@\38857Z?\STX\169387\1067878e}\SOH\ETB\EOTm\184898\US]\986782\189015\1059374\986508\b\DC1zfw-5\120662\CAN\1064450 \EMe\DC4|\14426Vo{\1076439\DC3#\USS\45051&zz\160719\&9\142411,\SI\f\SOHp\1025840\DLE\163178\1060369.&\997544kZ\50431u\b\50764\1109279n:\1103691D$.Q"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T09:00:02.901Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\DC2}q\CAN=SA\ETXx\t\ETX\\\v[\b)(\ESC]\135875Y\v@p\41515l\45065\157388\NUL\t\1100066\SOH1\DC1\ENQ\1021763\"i\29460\EM\b\ACK\SI\DC2v\ACK"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleMember,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T11:10:31.203Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\58076&\1059325Ec\NUL\16147}k\1036184l\172911\USJ\EM0^.+F\DEL\NUL\f$'`!\ETB[p\1041609}>E0y\96440#4I\a\66593jc\ESCgt\22473\1093208P\DC4!\1095909E93'Y$YL\46886b\r:,\181790\SO\153247y\ETX;\1064633\1099478z4z-D\1096755a\139100\&6\164829r\1033640\987906J\DLE\48134"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T23:41:34.529Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "Ft*O1\b&\SO\CAN<\72219\1092619m\n\DC4\DC2; \ETX\988837\DC1\1059627\"k.T\1023249[[\FS\EOT{j`\GS\997342c\1066411{\SUB\GSQY\182805\t\NAKy\t\132339j\1036225W "
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T00:29:17.658Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T13:34:37.117Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "Lo\r\1107113<D/\1080678\1047135|`5-j\113720{[qI\ETBY\136863C\ENQ\12798]\1098415#\DLEE\167933\&8S.\17945{[\GS\aE\159959E\ENQS"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T18:05:30.889Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\STX\169353t\b\169021\v\DC1)[Y3\ACK\ENQ4'K\a\1100241\1021845NSR(\13201Mm\1031218\&0`yC\3610\52938M~\1101289jBO \SYN\SOi/\1073867\ETX\NULuiu\vdP\1101172M3N\4552h3F\ETX!\17631cm\1070069\993270\97591\67352\US9\1005657\ETX\1068855\&2kFl\EOT\DC2r \1061616\158169\DC3c"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T15:21:05.519Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "K\1020696\1063906\171142\ETX\DC2M\1087382\SYN\1002264\25468\60040\1093048\ETBa~ZD\SI}+>B\95920_v\984354\1074038\1028140\1093478\&9\STXvyQ\128432&W\1030097\134680\1080940'\1053995:\149463\138648\1113696}-o\1046282le8Zp\1026640\1097625)nK\DC40\9951\&0DE\NAKK$io\RS|Ip2ClnU\181645"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_5 :: InvitationList
testObject_InvitationList_team_5 = InvitationList {ilInvitations = [], ilHasMore = False}

testObject_InvitationList_team_6 :: InvitationList
testObject_InvitationList_team_6 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T06:42:29.677Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T11:26:36.672Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T00:31:56.241Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T21:10:47.237Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "YBc\r\50934\&8{\\\n\1097235+\b\SYN\DEL'<\EOT\1081663Z\anOb\1096244\1101334\170916}2<u\ENQlS\138071A\DC1\180035\STX(\1032605\1063133/\r\51645NG\987674\23168k6^M\1087646\134976\vl\1064801\1003763/#T|\DC4\1032323\1091557\ETX|_\1016556\SOM\"m(\SUB\b\ACKmg\64021+\DC2\SUB32y"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T15:43:22.250Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T20:44:34.056Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Just (Name {fromName = "\1100765v\191022UcU+_\23043!?e Pr\40620=x-z5N\1059506"}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T11:23:55.061Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T10:06:43.943Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T19:42:31.295Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\988101l\52730u-@'\nn!'CK\DC38\r8\133255\69877\138131,;r\1065688S\1109836\SO\r^hH\178090\996621\"\194637u\1002144?A\1093531O1\173047\&6\155713\aW+\DC2C[c\1104847\&25-V\1091985\DC2\1017363m\1035909\189810d\1083637a\1075543+51\999435\NAK]\1047246\20557o*0\NAKC\3523n\vy\ETB\6284+\ACK\1049886"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T06:58:18.517Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T00:40:39.103Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Just (Name {fromName = "')\28977mD\71122?\v\"Q&_8\DC4a"}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T21:44:30.848Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T14:27:46.655Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "0\1032778F\1001257\19526/\DLE1\64642\57959dx\1021515\990081\SOE\b\1085143\65542\1023026,\DELkn\EOT\15377j[\185500\ACK\US{\1032659\1012564]b\f$i7,\25879{v\\uYI\1086200\186238\1068862|:w\DC1v\1097961Y\1067044\"N"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T03:57:53.185Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Just (Name {fromName = "\EM\1085994\5162\&29\93808\GS\n\RSzC`"}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleMember,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T14:35:39.474Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_7 :: InvitationList
testObject_InvitationList_team_7 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T14:44:40.049Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "8J\97163\1065575\&5\ETB#y\DC2T\SYN*O9\1030547CmCa)1e\SOH\131826)2\4970_u7\ETB\1025957\a\tb\568E\DC1@\1017070R\1059753\v\ESC\1097620\190666s\ENQ\111026\&1O\1070323Af(\14968)]\1039670\ESC3W\73889\"\12401\ENQ\EOT\SOH"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T19:09:35.565Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\1086593<\62708\1035786cl\EOT;\nB\40425\32084&Ur|8\ETX\a(UQ\58362\93792\NUL\DC2\147401h\985239|c\1063340?\42278\RS7h\999227\57954t\134319\NULw\r\34152\1029870\22366w_)\t\1025609\CANUy\EM#D"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T11:05:26.660Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "X\DC1\1103092\v/\a\SYN\ETBE\DC2<O\141295~b*\NUL\f\147396\&4\188255\EMO\1025085\96288UNH\15236.\1026289-\1080005\NAK?\1041361\DEL\1031624\24082JlY\1002297'\\2\EM"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_8 :: InvitationList
testObject_InvitationList_team_8 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T13:24:44.890Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just (Name {fromName = "I/\SOH\RS\1084682\1069618U\ETB\178928\1078899`\1087404TD4KU\5388?["}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T15:37:31.278Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\1041350rz\EM|%\EOT[\r\1039916\1017178i\1095116\38526\\+\156143i\US\bO3\58570M\984116I/X6Fnn^ 2g{]Z\1028085K}\a[a8\1058092f\SUB54b+Ye;t$\52307\SYN\999575\142215r"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_9 :: InvitationList
testObject_InvitationList_team_9 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T12:45:57.694Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T08:06:09.682Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "`\NUL\132575\1020351\30011P\143287\a\72400\a38\DC3\1112041QX\EMgq\161015.*e\40511#|'f(/G\nh$GHS\60489\GS(k\63238I`]\1055158x\190035i\STX\118995\370\163176o@\11183\151777Izq\156426\179352X\1094239^_\CANhGjR/z\DC2\182745\1076233\DLE\DLE\STX\STX\1028620DFW\120506\DC4W\DC4(\62434n\155938H\184409&it\STX<oW\ETB\SYN]"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleMember,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T01:04:27.531Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\ENQ\1105937\1082218X~|\133594\165234]\1020977gWMJ6\22072''\EM%xf\1095512=2\a\ACK \DC2\SOH\NUL\1015912\74994\18943.\1092396o\185429v\DEL\9591~>o@v/"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_10 :: InvitationList
testObject_InvitationList_team_10 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleMember,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T17:28:36.896Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "}" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName = "P\152999\ACK'e\DLE\GS\"\DC1K\1037800Fcvm[\"Sc}U\70290\1057576\1045791~!E\1049991\vV"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_11 :: InvitationList
testObject_InvitationList_team_11 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T01:33:08.374Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "Z",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "G\\,\NUL=\3549I-w\917561}\131693\25267-92\DC3@\ACK\US\\F\SUB\"-r\42155\&6\SI\3500\US*}c\1033785\US\aT8m@\26053M\DC2#MIq\r4nW\1104272y\ENQUd\47339#\179602\&5\n\STXV]\165952\"<OI~e\25178M~\DEL\182183\US{/|T\159562y$\EOT\GS \1103873}~[!\163064j0\995858~\95009"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = True
    }

testObject_InvitationList_team_12 :: InvitationList
testObject_InvitationList_team_12 = InvitationList {ilInvitations = [], ilHasMore = False}

testObject_InvitationList_team_13 :: InvitationList
testObject_InvitationList_team_13 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T04:37:12.563Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T05:36:38.967Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\994542\NAKw/\1052566.f\1007214-\1030283\DC1N\t\49191\\XM\ETB{i\49791\184588\144132\a\100864\tyz\1088074#s\1092212{<\175371\DC43-\157880\1105778:e\FS\1069621}\SUB!\1112190bc\vq{\14242)\1041251[\ETXBSQC"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T17:31:07.346Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\1102212\169290\1106435\100525eG\STX\1007181\r\173749\10441/\27105<\2270 '\12330\GS\t.\EOT_o_\STX\SIH\NUL\n\v\NULn \1094133\RS"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T17:18:26.847Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\141948;e\22744\SOHQz\EOT\a\1102781w(h!\171541\DLEx\173218.]\1057140 fs*\178577{nazI\ENQ\986534`\44375?\25283\DC1,W\n\SII\DC1I\38878\NAKl8\1062502*&\128545?\1104659/\1106548\&7Zp\NAK\1069997?WC\vZ\1006139IBxk\148534\ENQ\1070706 15\1054011\SI#2\EMG^w`\NAK\35325\1083642\1063225:|M\b\189501J&"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T12:43:17.559Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\GS\1107873\\PM\v\DLEU\1097493\FS\SUBF(\FS$w\1054931\NAK\ACK\1033467\30078gk\EM\SOe\118941\RSv\\\STX\133742\STXJ\194921R\EOT3\1016521jx\1108890K\1102645\190174\1063299\SI\3508Q\1109890x,\NUL,\ENQ\1102339Cg\SOH\b\993150\26092XZ:\ACKC\1042056^\1099391O\axZ\SI\SUB5\142807\t"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T14:24:17.699Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just (Name {fromName = "\US\1039297@p(#\1103640\28521\&0\1083979\n[q~2\f\1057993P\CAN\ACK"}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              inRole = RoleMember,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T16:30:09.682Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "C$\DC30\DC4Y\177559J\rK\1037217\&2g#.H2PmT\987228t\1034128\40120\SOHHI|a:?P\1105347%K[E\1032763ig-K\1040395+Ul\61321\FS\\ ;_h\RS\aLOz\1010646/\ETXV)\DELa@\174946\999132\"\GS\1054858\62985/\1044239\&2\17537\EOT\1032172"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_14 :: InvitationList
testObject_InvitationList_team_14 = InvitationList {ilInvitations = [], ilHasMore = True}

testObject_InvitationList_team_15 :: InvitationList
testObject_InvitationList_team_15 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T15:54:11.332Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "\EMOX^Gr\165898)\1011974\DC1U\\\1048848\13465\15567\&1N\50964M\1084911\7316Nj<\EOT\bR\\v\184725\58158\1111120\SUBr\NUL\182647A\1096185IK\986119\NAKf\25608N\RS\1056043e\139500f@N\174882%VY\SO\&Hz\DEL\SUBO\bL}B\DLE\nolk?\1017065`\62940\tak{$~Pd\1042568rm*q7\ESCi^\94032\997634Yvo;\169228xq+\7788ud>\1007747\170712\&0:\64700\191337+\SOH\vP\1020855X\38239\DEL\1070375.\EMN\"\180987"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T23:06:13.648Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just (Name {fromName = "\21461\&5q}B\SOH\156444`\65394w\\X@\1035677\143112\&7Mw,*z{\132791&~"}),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T10:37:03.809Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "V\1084142\DLEqY\65419CU\SO\1066304fQJ\ENQ\2579q+\a\SYN\987832\DC1@\149564`\22367qh+\184236A7\155937Y \DC1T\12622\&1_\1083975#B<\1058881;a6o="
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inRole = RoleAdmin,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T04:46:03.504Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          ",\1062974\60953{\1405\f\181627Uh\51417\t\ESC\EOT\SOHO@\SUB_\STXD\1109029\155675\SYNg}"
                      }
                  ),
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T12:53:52.047Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_16 :: InvitationList
testObject_InvitationList_team_16 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T15:25:30.297Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              inInviteeEmail = unsafeEmailAddress "\SI" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "E\120326YM<\1043108j\1075334\r\1080535O\1001460MCU\RSI\995971m\83127hG\DC2|:P\1070845Vj\FS\NULffgG)K{\1053135\&7x5\989168\173336\n\flT\1073822"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_17 :: InvitationList
testObject_InvitationList_team_17 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T10:54:19.942Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              inInviteeEmail = unsafeEmailAddress "&" "\179430",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }

testObject_InvitationList_team_18 :: InvitationList
testObject_InvitationList_team_18 = InvitationList {ilInvitations = [], ilHasMore = True}

testObject_InvitationList_team_19 :: InvitationList
testObject_InvitationList_team_19 = InvitationList {ilInvitations = [], ilHasMore = False}

testObject_InvitationList_team_20 :: InvitationList
testObject_InvitationList_team_20 =
  InvitationList
    { ilInvitations =
        [ Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inRole = RoleExternalPartner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T07:22:02.426Z"),
              inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName = Nothing,
              inInviteeUrl = Nothing
            },
          Invitation
            { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              inRole = RoleOwner,
              inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T18:56:29.712Z"),
              inCreatedBy = Nothing,
              inInviteeEmail = unsafeEmailAddress "" "",
              inInviteeName =
                Just
                  ( Name
                      { fromName =
                          "YPf\9566:\ENQ\7880&\CAN\DC1\1038811%\43425k\175051\1071439:Q\ENQF+\bb8Jh\1098643K\a\GSY\EOT\1061857\SI\1038192\SYN \1054176\&6>I\1041001B$z?\149694wECB\RS\155436\45892\"W\96542\994562@\RSg)\SOHm!-U\1079782\1005360o\ACKa\EOT\184482;R\1059495kgT\1102216f\EOT\RS\rp\78734\1045066X/\1065138)\STX5.Ym\1006828n\49887N\DC3\176452]?'\134458a4\"\997374!i5\RS\GSC14"
                      }
                  ),
              inInviteeUrl = Nothing
            }
        ],
      ilHasMore = False
    }
