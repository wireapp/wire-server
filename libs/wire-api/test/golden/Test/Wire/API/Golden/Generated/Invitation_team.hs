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

module Test.Wire.API.Golden.Generated.Invitation_team where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Team.Invitation (Invitation (..))
import Wire.API.Team.Role (Role (RoleAdmin, RoleExternalPartner, RoleMember, RoleOwner))
import Wire.API.User.Identity (Email (Email, emailDomain, emailLocal))
import Wire.API.User.Profile (Name (Name, fromName))

testObject_Invitation_team_1 :: Invitation
testObject_Invitation_team_1 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-11T20:13:15.856Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
      inInviteeEmail = Email {emailLocal = "\FS\58114Y", emailDomain = "7"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-12T14:47:35.551Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),
      inInviteeEmail = Email {emailLocal = "i", emailDomain = "m_:"},
      inInviteeName = Just (Name {fromName = "\1067847} 2pGEW+\rT\171609p\174643\157218&\146145v0\b"}),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T22:07:35.846Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
      inInviteeEmail = Email {emailLocal = "", emailDomain = "\31189L"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T09:23:58.270Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
      inInviteeEmail = Email {emailLocal = "^", emailDomain = "e"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")),
      inRole = RoleOwner,
      inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T03:42:15.266Z"),
      inCreatedBy = Nothing,
      inInviteeEmail = Email {emailLocal = "\SOHV", emailDomain = "f\1086249\43462"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "}G_\147658`X\1028823\131485\1014942L\"\1047959e6:E\DEL\51733\993223f-$\133906Z!s2p?#\tF 8\188400\165247\1023303\EOT\1087640*\1017476\SYN\DLE%Y\167940>\1111565\1042998\1027480g\"\1055088\SUB\SUB\180703\43419\EOTv\188258,\171408(\GSQT\150160;\1063450\ENQ\ETBB\1106414H\170195\\\1040638,Y"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_6 :: Invitation
testObject_Invitation_team_6 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T08:56:40.919Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))),
      inInviteeEmail = Email {emailLocal = "", emailDomain = "OC"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "O~\DC4U\RS?V3_\191280Slh\1072236Q1\1011443j|~M7\1092762\1097596\94632\DC1K\1078140Afs\178951lGV\1113159]`o\EMf\34020InvfDDy\\DI\163761\1091945\ETBB\159212F*X\SOH\SUB\50580\ETX\DLE<\ETX\SYNc\DEL\DLE,p\v*\1005720Vn\fI\70201xS\STXV\ESC$\EMu\1002390xl>\aZ\DC44e\DC4aZ"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_7 :: Invitation
testObject_Invitation_team_7 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-07T18:46:22.786Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),
      inInviteeEmail = Email {emailLocal = "oj", emailDomain = ""},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "\CAN.\110967\1085214\DLE\f\DLE\CAN\150564o;Yay:yY $\ETX<\879%@\USre>5L'R\DC3\178035oy#]c4!\99741U\54858\26279\1042232\1062242p_>f\SO\DEL\175240\1077738\995735_Vm\US}\STXPz\r\ENQK\SO+>\991648\NUL\153467?pu?r\ESC\SUB!?\168405;\6533S\18757\a\1071148\b\1023581<jo\100111$\DLE-NM\NUL\46271\1086047\ETX\DC3lx3_!on\65322%C\1049909.C+\GS\135785\38869<Dl \25861"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_8 :: Invitation
testObject_Invitation_team_8 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-07T12:20:00.738Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
      inInviteeEmail = Email {emailLocal = "", emailDomain = "Y\fr"},
      inInviteeName = Just (Name {fromName = "\67592\154970\1102305lE\990376\SYN\rjI!@\RS\1094043"}),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_9 :: Invitation
testObject_Invitation_team_9 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-06T13:15:34.606Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),
      inInviteeEmail = Email {emailLocal = "Qi\183990", emailDomain = "\144719"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_10 :: Invitation
testObject_Invitation_team_10 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-10T19:57:59.926Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),
      inInviteeEmail = Email {emailLocal = "\SOH", emailDomain = "\SUB"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_11 :: Invitation
testObject_Invitation_team_11 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-10T16:20:51.120Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),
      inInviteeEmail = Email {emailLocal = "6", emailDomain = "\1101264N"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "\1066540,\1101928\32204sC\nD\RS\989890*\DC1\162957\DC1\995201a\1037107\98296{.\29119\185514\48733\137782\&9/\3207<;$\186584\133817Z\ENQ'\1026078!F\1106556\1034911n\"\n8\GSH\1035234\&9\68252z:d\DLEF\161136y_w\ri\36717!>\996567\17385\120022\b\SUB\FS\SIF%<\125113\SIh\ESC\ETX\SI\994739\USO\NULg_\151272\47274\1026399\EOT\1058084\1089771z~%IA'R\b\1011572Hv^\1043633wrjb\t\166747\ETX"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_12 :: Invitation
testObject_Invitation_team_12 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-12T22:47:35.829Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))),
      inInviteeEmail = Email {emailLocal = "\1016862\141073\RS", emailDomain = ""},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "\DLEZ+wd^\67082\1073384\&1\STXYdXt>\1081020LSB7F9\\\135148\ENQ\n\987295\"\127009|\a\61724\157754\DEL'\ESCTygU\1106772R\52822\1071584O4\1035713E9\"\1016016\DC2Re\ENQD}\1051112\161959\1104733\bV\176894%98'\RS9\ACK4yP\83405\14400\345\aw\t\1098022\v\1078003xv/Yl\1005740\158703"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_13 :: Invitation
testObject_Invitation_team_13 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001")),
      inRole = RoleMember,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T01:18:31.982Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),
      inInviteeEmail = Email {emailLocal = "", emailDomain = "\DELr"},
      inInviteeName = Just (Name {fromName = "U"}),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_14 :: Invitation
testObject_Invitation_team_14 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000")),
      inRole = RoleOwner,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-12T23:54:25.090Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),
      inInviteeEmail = Email {emailLocal = "EI", emailDomain = "{"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_15 :: Invitation
testObject_Invitation_team_15 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")),
      inRole = RoleOwner,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T22:22:28.568Z"),
      inCreatedBy = Nothing,
      inInviteeEmail = Email {emailLocal = ".", emailDomain = "\DEL"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "\71448\US&KIL\DC3\1086159![\n6\1111661HEj4E\12136UL\US>2\1070931_\nJ\53410Pv\SO\SIR\30897\&8\bmS\45510mE\ag\SYN\ENQ%\14545\f!\v\US\119306\ENQ\184817\1044744\SO83!j\73854\GS\1071331,\RS\CANF\1062795\1110535U\EMJb\DC1j\EMY\92304O\1007855"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_16 :: Invitation
testObject_Invitation_team_16 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-09T09:56:33.113Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
      inInviteeEmail = Email {emailLocal = "\\", emailDomain = "\"\DEL{"},
      inInviteeName = Just (Name {fromName = "\GS\DC4Q;6/_f*7\1093966\SI+\1092810\41698\&9"}),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_17 :: Invitation
testObject_Invitation_team_17 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T06:30:23.239Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
      inInviteeEmail = Email {emailLocal = "", emailDomain = "\SOH[\97119"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "Z\ESC9E\DEL\NAK\37708\83413}(3m\97177\97764'\1072786.WY;\RS8?v<J\999550I\GS\1037689=o\1020531\&5"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_18 :: Invitation
testObject_Invitation_team_18 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
      inRole = RoleAdmin,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-08T06:07:59.528Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
      inInviteeEmail = Email {emailLocal = "\SOH", emailDomain = "l\42676a"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "9\174654{\STX\\\DEL\DEL\179405Ea\ETX.\\\ETX\1038439\142170\1052766M/$\DC3\1009110\EM\STXsH\134868\123163H-|\1008958=A9#\120244|j\EM \DLE%=o&-+6\tq\1099583`@N\r\ETX5bi\1042843SBU\fH@\SOr\DC12\3264\FS\128633\RS]]SS\1051198ho\nJ/\1042511#)RV+\SO4\169681\&0\1081017\179235[q\27346D\1054061\EM\1076129\"\NULfl\DC1\1008975j>-\1100720\DC2\1015859"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_19 :: Invitation
testObject_Invitation_team_19 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")),
      inRole = RoleMember,
      inInvitation = Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-07T15:08:06.796Z"),
      inCreatedBy = Nothing,
      inInviteeEmail = Email {emailLocal = "\1019726\96050\DEL", emailDomain = "(S\ETB"},
      inInviteeName =
        Just
          ( Name
              { fromName =
                  "\38776r\111317\ETXQi\1000087\1097943\EM\170747\74323+\1067948Q?H=G-\RS;\1103719\SOq^K;a\1052250W\EM X\83384\1073320>M\980\26387jjbU-&\1040136v\NULy\181884\a|\SYNUfJCHjP\SO\1111555\27981DNA:~s"
              }
          ),
      inInviteeUrl = Nothing
    }

testObject_Invitation_team_20 :: Invitation
testObject_Invitation_team_20 =
  Invitation
    { inTeam = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
      inRole = RoleExternalPartner,
      inInvitation = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001")),
      inCreatedAt = fromJust (readUTCTimeMillis "1864-05-12T08:07:17.747Z"),
      inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
      inInviteeEmail = Email {emailLocal = "b", emailDomain = "u9T"},
      inInviteeName = Nothing,
      inInviteeUrl = Nothing
    }
