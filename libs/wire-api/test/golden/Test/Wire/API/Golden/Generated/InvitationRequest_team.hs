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

module Test.Wire.API.Golden.Generated.InvitationRequest_team where

import Data.ISO3166_CountryCodes (CountryCode (BJ, FJ, GH, LB, ME, NL, OM, PA, TC, TZ))
import Data.LanguageCodes qualified (ISO639_1 (AF, AR, DA, DV, KJ, KS, KU, LG, NN, NY, OM, SI))
import Imports (Maybe (Just, Nothing))
import Wire.API.Locale
import Wire.API.Team.Invitation (InvitationRequest (..))
import Wire.API.Team.Role (Role (RoleAdmin, RoleExternalPartner, RoleMember, RoleOwner))
import Wire.API.User.Identity
import Wire.API.User.Profile (Name (Name, fromName))

testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 =
  InvitationRequest
    { locale = Just (Locale {lLanguage = Language Data.LanguageCodes.NN, lCountry = Nothing}),
      role = Just RoleOwner,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.AF, lCountry = Just (Country {fromCountry = GH})}),
      role = Nothing,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.LG, lCountry = Just (Country {fromCountry = TZ})}),
      role = Just RoleAdmin,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "\27175\1085444\v\182035\144967G\189107\1042607\ETX\180573\1047918\ETX\1075522ZG\1087064\STX+i\46576Ux\FS\FS5\ESC\ae\10301\36223(3\1009347\\\t\EOT\v@\ENQs\r#R\136368G'N^?\NAKB\f\FS\NULx\1024041@\34031\1105463\1058551`A]@\34846\133788*\1025332N;\ETX\FSh\bS\US\US\SO`^qU<\21803\SYN\1094791\ETX\1112073M\SI\1019355\4619=zM[\181520\161190\n\SI}\ENQ\1008012\aaZI\18628\ACKE#G^t\148685\DLE\157774LY\182624\&6vt\\"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleMember,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleAdmin,
      inviteeName = Just (Name {fromName = "\171800\1076860\1103443\CAN8=\n;}\169054M\ao\v3+\n"}),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_6 :: InvitationRequest
testObject_InvitationRequest_team_6 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Just (Country {fromCountry = ME})}),
      role = Just RoleExternalPartner,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "\RSD[alw\RS\ACKP \999760\rO\175510'8\989959\1082925g W:8\v:-(`+\131521\ESC_\CAN\1105214\44926(\"&\DC2NZ\1082341\ACKS\SYNLOW|p\EM\194645\&1\175388"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_7 :: InvitationRequest
testObject_InvitationRequest_team_7 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleAdmin,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_8 :: InvitationRequest
testObject_InvitationRequest_team_8 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = NL})}),
      role = Nothing,
      inviteeName =
        Just (Name {fromName = "\1036838&f\1104978\1021739j5\CANv]k\1034960\993099c[\1019257\1047325\EOTw.uL~/"}),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_9 :: InvitationRequest
testObject_InvitationRequest_team_9 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KJ, lCountry = Just (Country {fromCountry = FJ})}),
      role = Just RoleAdmin,
      inviteeName =
        Just (Name {fromName = "|H\181717/%\RSu\1019619\&7V\142010\62451*G\SOHE\993531,\1015423WGtY\SYN*Nd\156695{Pl"}),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_10 :: InvitationRequest
testObject_InvitationRequest_team_10 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.NY, lCountry = Just (Country {fromCountry = OM})}),
      role = Just RoleMember,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "H\1008404\RS\45861\92335uv\1045159\DC2\1045852\SUB \160164=a\ESC4H,B\CAN\1039540GpV0\1044935;_\NUL\173370Z\DC1\28376\NAK6\32784'W9z\11986\t\59610r\150374\1057016\SYN_ge\35917\EOTD\94732o\an>\993583"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_11 :: InvitationRequest
testObject_InvitationRequest_team_11 =
  InvitationRequest
    { locale = Just (Locale {lLanguage = Language Data.LanguageCodes.SI, lCountry = Nothing}),
      role = Just RoleOwner,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "\167004\41433\11577\74832h_5bb2}\46841\166935P\NUL\SOT*\US`b\170964\SI:4\n5\SUB\GS*T\1016149Bv\ESC\ETX\GS\1050773\175887Uu\r_\DLE)y\153990\EOT\b\US\DC4\FS\CAN?\1050027\149716\22398\NAK\SUB4\v 5\NULi\43113o=\tnG\37464\ETBiC\DC39\SOP\1026840\n\v\EM\SYNU\7800%\49334\DC2\USF\FS"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_12 :: InvitationRequest
testObject_InvitationRequest_team_12 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.AR, lCountry = Just (Country {fromCountry = PA})}),
      role = Nothing,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "_\EM@\GS0\52658\1041209\1014911\FS\DLE\1100406!\1081838\SOc\US\NUL\SOH>\1074611\168456\EM\175538\&1}!h0\DLE\1053201w\EOT\1073681\&1aJ6c\GS\986890b\131925{\996638\131443\a\1094281"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_13 :: InvitationRequest
testObject_InvitationRequest_team_13 =
  InvitationRequest
    { locale = Nothing,
      role = Nothing,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "C\990664+\1033671\n#s\1072813\FSpb\SOH\1015233\1073302\&1\ETBE_\CANj\EMV\US\1063126\15431\1099470lO8\ACK\1056562\FS\SYN\CAN\DLE6\137862-beR!s\48584\ETB\v\1049375\984016xt\SIRf~w\1030329\DEL+_\70046\&91:,\1034030#cf\1056279\3624\2548\6959B\"\1097722F\t\1109914\1069782/\DEL\DLE'\1004715*\171262\&7\156200w\1061410H\59715x\DC32\EMt\163668o6\DC4F%=t\1003324\1097336=\NUL\ENQA\1101771\1011923\NUL\EOT[i\992519@\b\FS\f"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_14 :: InvitationRequest
testObject_InvitationRequest_team_14 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = LB})}),
      role = Just RoleAdmin,
      inviteeName = Just (Name {fromName = "\NAKwGn\996611\149528\&1}\EOTgY.>=}"}),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_15 :: InvitationRequest
testObject_InvitationRequest_team_15 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleOwner,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "y\1104714\&5\1000317\710S\1019005\DC4\rH/_\DC3A\ETX\119343\&0w\GS?TQd*1&[?cHW}\21482\1021206\CAN\180566Q+\ETXmh\995371X\SO\ENQ\DC1^g\144398\bqrNV\SO\1095058WMe\a\ENQ"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_16 :: InvitationRequest
testObject_InvitationRequest_team_16 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.OM, lCountry = Just (Country {fromCountry = BJ})}),
      role = Just RoleAdmin,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_17 :: InvitationRequest
testObject_InvitationRequest_team_17 =
  InvitationRequest
    { locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KJ, lCountry = Just (Country {fromCountry = TC})}),
      role = Just RoleExternalPartner,
      inviteeName = Nothing,
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_18 :: InvitationRequest
testObject_InvitationRequest_team_18 =
  InvitationRequest
    { locale = Just (Locale {lLanguage = Language Data.LanguageCodes.KU, lCountry = Nothing}),
      role = Just RoleExternalPartner,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "8VPAp\137681\&2L<s\ACKt]\1051893\1028831G/\SIQb\1099332<\62973B\DC3\995191kJ&\1028424\DLE\a \66433\SO\987741\1099076$\99376\"u2g\ENQ[<.N;%\EMsm\43781*\1030957s\184809DsCowW-\1069896&EF=\\H\NAK,Z\rJ\ETBw-\STX\ahC`\1077061\52563\&1Ds^7Udh+e\fL Ld\ESCh&\1000121\1102718\1028691;\142313\a\985672Xp\26072\SOP\b\t\187311\1063310.\DEL\RSp"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_19 :: InvitationRequest
testObject_InvitationRequest_team_19 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleAdmin,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "kl\ETX\EOT\SYN%s7\1031959fX\994905A\b7\DC1\DELD\EOT\DC1\165155s\DELg)dD\157274Rx[\1026892Tw\68117\RS\SUB\1049684z\\\SI\ENQ\17054l\1089470l|oKc\\(\187173\1101164=\33052\&2VI*\1095067\&2oTh&#+;o\5017dXA\12103=*\1074686Q\1032360{\994965\917585\&5}\GS9D\186360\1064921r\1080854P:<!|\1002411\v4Pt1\983861g\b\STX\152876\rfY\135334$\DEL_\54841\"\1035381\&8"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }

testObject_InvitationRequest_team_20 :: InvitationRequest
testObject_InvitationRequest_team_20 =
  InvitationRequest
    { locale = Nothing,
      role = Just RoleOwner,
      inviteeName =
        Just
          ( Name
              { fromName =
                  "N\1014949\3115qE\1086743,\1069753\1076493\&3-19bY\"Iz|BpQ\1112885\"\ACKdfC\1095189p\SO\1038198%-Z\SUB\1082854!Z\156657d\va\174302\ESC\b\ESCg\DELb\b\1009771\995646X}\STX\\^\1091690\&9\58052\1113953"
              }
          ),
      inviteeEmail = unsafeEmailAddress "some" "example"
    }
