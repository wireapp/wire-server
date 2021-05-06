{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Invitation_team where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_Invitation_team_1 :: Invitation
testObject_Invitation_team_1 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T10:48:13.043Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "w\r", emailDomain = "\ESC\RS"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+2071044484"})}
testObject_Invitation_team_2 :: Invitation
testObject_Invitation_team_2 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T10:34:15.352Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "X\1075439u", emailDomain = "\1043289q0"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+3367033186"})}
testObject_Invitation_team_3 :: Invitation
testObject_Invitation_team_3 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-10T13:20:14.316Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "6%", emailDomain = "\72133"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+45723596"})}
testObject_Invitation_team_4 :: Invitation
testObject_Invitation_team_4 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T06:18:48.876Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\a\156848", emailDomain = "N"}, inInviteeName = Nothing, inInviteePhone = Nothing}
testObject_Invitation_team_5 :: Invitation
testObject_Invitation_team_5 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T16:58:39.133Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\159505f"}, inInviteeName = Just (Name {fromName = "{\f1\15651Hzi]%W]m\SOH\23977\&7G6|\58011eE,pIr^\f\1056919T\2383\1015041\134253\1093876\ETBQl\135858\fZq*N\EMN\164611a\1040563pC\DLEf\STX\DC25\SO\1100247.p:\EOT\"I\CAN\1112202M\144062J,\1072932QDal]\1103931Hm\1025318\194873\&5g\DLE{\ESCn\NULi"}), inInviteePhone = Just (Phone {fromPhone = "+181747415636101"})}
testObject_Invitation_team_6 :: Invitation
testObject_Invitation_team_6 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T16:40:22.583Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\a_!"}, inInviteeName = Just (Name {fromName = "9t\ETBi\1066473\&9(wt\16284:e>\35656\135187\1023392\17170\&1\100595o^\190929\66252\23607\57686\STX\1066822\SYN`4<\1087532\&7fCNd\ENQyH27\1064642z\1061728\n\tt\1077276[\1019040\DC4g\STXBc/Vu\USW8\21728\42322\NAK\ACK\DC1ij/\188606e\1062785k\FS0\ETX\177047/\n\ESC \167314z\\\1026030\&0\ETXGc\1058508\CAN+\CAN~\SI\13234}\ETB8\1101587"}), inInviteePhone = Just (Phone {fromPhone = "+62673280983"})}
testObject_Invitation_team_7 :: Invitation
testObject_Invitation_team_7 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T19:11:56.716Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "$\RS", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+29253647990733"})}
testObject_Invitation_team_8 :: Invitation
testObject_Invitation_team_8 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T15:12:03.408Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "\176729\STXI", emailDomain = "\ENQf"}, inInviteeName = Just (Name {fromName = "r\"\1050896\152862\FSI0W\NAK\1007310$72-\1080664U\ACK\NAKx\RS\1027093\160552\&3\",T\ETB\16564;\173945\EM\1107251\996552\40203\146560WR\\8\69653b\ACKk2M|AG\36301\DC4"}), inInviteePhone = Just (Phone {fromPhone = "+9667260938"})}
testObject_Invitation_team_9 :: Invitation
testObject_Invitation_team_9 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-10T06:33:58.601Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "\1023375", emailDomain = "Y["}, inInviteeName = Just (Name {fromName = "\EOT\168994\159477rN<t\991049\SUB"}), inInviteePhone = Just (Phone {fromPhone = "+8063673702"})}
testObject_Invitation_team_10 :: Invitation
testObject_Invitation_team_10 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T23:02:39.703Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = ">", emailDomain = "?"}, inInviteeName = Just (Name {fromName = "|\rH\34300U\1087443*\GS8Mb7\96774+\SUB\1024141\FS"}), inInviteePhone = Just (Phone {fromPhone = "+478095894"})}
testObject_Invitation_team_11 :: Invitation
testObject_Invitation_team_11 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T15:37:27.924Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = "2"}, inInviteeName = Just (Name {fromName = "\1095254\GS/M}\119605%\rEM"}), inInviteePhone = Just (Phone {fromPhone = "+136701674733379"})}
testObject_Invitation_team_12 :: Invitation
testObject_Invitation_team_12 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T23:20:19.505Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "M\CAN"}, inInviteeName = Just (Name {fromName = "\37189\EM,;\US1\SUBF\14430\&9\bb\26512O#\146689i;]/\ESC\166405.\ESC|\16182\DELw*\136945\67650\fH\100226l"}), inInviteePhone = Just (Phone {fromPhone = "+05500249892"})}
testObject_Invitation_team_13 :: Invitation
testObject_Invitation_team_13 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-07T08:17:38.486Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), inInviteeEmail = Email {emailLocal = "}l", emailDomain = "6"}, inInviteeName = Just (Name {fromName = "\DC1\189648d\t&h(\156594\v1P\132931[\NAK$Lq(_b\1073763\1102891\t\54265\SOH\1088773=Y\132456 \EMG{\v\SOHMl\13266\DC3\140146G*\1042423|#?]h\1073424\1008804PL\v\1068342X\DLE\1067569\1080333\45257\1081207?\68880@\4639\42593\1032200\ETB9\1042966\5193\&1t\119914\1106845Z\18435m\DC2\20306\SUB\1030914\184341xS\1015297e\983274*G0\DC2r\1003503.\DEL\DC1#8/N\1777\EM{\25526=/\1030057|He\1035345\ESC\ESC\1076067\ETX\53947#\183920\USQz\SI\ACKm"}), inInviteePhone = Just (Phone {fromPhone = "+50789115094934"})}
testObject_Invitation_team_14 :: Invitation
testObject_Invitation_team_14 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T17:17:08.213Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = "?\US"}, inInviteeName = Just (Name {fromName = "\120609\1054959\US\61437\140948\138974z(QfZ+m\47078y\SIS\rV\1038410\1057002\ENQN\120897\US@\DC1B\DLED?\a[T'\NULz\1094945MWz\"}8*N\987384\993014^a\19635\SI\NAKt\1099665\&4P\1079663\ETB[\NULB\1063143Q\163794r.Z tb;$\121408\US\NAKG/Q\NUL\RST\18983\5106q87\140157\24261$H\1048539\161493%`\35493\1060/9a+g\163353\41999\NULz\45648>\USp}-\1073513\&6"}), inInviteePhone = Just (Phone {fromPhone = "+6675352236"})}
testObject_Invitation_team_15 :: Invitation
testObject_Invitation_team_15 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T05:13:10.797Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "F\5492", emailDomain = "6"}, inInviteeName = Just (Name {fromName = "q~\SO7+B\DC3\NULd\EOTqg\1029389\ETXja\131768!%?CfU\SIq\65891Tq\ENQ\30195\1110325.Z$DK\GS\SOH\SOHF\1082138 C\DC2\1049340\&35\1013809QT\62105.\ESCh"}), inInviteePhone = Just (Phone {fromPhone = "+1079355327517"})}
testObject_Invitation_team_16 :: Invitation
testObject_Invitation_team_16 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T00:39:02.858Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "N\61080", emailDomain = "N\SUBm"}, inInviteeName = Just (Name {fromName = ";W\993913I\GS\992629\DC3Ksj9\1095189\1031508L*\159569AT\t!\DC3D)\ACKX]E\42407+p[\"~#\ETX\1010112$\985103\1026166K`vB4\43665#s]\998864+XT\ENQ+\ETB\SYN\187006^2#.\1005135\62154\DC1?L+UX\DEL\CANf\n"}), inInviteePhone = Just (Phone {fromPhone = "+7504527031289"})}
testObject_Invitation_team_17 :: Invitation
testObject_Invitation_team_17 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-06T21:49:51.696Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), inInviteeEmail = Email {emailLocal = "", emailDomain = "\ETB"}, inInviteeName = Just (Name {fromName = "&ny tU"}), inInviteePhone = Just (Phone {fromPhone = "+01585350070"})}
testObject_Invitation_team_18 :: Invitation
testObject_Invitation_team_18 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T22:08:45.211Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "\DC4V>", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+612993332645527"})}
testObject_Invitation_team_19 :: Invitation
testObject_Invitation_team_19 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-12T03:26:49.664Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), inInviteeEmail = Email {emailLocal = "\159028", emailDomain = "\DC3P("}, inInviteeName = Just (Name {fromName = "\179935\23452\FS2k\SOH\131705]vaC\1028118$\153836pw\1080968\1035763\179787\&3f\DLE\NUL\144261\58969wM\15299P\1027381Y^`=;h\rw\1047715(\1012437yXq\CANb\1089874\1052834\96108u\DC2R\4132\&0#\ACK\FSo\RSH\ACK\SOHD,\GS\69465\175235d\ETB\t\1109015f\1025046{4"}), inInviteePhone = Nothing}
testObject_Invitation_team_20 :: Invitation
testObject_Invitation_team_20 = Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-11T01:15:20.300Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), inInviteeEmail = Email {emailLocal = "Qy", emailDomain = "Hf`"}, inInviteeName = Just (Name {fromName = "_s\1051144\ETX\121337b\1005713K+\35458G\997868B\1022221lC*.\SO\SOH?\1052660up\1039213R\DC1\994527\1107945"}), inInviteePhone = Just (Phone {fromPhone = "+372049798144"})}
