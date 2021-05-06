{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationList_team where

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
testObject_InvitationList_team_1 :: InvitationList
testObject_InvitationList_team_1 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T16:16:51.435Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "*t\US\NAKe|/'\SUB\ESC\1060846>Ku\DC1uv\1029576\986891y\993392C%\1024362\&1"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:57:06.191Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T18:51:48.273Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "e7Z]\SO|\147757\&6\152452a\57861\1083758U\DC2\1055711[T\165074\1023220\1018190\1048363`i\GS\SUB\RS+\DLE\vS\NUL?\164346j\aBc\RSvk\SOH3A\1012428\1552M(`H\158026)\1087982\1025206\148775\\G\1076397p\b\STX\1045422\1031451oJk\6509F\r\1086082\DC4\NAK\985876o\156857H:`\5736GR\174040\1043708=\1053194P\SI\NAKR2\NAK\172665P\96983Ey\ENQ.F\r\"\16106"}), inInviteePhone = Just (Phone {fromPhone = "+49028990328"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T11:53:53.585Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\ETB&e\DC4\986872\28572;W\1066337(XP/F\US\\daQB#zw\1003102\171986a\136614\DEL\142071\tY\37043\65138\1043800`\1102587\&8"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T07:52:09.622Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\v\139816\59843g\roh\988386\1069734\SOx\NULnR6a"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T11:10:53.978Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\67989\177688:7q~dLoG\v\SYNKXFy\ESC\40538W\148026B6\165288Bh 4V4\DC2Qf\7534"}), inInviteePhone = Just (Phone {fromPhone = "+90060367159"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:56:04.858Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+3152571897426"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T19:35:55.423Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "Oq*81\185494n[w\1031086\917606E\59266B\179833\60359D\158107wd\1092336\126487\1078245b\SI\RS:ti}[zbMg\DELh_:\DC1\DEL\SO\SOHu7\DC3\tn\1099910\ETX\SI\USQ\ESC\n"}), inInviteePhone = Just (Phone {fromPhone = "+733236597045"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T13:33:14.475Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+348723661"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T21:58:53.502Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "x\DC3\USz\bv1\136350\1092282\58731\152438t\1035905n\DLE\1043872\SO\144838[\1022570)\164653\STX\21852gOmz\RS6R\FS"}), inInviteePhone = Just (Phone {fromPhone = "+63651478528028"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T19:55:04.196Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\172900\SUBM3]\ETB8d\14565\1056388|M\1089813\&1\1058158\95820o\999337Z\1016776\SO*\a\US\STX\1074166\&5\6260 r\CAN\64617\&5mt}2 *\1076739\vxm4l|\1032912n 7kR\1020380eS"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T05:10:17.587Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "4rebr+K\DC4\tzHUq\59704\EM\1076334\1086539!\ETBs\ACKI\37162\1019636K\21745]M\1029095\44768\&5\142595PeR\15964clCI\11549\998528\1048457 E\149161aM[Rf\\e[r+\157084t\157034W(\29057Q\1057358`t\135548m\152017/\175736\133922,;A,\ESC8\a\STXC\SOH\f\SOH\1006557fN\1055108;I\DELpOc\SUB\152384"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T20:15:32.702Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+585034989"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T00:58:32.312Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+156825392"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T12:59:15.074Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "x^\"\DC3\1063348\DC2&(\9922\1055293uk\162317\987290\SOH\RS>z7x\185228V\1111806F\FS\152860a\1090243PBP\DLE\EOTUe-:B\1020876\136713\1082058{\1047784\1099913(\DLE\9901\SOH\DC2nMa4\188830(\1073790\&7\1082801\1110207\a\175588o\n,\"clf\19720Y\ETX\FS\NUL\v"}), inInviteePhone = Just (Phone {fromPhone = "+334077988"})}], ilHasMore = True}
testObject_InvitationList_team_2 :: InvitationList
testObject_InvitationList_team_2 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_3 :: InvitationList
testObject_InvitationList_team_3 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T11:25:15.015Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\SI.\163193CX.\40600V,C_$}5Y\SO\&H-)EXa\tw\SO\991579@\37786 K\187338=!\1084271m5HP\SOHM =c\1105544=@\EOTp\b\"S\"\995485LDr\995346b\NAK\1027766R\4921J\15151f\1037065\1062286@\SOH@\r\DC2w:G\tt\151337\v\687>(*&=\DC2\137356j"}), inInviteePhone = Just (Phone {fromPhone = "+485874000"})}], ilHasMore = False}
testObject_InvitationList_team_4 :: InvitationList
testObject_InvitationList_team_4 = InvitationList {ilInvitations = [], ilHasMore = False}
testObject_InvitationList_team_5 :: InvitationList
testObject_InvitationList_team_5 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T16:51:35.956Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1039236\9335M\UShRB\NULC[Yu\ETB\n\141101\DC3\DC2L\1001018[7\1069876\DC4s51\1069470E\1033657\1019030sqA\98134U2\194652t\ETBG{+(\NUL\1002894\"\ACK\ETB_b\19445\154164\1060742qB'y\7463\GSj-\166450'\145102\SO-~\SOH\1008268c{DYa\1104902Y0\1016096h\1047744u\ETB\DC1XN\1079914e\10837\67293\a\156728+;<"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T09:08:38.852Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "A*s9&\121469N"}), inInviteePhone = Just (Phone {fromPhone = "+0499245345"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T23:57:06.767Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "IN\STX\8609/D(~l7\STXK(\CAN\36861\&7\1063897"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T18:00:02.817Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1068244!Q#x&\NUL\16015tQ!\1113515\tH\5445.Im\DLE9O.PvZ^^/^]h\v\"}x\72346=Q\DC2\FS\NUL\168452\134568\DC2)\1024784#\167735UP\EME\14734@$0\1099965UZ\RS\11808\6671\RS\1084944$\18162\985298\&2"}), inInviteePhone = Nothing}], ilHasMore = False}
testObject_InvitationList_team_6 :: InvitationList
testObject_InvitationList_team_6 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_7 :: InvitationList
testObject_InvitationList_team_7 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_8 :: InvitationList
testObject_InvitationList_team_8 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_9 :: InvitationList
testObject_InvitationList_team_9 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_10 :: InvitationList
testObject_InvitationList_team_10 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-08T12:15:04.529Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "\\", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+115892419997"})}], ilHasMore = True}
testObject_InvitationList_team_11 :: InvitationList
testObject_InvitationList_team_11 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T13:18:23.644Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = "\SOH"}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+26541724330"})}], ilHasMore = True}
testObject_InvitationList_team_12 :: InvitationList
testObject_InvitationList_team_12 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_13 :: InvitationList
testObject_InvitationList_team_13 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T15:13:22.189Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\DLEq\1007341\12650\ENQ37\161540\1113455\143574\b\NAK\1031648-]Bme>\v\DC1\ETXP|c\7578T\144976\NULj*='\GS\1056870\STXyZD\1075035W-\SYN\1066459\152313LOW\21326o(\STXt\NULn\SOHD\1072738yD\171513x.\97479\1092990y\120416\97509\1083332UR!kt*\182678zU\SI"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T00:03:40.855Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "t\t\1083604ZY\1108237\1036600\1053217E&9B\69670s|p"}), inInviteePhone = Just (Phone {fromPhone = "+0488723791"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T13:57:30.236Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\v;l39v\1074843\&4;(\FS\155463p|JD\n)\1046433N9\ETB\CAN\44156g\RS.\178016Z\NUL^V\SYN\ACKfV\DELV7\"%\1085434~J\68752>\f!%\DC4(\95436"}), inInviteePhone = Just (Phone {fromPhone = "+19955027081"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:22:56.176Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "=Ne\1064006\b^\60044\1028115\NUL\DC42b4\1093961o^y\1014680\1019897\992680'\NAK\163178^\NUL*Bm\RS\1060636zm4\SO-\155119n\DC1\133016\CAN\999069TY\1104120tX\11067\1011455ZoYB\SOHfb~\r\SI\DC1,:\FSs\131417N\SOHl\99040\v:\r|t\ESC5h\543\994185\25423zk\DLE\11838\DC3$_\1019047:(\1066620T\141189\1629<\1052925\ENQ\143836\1087090\&9\1050332|\1021915\&3\22217\RS\ACK\1070433lw\SOqa\US\DC1"}), inInviteePhone = Just (Phone {fromPhone = "+66390219"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T00:10:43.572Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "Rl\ETB\25881\&9_rF\1104286L|\ETB$%jo}T9\SOHo"}), inInviteePhone = Just (Phone {fromPhone = "+10906501025"})}], ilHasMore = False}
testObject_InvitationList_team_14 :: InvitationList
testObject_InvitationList_team_14 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T12:04:20.844Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "IfI\vN?0xx\179525P\ESCK&|\46892c\DC4"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:47:14.230Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1014099?{2S\1099047\1000045\28385 =\DC1\14524\1016029^EB\RS\29566/\1042292\36330{\162110\CAN\1061949\6360\16580v\1011L\STX)'\15876W\1101412\SI\ETX\1029838Vd\39199\1069951D\nd|G\174621BVF\DC4Yq\133343CN\v\1068281\58837\1055927\1107145r\NUL}Xd\RS\31684,\ENQ\165565\98718\RS\1093390\1028267\1000337II%=\ETBU\SUBvQ\1011766c>%\1011441\1060769\&9Y+ \28705{\190373*uC\164796?\1056221DfJ\ETX\110729R\1059164G\NUL"}), inInviteePhone = Just (Phone {fromPhone = "+28696693002"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:06:58.369Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1087871EPa\171557\1070044\DC4%\1018929Q\155357B.\1006417)\NUL\4793S\176673G^\1063521\51225\t&rt)\t2x!\1074065.\n~\CAN.c7\b\SUB\1110886*n|!=P?Ez(\bf\1056730-#\161321Y#\66255gn\1061556\vLG\137620U`]7{pjD"}), inInviteePhone = Just (Phone {fromPhone = "+949184961"})}], ilHasMore = True}
testObject_InvitationList_team_15 :: InvitationList
testObject_InvitationList_team_15 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T00:50:40.166Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "\ENQ", emailDomain = "\t"}, inInviteeName = Just (Name {fromName = "-s\vN\178956\DC1uX\SIH5\1007359n!6X\123196=\vAq\132405\&0\f\189414\\=;Oru^VP\1082685w`\153441\fT\CAN,I\1056594\SUB\991533\153352S\bi\DEL\1081117\154723%\SOHC\f\1022414%J.J`h\1063699d\149010\EOTK\n\STX\68839f\145442<\1027186\1062617\10377\t\993031\DLEl\94471\&7}\48400\&3\DC4m?\986228,Y\50418k\DC3"}), inInviteePhone = Just (Phone {fromPhone = "+11095211389823"})}], ilHasMore = False}
testObject_InvitationList_team_16 :: InvitationList
testObject_InvitationList_team_16 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:48:52.879Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "u\1069748L3l\158744\1061684@+]7]\ESCBVtj\a\1006039\GS\1051546\986806\1073389\1001868\14865\143837N9;\179712$\1031012\1035136X\176693Y:\1024988k\69900\133257vUc\174056q6\163564\9836\ACKNY+\SUB\DC2\STX;\n\986463\987009'[!\1042975\DC2\1067279!5;\SOY!{\1093782.o \DC4\59216b\1059827\rh\EM)-G\74754'l\DC3\SI\136037\SIUt\1063835\&0\FS<\34687\b\ENQ\32459~t\47452\NUL2"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T11:46:20.290Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\97361\&9\DC1\RS\SUB\nE\ETX#\1109709Y+B\13372d\"\fgS\STX\67372\93971\100013[\ENQI5~?^'\CAN<Q\f\DEL\DELj\ENQ\"iu}\DC1a]\1078387\ETB8Y\DC4\DC4dm$\DC3H'<\21852\DC3z\EM7\t$w.7D1\f1Im(\1086715\1066066\ESC\13518\\\66779A{i\1107201\65895\&6"}), inInviteePhone = Just (Phone {fromPhone = "+84898012267"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T00:12:48.374Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "=\1089472!\SO\SI\DC3\54352\1054416\ETX\ENQ\CANC\f|q\SOgw \DEL\177419\fOylE\174069G39\156820f\DC3ph;\SO\DELo\36099tYh\FS\ai\129406\27355\FSf+e\DC2\a\183497\r2_\1008950b$8K\24602^\1053417\bWDc\ACK|\DC3H-.\1014196XW\DC2\68494W)e7\EOTF"}), inInviteePhone = Just (Phone {fromPhone = "+5072758560848"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T17:20:14.733Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\DEL\1005744\1089585DI3\DELAN\\9K\ETXPi\1007108d!3\1079860\ACK$e\136206sxA\SYN\1044197D\USO\SUB\ETX\t\td\1021722l\1061981z\1006133\&0\SYNy\FS:\1092191yM\7076\a\ETB\DEL\USQ\7592\&5/il\b\NUL{T;\DC2\49156{D\DEL\ENQ\FS\SUB556qL7\CAN\CAN\41798YIO\140249#\165549\1111103\b\DC3X\FSEsA\180505t\1092007PiH\ETXC\SOH\1035571 Hv(Xp0g5\ACK"}), inInviteePhone = Nothing}], ilHasMore = False}
testObject_InvitationList_team_17 :: InvitationList
testObject_InvitationList_team_17 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T22:14:19.348Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+19378425"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T22:41:49.765Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\1003318M\ETBBR8U\DEL{S\ETX\57608F\28902u\1063165(]\US!\aHf\nx\1026872)k\t\170407!.\EOT\1055682\165220\SO\133727\1048688\1098638Utr4\b]\168154z\1078742\ETB'Z\153466\1733\SO:_\NUL\1002369\33988N]\1092760fe\DC2\180626M\GS\NUL\ETX\70082d\74379\SOH5I\SOH\"U\SOH#]D\ESC\1047316\1083774,\1100879\1038869\171266T=Zn\1047878\52550Y\ETB58\DC1\SYNj\141028qYmRuL\ACK\43412i\1088422\r\64932\24674\US/\1088312gR[\DLE\1055378"}), inInviteePhone = Just (Phone {fromPhone = "+28953928121"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T04:46:06.216Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T07:31:39.140Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "rA\EM\168691\&13o\DEL2w=]+G?@Z\1002678\EOTd/\"KCg)K\125030\32298\DEL{A;\1032873\155574\1068880&\"/\18050\128065XC\1043506\a\989119"}), inInviteePhone = Just (Phone {fromPhone = "+263974605"})}], ilHasMore = False}
testObject_InvitationList_team_18 :: InvitationList
testObject_InvitationList_team_18 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:07:40.948Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\SI(\ETX\n\SOH>\FSxi#2/U\DC1\152808\SYNU9g\1058722z.x\SUB|\DELr\53807L\170405\&4x@\1109658\47076\&5\EOT2\SYN\1087710\&6\NUL\EM\NAK\1001141\&0\\\NUL\1107051'\147441_\SOHH\ESCFB\CANu\"7^\rY5\a\15718,\40251\1048391I\"fQd}mfC\1098944=x\STX<\"\ESC.\178482_Tp\EOT?\STX\EOT\NAKk\60126\ACKq\SI\98175"}), inInviteePhone = Just (Phone {fromPhone = "+01042525978473"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T09:57:13.375Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "+\fw\1098430W\1015777\EM;Z\ETX\US<\49945 ~91\tq\181969Y~\v\CAN*5\b$!\45600\&6\1005154\a\ENQt6$\\n\DC4`m/\190871K\16560rg@\1028886;D\SYNV`g\NAK\1002352.y~\1005562Rr\990528Sg\EM\US\989013BE\120175L\t\1033096\t]\142938\DC2\DC3\ESC\1045756X%"}), inInviteePhone = Nothing}], ilHasMore = False}
testObject_InvitationList_team_19 :: InvitationList
testObject_InvitationList_team_19 = InvitationList {ilInvitations = [], ilHasMore = True}
testObject_InvitationList_team_20 :: InvitationList
testObject_InvitationList_team_20 = InvitationList {ilInvitations = [Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inRole = RoleExternalPartner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T17:37:53.690Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Nothing, inInviteePhone = Just (Phone {fromPhone = "+28200224"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), inRole = RoleOwner, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T06:38:47.152Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\133086\DC1\DC3\20795BhS3\NUL\174358i\41092D:a\78144\178925#9zL\DEL"}), inInviteePhone = Just (Phone {fromPhone = "+294077724316079"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T23:41:01.341Z")), inCreatedBy = Nothing, inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "\161937wykwM\FScH\1093109,2Vc\a\STX39fg\SUB\SI\ENQG7^l\917505Ey\149254|Yk\1060356wz(\DEL\DEL\DC1(uK\1106180P\62353\&9\ETX\EOT\b\1010428\128190xw\1007149\alDQ\194949V}F\ESC,\29620\SIB'\92560e\DC1\55286q\SIW\164731\t\1004752\1032704\NUL\fn:\1013756]\ACK-v\DC4j(\143582QKw8/\DC49Q\a\SYN(X\1093312g|]\f\138692\189209`\62638,\"C9\SO\FSz\60797\13878R"}), inInviteePhone = Just (Phone {fromPhone = "+3702569826"})},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), inRole = RoleMember, inInvitation = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T12:22:34.203Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "?t\1049138zx\47772\1035747n\b\DLEa&=\1095744*\t\f\1002713\1004061b\167203\142381s\50742\SYN\NULp\EOT\GS\DEL\1051363\158001b*\1090978\US.\1063919\984549\1076681zM"}), inInviteePhone = Nothing},Invitation {inTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), inRole = RoleAdmin, inInvitation = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), inCreatedAt = (fromJust (readUTCTimeMillis "1864-05-09T03:52:07.084Z")), inCreatedBy = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), inInviteeEmail = Email {emailLocal = "", emailDomain = ""}, inInviteeName = Just (Name {fromName = "K\ENQ$4\32447\RS81{\a|?L~d\182068\v\128869h^SBV\GS\143436p\ACKH[\32346'\47277Sud7\1021779/\DEL\998794j??3>\1074400\148218$\EOT6\183246\60862\CANr*p\1057454\161744 (\1030663\f_\DC27\158736\ETB+\46842K\SOH8\4273OqL+E\SOHr&\SOW-+B] |"}), inInviteePhone = Nothing}], ilHasMore = True}
