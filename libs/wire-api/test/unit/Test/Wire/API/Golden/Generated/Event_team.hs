{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where

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
testObject_Event_team_1 :: Event
testObject_Event_team_1 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "0000078d-0000-75ce-0000-574e0000553d")))) (read ("1864-05-22 19:19:18.959937580351 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "000002e5-0000-4769-0000-6d2800001ac1"))))))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "000029cf-0000-3a57-0000-004b00001613")))) (read ("1864-05-23 01:35:13.373578613794 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "00002a72-0000-131f-0000-6e1e000079ea"))))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000006b6-0000-1bfe-0000-03b500006e9e")))) (read ("1864-04-19 15:02:37.836601193666 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00006ca0-0000-59f8-0000-268100002c23"))))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "00004e77-0000-1d10-0000-6a8a00007c62")))) (read ("1864-05-09 03:03:17.135803742963 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "00002ffd-0000-4b6b-0000-7d270000670b"))))))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00003a9f-0000-67a2-0000-38e2000008b3")))) (read ("1864-05-09 07:29:00.434822379505 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "0000232f-0000-2a31-0000-2acf00000219"))))))
testObject_Event_team_6 :: Event
testObject_Event_team_6 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "000039b0-0000-2e30-0000-1700000075b4")))) (read ("1864-05-07 05:22:55.909870703806 UTC")) & eventData .~ (Nothing))
testObject_Event_team_7 :: Event
testObject_Event_team_7 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000022e2-0000-1479-0000-01f500001be7")))) (read ("1864-05-05 07:42:43.107910904967 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00005447-0000-6307-0000-580f0000054f"))))))
testObject_Event_team_8 :: Event
testObject_Event_team_8 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00001b3c-0000-3a75-0000-79fb00000706")))) (read ("1864-05-17 04:11:06.940395952557 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00004d1d-0000-7f42-0000-49440000177d"))))))
testObject_Event_team_9 :: Event
testObject_Event_team_9 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00006d0d-0000-72e8-0000-69e700005221")))) (read ("1864-04-18 02:02:38.428154476962 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004")))) ("\1079070") ("r\58621*m") (NonBinding) & teamIconKey .~ (Nothing)))))
testObject_Event_team_10 :: Event
testObject_Event_team_10 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00002161-0000-607f-0000-326500003aa4")))) (read ("1864-04-26 14:04:31.982291187552 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\1107985X0V\44388\EOT\NAK\FS'\1083890\1088069\DELoIq\b\DEL\DC3\ACKO,l\t\49177\SOH]\917574nw\tK\176799:\151374\DC1\1004378\1038520=\ETB\GS\r(\GS\v;\aZj\r\SO\44158.b\v\ACK\DC4\\8\bWX\EM8d,\r\NULpV5\v\ESCql\1042883\bPq\STXZ/0l+|r2SIl\51309\12463\faw:\998395\9790-\ETBLn`\1074891!\SYNTkQ%sH\ACK\FS\GS\ENQ\ACKe\1058014T\aj\ACK\ACKl93\999805:M]\113762^!\USU\1097382o\SUBJu|\SYN\127165r\137962\DLE\999346@")), _iconUpdate = Just (unsafeRange ("-/G\1096986\145554\STX^w\1092693\&2N\1077213R\NAK\177638s6+rw\1052778\&5\ETB\111096G\169048N\1000981\1060779\ENQ\CANW\DC2\ENQ*\83506()e7aQn\1015215O\EM2\EM\68858mu\1088924\ETX\1097057*V\ENQk.W$\31857z>\97130\a\62883\US\12877\143942XhhF[# D\ESCE\ETB\ACK\1044571\151554\174602JF\32960A'F\NULQ_|\127174\93800\1026904\&7\131104<o\1063414\v\49080\1035888\DC4\ETB;e\f\135945/\1061321)\986859\&0NW~= \USY\EMD\NUL\ETB\1048359\STX\15203\NAK~\1008182;8\f1\184388\1078570$Pe\DC3\1068415\984707P\\\1050662\SO\191041\43422*\987384x\ACKbG/\187755gO;\RS?l!\987416\GSZ\1087623\DC1&\1093369\157325G\1084233us\1005049'")), _iconKeyUpdate = Just (unsafeRange ("\EOTg|\DC3]L\194763``\DLEu\48649!P{}*:\SOH\44147\1042608\&7dn\6203\989547\181263\58288Y/\39713\1014258_'/BT~\1037749I\1017569!\95990\135057nm\26201p\SO^w\ACK\173681*:-(J\172132\1089710\SUBZr]\b\1087505Ib\110774K\129610\183194\ve\\p\v\b<p\1073007W\1003336\&9\185730y\ENQ\43078qvC\ETB\8173O\983442\1101402\142274@\NAK/8$\SOHu\SYN>\RSL@\194835f{\1003837\STX\169242\SOH\1076128\f\SI0 \STX(o"))}))))
testObject_Event_team_11 :: Event
testObject_Event_team_11 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00001f9b-0000-176f-0000-4f7900002b90")))) (read ("1864-04-21 17:29:58.558632721753 UTC")) & eventData .~ (Nothing))
testObject_Event_team_12 :: Event
testObject_Event_team_12 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00003f99-0000-34dd-0000-0e8000007f81")))) (read ("1864-05-13 06:11:56.351813303852 UTC")) & eventData .~ (Nothing))
testObject_Event_team_13 :: Event
testObject_Event_team_13 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "000020e9-0000-14f9-0000-48f90000207b")))) (read ("1864-05-31 04:54:41.679169051727 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("T\43324\62286h\187002G;\54467\f\1002016i\1020822F\140520/a5Y\ETBAS\DC47;BPp\DC2|\1040982\19772\v\ETX\1094225\SO\DEL5k\1038396M]\US\NUL!\45024o\1044175\1010240\173998V:,\SI\47787\f\1028747f\n\1094028\DEL`H\1030048jv\ENQ`\20755XZ\189943F9\1104490$\STX\1084079n\158331\183719\STX@\SI\vl<=\61943r`w@w\1074740\ACKn\184967\DC3Hx\1109223$\"V\DC3\186986\170242|\19821\FSu\987733\34229ADt\SO\118807EDx\9041\US^c+\ESC^\1091725\183887OM\r5Wy/Ol\182793I~\31357\37728N\44646\67288\1078796H\139218u=\EOT\1250;c{v\EOT;n:\DC1:\RS\ETB=\138959I/\f\NAK\1013847\136569\DC1u\1025766-O\DC1\1088318rhE\29364\1098419>I\"'<\FSm\13158[\nxY9F\170025\&4\1092787H\1033945(\1109615\&6\92714\ESC'v&\NUL \GS\STXL")), _iconUpdate = Just (unsafeRange ("X\STX3>>\b,\3596]}Z#;|G\7719\60372<\ENQ{\US\999156}\1039543}C\174137HN91\DC2\a\1046281\DC3\DLET~\1008417\95068\&8=h\1095396}z\1103518\1054265\DEL),\f\CAN\13621-Q\EM\1047780V>\DELC9\ENQ+\993276(G\990738\28119\t(\1099426\ETBw%\42312e\23137\28466g\GS&\t!)r\b\7187\1085042k\99539\&7\DLEH}\1088757\&91T\1039056\98156r_/\USO\ENQ\1103453_\t6&.\SO1\176102u\43624\SUBqm#qL\174645u\DC3\161994\1036304\&7E?\b3T\NAK$Qu\1099028pK&\1103873\1098758\&3\FS\rda \52477\GSx\RS\991681@3\65347[\14890iS;.V]\169033$pH\1045225\a\144568D,\SUB\CAN\a\1078974P\67176\1087911\&35\EM[@\1055673Xb%\1096477G}\62287\131074v\ETB)\54135\1103516\1105386c\SYNY+!g9K(7Z/\135233CY0\a\5467\"s>\190188\46714\1088062\DC2d\SUB\EM\SOY\US\1085404|\43259;\NULW\US,\ESC")), _iconKeyUpdate = Just (unsafeRange ("\DEL=FE\ETX#qd\998052\&9\164763@\NULl\996467m\1029678RH\STX\ETB\1073946\1111434R\DLE\DC3]\ESC\77951$O\DC2E]N\a3xFXs`\NULET!:z\SUB\STX~<\GS\1013616x-%\FS#\DC1-wLt\67845LMk\NAK~u8\SOH\DC2\f\131400\r[Bk\1075784\US>U\67600g2\SIJ6\145284LH*qU\1059563WZ\1001889a\DC4/\USr$\CANv\DC4}#\48753\DLE\v\SOL\DC3\f\999240c\78771Xw\v/\vUo\59874\1074050\US[(D=ZR\SUB-I'\159954\1102851Y\ETB\129528\FS\vm\30901%6dw\DC2s2\175111C\ESC\151548[\67140g\1032815t\184791r\1033256\fs\58603\ACKH\178868\1001932Ylq\65684Q\SI\SI>\1073104\a\1011698:J\DC44\NAKz\SOH*\189259.=jN7\1112785"))}))))
testObject_Event_team_14 :: Event
testObject_Event_team_14 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00005f61-0000-3435-0000-3a780000085c")))) (read ("1864-04-17 08:25:22.886663025026 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00006e28-0000-4e70-0000-66b400003eba"))) (Just (Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetMemberPermissions,GetTeamConversations,DeleteTeam]})))))
testObject_Event_team_15 :: Event
testObject_Event_team_15 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00004aae-0000-09e0-0000-665000007b00")))) (read ("1864-06-05 14:27:07.054056553242 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000003")))) (">\CAN\RS") ("X") (Binding) & teamIconKey .~ (Just "")))))
testObject_Event_team_16 :: Event
testObject_Event_team_16 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "00006c7e-0000-15cd-0000-602500000291")))) (read ("1864-06-08 07:05:40.889271268693 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "000006b7-0000-65b6-0000-770d0000597d"))))))
testObject_Event_team_17 :: Event
testObject_Event_team_17 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "000028d1-0000-7223-0000-32b100004966")))) (read ("1864-04-10 12:38:50.85577148429 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00003f00-0000-7896-0000-0aef000036f9"))) Nothing)))
testObject_Event_team_18 :: Event
testObject_Event_team_18 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "0000092c-0000-2fd6-0000-53c6000019db")))) (read ("1864-05-16 12:47:50.067570217361 UTC")) & eventData .~ (Nothing))
testObject_Event_team_19 :: Event
testObject_Event_team_19 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000071ad-0000-3327-0000-3d600000123d")))) (read ("1864-05-23 12:49:06.970184715202 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00000903-0000-7731-0000-607e000066d3"))))))
testObject_Event_team_20 :: Event
testObject_Event_team_20 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00000581-0000-5159-0000-1cae00001157")))) (read ("1864-05-25 15:12:24.680509239587 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00005c43-0000-4159-0000-40ad00003f3c"))) (Just (Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,SetMemberPermissions,DeleteTeam]})))))
