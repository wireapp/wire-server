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
testObject_Event_team_1 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000019a0-0000-7c55-0000-48f3000043aa")))) (read ("1864-05-24 19:17:14.545982076 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "0000411c-0000-2add-0000-119e000064b0"))))))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "0000414b-0000-46fc-0000-2a500000214d")))) (read ("1864-05-24 01:00:12.533609748293 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00005c31-0000-37f1-0000-444800004804"))))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "000037e8-0000-53b0-0000-0cbd0000778e")))) (read ("1864-04-26 03:07:50.972735890767 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00000e00-0000-3776-0000-067a00002024"))) (Just (Permissions {_self = fromList [], _copy = fromList []})))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "00004308-0000-2dae-0000-3b5a00000d62")))) (read ("1864-05-25 00:21:10.943516621226 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00005269-0000-5000-0000-2fc100006887"))))))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "0000632a-0000-24d5-0000-1676000056c3")))) (read ("1864-05-10 07:02:00.976873465897 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "000060b7-0000-439f-0000-1d90000069ee"))))))
testObject_Event_team_6 :: Event
testObject_Event_team_6 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "000051fd-0000-5c07-0000-6de3000001d0")))) (read ("1864-04-24 10:01:16.881073295611 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("qm\DC2.\tw\DLE\SOV\1074489\NUL\CAN%0-6f\byb(bk\1024633NQPu\USw\DLE\vS\SO\1094206x4\fi\1037476M\911Y\1027085\DLE\37793a\"\t\135110!\1102995Uglf\991190 \182810{\186425\SI \1094886\SYN\45155\1106875\DLEpL\v\SOHp\1006306!}k\31148\ts1&6R\19707\136624.E\1054281\154544\997940\7558O\1099803\154826\&7^")), _iconUpdate = Just (unsafeRange ("7\1088922@Y+O\ETB2FDz)\vZ7\v0\1083770\&5kM\SI}tnx\DC4l\DC3\1068541\1044294s\95419\1072759\&6\1101411]]\fd")), _iconKeyUpdate = Just (unsafeRange ("\NUL\25730\DC49\DC1\1015898_6rM6\t\NUL\EOTm\DC20>Y\1103174\190683r\DC37^\ACKes6-\149498\1054060\f\128622\&9\SUB\37160\189693\1083587h\187661\tqZ\US\145473\1025485\RSZ\190674YY\1012743n!S*\FS\aG\1104373\DC1[\\j\ENQVs\1108445\t\1093029m\61309J\CAN\NAK\ESC\GS\30020\&4v]YoN\152010@V\DEL\US=4C\SOH\78284.bVZ,2\986593\SI0\1037615\DC1I\1101447\&4z|\td\DC2@V\DC2\1031261~\169376\190128\&1^y>/#\SYNR:\155118I\DC3s],1\SOH\FS\1011003\STXv\1048533\128711\EM_\DC1a\USyW\US+\1090006\9464h:\1082164Lqs\1041133@b\52716\DLE{\"\181117\bF=g3$>\153816\NULvr\SUB\ESC.P{\1104475Z\ETB\a=Ux\175904`\1033279\61288M:\134576I1Gk4\53409\66015\1051992\66754c\170398\&3\nf\NAK_-&"))}))))
testObject_Event_team_7 :: Event
testObject_Event_team_7 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "00005982-0000-0a60-0000-1d1100001b46")))) (read ("1864-05-26 02:14:24.232692348743 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00007a38-0000-5d86-0000-31c200005b1e"))))))
testObject_Event_team_8 :: Event
testObject_Event_team_8 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00004cd8-0000-5b30-0000-248500001959")))) (read ("1864-06-02 19:57:44.022837373893 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000000")))) ("\fR\\") ("\a") (NonBinding) & teamIconKey .~ (Just "\1013163$\1032058Z")))))
testObject_Event_team_9 :: Event
testObject_Event_team_9 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00004c3c-0000-1932-0000-60b900000c0a")))) (read ("1864-06-07 20:59:35.003289782864 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00005ea6-0000-3933-0000-29f700006851"))) Nothing)))
testObject_Event_team_10 :: Event
testObject_Event_team_10 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00003780-0000-4107-0000-55c0000010da")))) (read ("1864-06-08 07:45:26.111391323964 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000003")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "T")))))
testObject_Event_team_11 :: Event
testObject_Event_team_11 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00002faa-0000-420e-0000-554e00003362")))) (read ("1864-05-29 10:38:43.319766304404 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")))) ("\166859\1052194-\1022699") ("") (Binding) & teamIconKey .~ (Just "s")))))
testObject_Event_team_12 :: Event
testObject_Event_team_12 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "000049c9-0000-249b-0000-2e23000071d7")))) (read ("1864-05-28 14:43:01.312893484267 UTC")) & eventData .~ (Nothing))
testObject_Event_team_13 :: Event
testObject_Event_team_13 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00003310-0000-7b64-0000-351e00003a71")))) (read ("1864-04-18 14:48:11.966711338842 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("$\98716\DEL\SI\21403YY(\5745 \143617T^jg/\55259Q\ETX\v\169880%j\ETX\150760\1028129\177044it\78220\SOH-29u##\b\SOHj9\rA__\15467X\DC38~\fl(\138588\166322\&5&\989010\1095242w")), _iconUpdate = Just (unsafeRange ("\RS\147122\DLED'E\992022\168389\991146e6CpoA\1032474\&7qsG]\SOH\1088415\139562Xr\1073418\ACK\t\157945fBvT}u\bLh\DEL))\NUL\162663w^\RS\1037533\10837\&83\14109\7472\ENQ0\1053312\&85_E\128034\66483\"\t\1111815\1102713\41085\53934c%E6\EOT\9187\&9\SYN\DEL?v} \ENQ\1046221\DC1m \1089108\vU\1042971Z=*\145195\991026\14580")), _iconKeyUpdate = Just (unsafeRange (".|\DLE#10\12429YAa_\70810L>>*#1\60343$\DLEM\RSFUAi\1062967C\DLE\1102448-\1078484\SYN\SOH\995653od}1\b$'s\1070774MP\1108617\184487\1061416xN|r\129184\140614jS\NULq\1050140O\180927\165449\162080\f\DC4\996854~\1045724\186895\1083609i\1010242\b{E\a\DEL\1070301\DC2~^fa\995688;|\1106142t*\":_c:K\1049278K8SQ\SO\1066624\SO%.\SYNA\\k(\v$b\UST~\"1S7*|\SYNk47\5218\167614a\SOH\ETB\53339b(\1022682.I\17589f\DLE S\43497O:\SUBt\986831C\SO\ENQ7~\992826aD\1080539\1109264I32)\8336\NAKv$\26218j"))}))))
testObject_Event_team_14 :: Event
testObject_Event_team_14 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00006d62-0000-4804-0000-3875000005a2")))) (read ("1864-05-04 22:41:27.754743280427 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00003a10-0000-6778-0000-0df600000ff8"))))))
testObject_Event_team_15 :: Event
testObject_Event_team_15 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00001e56-0000-23bb-0000-53e8000000a2")))) (read ("1864-05-23 10:48:01.80383530588 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00000de0-0000-7804-0000-2aa400003a32"))))))
testObject_Event_team_16 :: Event
testObject_Event_team_16 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00003b6a-0000-725f-0000-276b000006c9")))) (read ("1864-04-10 07:00:48.381506018504 UTC")) & eventData .~ (Nothing))
testObject_Event_team_17 :: Event
testObject_Event_team_17 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00001830-0000-2759-0000-1d2f000002b5")))) (read ("1864-04-28 14:50:59.215637873137 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000000")))) ("\1014423\1047131lo`") ("[z") (Binding) & teamIconKey .~ (Just "c*\30583\SIf")))))
testObject_Event_team_18 :: Event
testObject_Event_team_18 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00003aab-0000-62a8-0000-5d290000086f")))) (read ("1864-05-01 08:25:43.138943354086 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00003557-0000-7c0a-0000-1df500001134"))) (Just (Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,SetMemberPermissions,DeleteTeam]})))))
testObject_Event_team_19 :: Event
testObject_Event_team_19 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00005739-0000-77a5-0000-22bc00000f96")))) (read ("1864-05-14 14:01:26.436605549773 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00006c2f-0000-762c-0000-5a5300005bf4"))))))
testObject_Event_team_20 :: Event
testObject_Event_team_20 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "0000571b-0000-274e-0000-2faf00002790")))) (read ("1864-06-01 01:19:44.334136845126 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\t\NUL\DC4Xuz\"\1057998U\66514[\167448d\SO]C@]{\SO\28292\1089513V\62103zq|\DLE\ETX\93789$\ACK\ACK;\US\143957\DC1\v\93060cnF\1102348&\ESC_m\38541\&0T\fy\12549]Y\1078863v?DF\46342z\1018078p\vvR<1\DC2:\fnXbQ")), _iconUpdate = Just (unsafeRange ("\SYN\a\ETX\1104167\\\nGl\1110610ZD\1057184\19817K\b|>=\1047305\34377KBIfk\1008293\ETXL#\44568\18006\32496L0g\42517\1090495d\NAKCAu/`i!\EOT\6347\&2CE(n\t2\EOTo\USS1\1068397F(\GS>\DC1\SI}\ETB\119002n\1084870\ACKrAt2\1087139\f\1047278\1048298kmx:1\CAN\4295\1089723|~\182743*\990999H\142929m\1030596\US!\50557\987396\154145\1034818\DC3p72k")), _iconKeyUpdate = Just (unsafeRange ("FUL\DC2\1052425f\ETBOX@<\SUB\SYN[cgvp\ACK{\1056501g)SlC\SYNA\39972\t\184954\NAKU<#/$\31239\1089015\DELp\133688\&6\78818\&9'\78382-p\984616j\EM^0\3544\1098555\133908C\990319)\156113\1101477\ESCugb~\CAN~\f\DC3]\f\EMIc0\a? 5t\EOT:/oOsN\\B\EOT\95273;o\ACK\r\NAK\DC4@&\SYN\97279LD\4734}O^+\vls_-*T\18301M\DC3^'y\34318\DLEG\14201\97545\&8\SO\1018694\59277o\184987\1053748TP&RJ=a\DC1\6715!V\145549\&7/v4\48983\t\160298\142751P2c\SUBc\159936DP\173401\RS\1038085\73859gRf\163992g\15827\NAKv\99176PGN\DEL\1080795\\\98601>4aJ\47074\NULG\998719X\1019467\137564Mk\26130\&90\DC3\NUL0\164223p\r-\1092832$\SO0\177055w\1018242\CAN:.L\1000340\1055195s\ESC\30278\DLE)K\984082\&5~ \n9\ETB\1091488}EQ\143281#{Z:\DC4;V?}Fz\bg"))}))))
