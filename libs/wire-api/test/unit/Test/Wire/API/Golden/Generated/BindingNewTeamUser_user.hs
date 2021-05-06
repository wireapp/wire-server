{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BindingNewTeamUser_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_BindingNewTeamUser_user_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_1 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("k,'@R\999000RDB\95406\"!nC&TL\43084ubAE\1096848sc\STX\143091\150470u\998317\1059609\1106918k\1027682\1068540\169294]\8234e\1054416]gr\176942\ESC\DC3\SII:=\DELE\"\SIL\"$s|r\NULu\aG'\SIK")), _newTeamIcon = (unsafeRange ("\54303!")), _newTeamIconKey = Just (unsafeRange ("<^~\US\ETXM\US\DLEa\EM+hG\DC4\1021578\25618s\\Z\DC2g\177169\&2P3,E\ACK:R/\984346=|[\16140\44113j\162666\1095611~d#~;zl5\NULW,\61934\&7\1084793t\67296.\fn{#3q8}\191049\994857;M\47337h0\174540.f^BnyF\1094046\STX8\1012145@\33568\&5\ESCr>((\185797!\NULjh\USk\DC4IVN/\1007701r\DC1\1045834\SUBeu7uBa\CANDx\180424\168200\DC19\bO\1021685qr-R4b\1016275\DEL)\GS;I\RS\159650\&0_yzJ\19327b/\DC3<9\177222`\1050838\74988~{\ENQ\1010003A\119065c;A$S\1083466\1040512Cr[y\37612\\\150441\1075207\1080422I\DLE\1015256\42770.\SID\GS|\SOk\1076037\DLEW\NAK8\STX%'+\SOH@G8\993058\EOT\985904:dW\1045887\1061784\&3Mv\ACKk\f\r\1037285\1016918:_\GS\bDfK\1028619,\173266\30233\165363\RS\1036092\SYN")), _newTeamMembers = Nothing}), bnuCurrency = Just ERN}
testObject_BindingNewTeamUser_user_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_2 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\156572CISjry\996630\1111770\1013324\GS.]#{\ACK}1\ENQ\CAN2L\1003519Rc?\r\191136z \1084506\DC3c>\151626U\ENQ\17175at\161461\EOTo\ENQ5\ag\SOHr\165325C\138084\158322o\1057031R26LLquKV\1047703\US\172548\n")), _newTeamIcon = (unsafeRange ("4Q\RS\DC3\1060158\1054480\44444\1074136w\RSZ+esb\1036898\120427-#jh2\"uP\166788\SOH\1045694E\985777\NAK\189207u\1005314):\1055722\ENQ\1003940z_*\GS2\FSI\147775\b\999221\1108432Ov+7j8\GSeK\n\DC3\STX\ETX*\993823\SOH%aW\DLE'W\11353\\\991304v#&3\27489v\a\15900\DC3r8#\1089004;M\\[\1001228g F")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just SGD}
testObject_BindingNewTeamUser_user_3 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_3 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\ESC\US\1040625\1047630\1080863'\1098351\CANlm\146180E*\1031147\1025899\STX\49840!\GS\DC2~\54847~p\DC4\1043623fo3s6\137845\1075023RN;2\ETB\ESCJ\165483\1001686l\147063b:\185667=u?\54893J+`\177421\1005012mv\1038\\c<:\DC4\DC1+/r]\163118j\DEL\60618j\SOH\DC3~\1077789=\ACK\1052816?a<jX\ETB$\DEL4f")), _newTeamIcon = (unsafeRange ("wZ^D\ETX~n\132707}`h'}U\83431~1\170835\RS\1078425u\1084950\SIGq\GS\r\DLE\1096019\"if\1069806\EOTZ\1050706\SOH}0'\1035129EL\140279f\1053413sv\ESC\1109313\DC2I\127037\&9\SUBP-ae\"\CAN]q\SOH20P\b\51425 \NULB\DC2\41777N9\987054a`\CAN\1086519\"\157426s\EOTV\CAN\991618\DC14\62823\US\161822)B\1080169\GS 6\GS4\60071$\128655<U6\1080062\141453rUSW[p{\SOH(\DC2\1002342ZF\71921M[\74352\STX\b#0(@\151420\24468\&5\985737:Q\1097196\1076744,\GS")), _newTeamIconKey = Just (unsafeRange ("\1098214\NULUl<^-&\1051821O\1090348l\54716?^\1027374@\DC1\96587*%\n}\t\n<\1065356n\984327\125041\164185\DC2\DC2e<Tnp\1046518DB@\ETB\SO\DC2v\FS;GM\139840[\167318Y\DC2\CAN\EOT%Z9zt\1073691\t\DC2)(\SOH\1076614\r\74794?\\")), _newTeamMembers = Nothing}), bnuCurrency = Just DJF}
testObject_BindingNewTeamUser_user_4 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_4 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1094309'\v\991452\SI\"i\t\1775\1077291~\EOT\ETB`K\1056747\DC2a\34933fu1\FSg\1082235\1006921~)\NULdj\\L\1111356F\bz\10631;\EM\CAN\1030606q\1045593U\28237\DC4\DC3\996451\1106497\&6YCwg\159272\ETB6y\14905@\DC4'\151107\1013070uP\SOHu\1102140\22389jiv\SOP\SO\1065663\&4\EMI\17955\94575\DEL\1032488z\1082098>\GSQ\1059088\1082963G\47166\168055\STX\1046207\DLEa\r\1008684\1017084f\157444\&1'\177202\169174.\ENQ_o\NUL\1108447m'2\1064977Q\70694\44726\1075749n{\DC1Z\68824\r\NULf\1078284K\RS\1099965\182550)\NAK\b\1039441~\991335\70152\ENQW\DC1\SOH\1031725\f\DC1H\178908\996462O~\ETB\29769g\181442a\182073{$%)(q\f\ACKC8p\GS\31598\\2\NAKW0-Q\131510\28007\4255dl^\NUL\SOHY\NUL[8\8850\7444\t]9\f\988797}\60447\SOHEK\994775\USDRp")), _newTeamIcon = (unsafeRange ("\119259\189457\119892'2\1082825g\US}H\ETB\178819_C\99869M\45046\EOTs6\DLEm\43090:bZ\136091\1069542\v\1017054\&9cP\SYN\SO\995499X\145822\v^9\FSaOR90\EOT\EM.\1060082\158152\171029\CAN\1000765l_\46457\1093955e\1083078\DC2\STXDk\SI\FSo\67301\&7\STX\SUBoNg\72771Vj\STXeQ\1112037o;8%\1006514*\SI<\b3M8\78813H\STXUcSTgS@q\98375Z`>\f\143105\1102811'a)\DC1c\1055945\NUL\ACK\1039289\1104409\987604P\92284\SO]]9X\STXYx\1111188uO\SIO]$\b\128024\136766|[\73088X\a@\fx\143935A5\1019153\1011124}\1113824\DEL-\CANn\vkn\ACKn\DC2.' \161627\DLE:\DLEj\26265\&2\FSp\31459\f\1005928\1063846\a:\SOHd6\20689d\b\SIbd\t\RS[w\869\29691\DC1[\";\20872l\183588,\1019108W\46118V\fS\134630\19938\993107\1020464T\1033147D\EOT\DC1\US\1088251U\27972\1002065\188301PK\148425")), _newTeamIconKey = Just (unsafeRange ("b\t\1068070\63172.t\1111292\ENQ<\CAN]\1025356W-\1088462w{E[\SOHM}4U[\121058i\f\CAN YD\1050170\RSvnn.\1069669\FS\SI=\1112516j\DEL\SYNj\SOH\181930.\37882\SYN\1020397!S\ESC\DC4:\185801%\1099587\183187\ACK\148738\DLE6.<8L]\SYNk\ENQH\NULu]aJtC\166080\&1\69242\189357\US%\179226\1112651\1006001\b53 \1077441\SUB\13631\CANA\ACK\ACKK!I4\1018250b\1095054X>\24569B*xz\131208D\ETX\1010950xc!\163500/X\RS\1056388\1089141\1060709\188438\FS\1031638#\140255\15697\17332@\1105723\&00J#9\f4\NUL\993887\r +\NAK\185995o\EM>\99566\ACKO\1107005Z\GSY\1015227j\RSG\187952\ACK..5\DC1\29611\RSI\64677$\r3+\"9I*\156252\CANW")), _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_5 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_5 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("{\14285Y#!w~Y\1036278F\DC4Q\150304\\\"\DC47\33283\&01N\ENQ\33432\1037504\1027200\&5\DC3@\DEL\1007170gz\156337?Wk\1047880\38920J>d\1032830oB:\165660J0{_\1075961\FS\fc<\1034671\150803\ENQ*U]\155209\6134U\58998L\145468\DC3p\1090269pb\181056JPk0\99450sJr\134273a\rwd\32179\&8sm#X")), _newTeamIcon = (unsafeRange ("*A\SOHkv\27828/^'0zkh?8\ESC\SUBY\US\DC2kK\SOHT\74412\988545\v5\SOH5(\DC2\v@\FS\9658\n\SYN\27906/i\SUB\172384\RS9CEQ\SOH\FSA<GxDrV}6\GSN\DELetw7GE\ETB\191404\SUBT\EOT\STXlfQ\NUL\1056849\&0\SO\EMI_jH\161304.>\1104992zl\156211N!y APszc\ENQqW\1000335(C\185461.\DEL@B\NUL\NAK\NUL\1007225\57772\983702\25830\SOH\nO\DLE[]Ev\NUL`0aHU\29198\SO\1007255\132994]\a[7\DC4\SI9h\STXP\996133\RS\ESC@\1778{|\94073@U\f_f\1054305@^\CANj\ETX\1082238\1066240\15454Mx\US'Y%e{q\10404\1041444\1005251*\51610\NULAJ\DC3Mb\993963\175448u\"$s*\ETB\45557\1109533\1045588@\rmi\97032\DC4\17008\1022285\132251XK\RSrQP\441\NAK\ETX\GS\1102333ORZ`D\177757~\1036449\1025071\SI$")), _newTeamIconKey = Just (unsafeRange ("\168569\&2\136258\53058\bg\1027441-Bb\8796\&5\176798OTE\1087291\ENQ\74194c-!\RSl\SOt\1027845nk\36849A'\36931\f`\1079400\v\r,B.V\1104884\&0\1092378\173038\SI\1017158{I\15489qZ\1099239\STX\ACK\30302\1009316\SOo\1096766\\p+O\ACKs&\ACKgZ\194643\&9\rz\DEL(Dr\RSA\62034l\SOH83NA\ETX.\993014l~$t\ETB\1109890\ETB\v\SO[\t\EM\\n/X(\190007&2S\994100\GS\DC2+R>$&0sJSm0\SI\11231\39447ER\49679~''\DLEm>")), _newTeamMembers = Nothing}), bnuCurrency = Just ILS}
