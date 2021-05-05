{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewUser_user where

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
testObject_NewUser_1 :: NewUser
testObject_NewUser_1 = NewUser {newUserDisplayName = Name {fromName = "\ESC\1040236X=xiI\1096917\&9\181580#{_\rG\ACKl\169449\DC3}\1083306iw\1082183\EOT$8\STX]\\e%\34638#F\1058690=-N@<z\ETX[fTU\SUB\12531K l'8f\1015652!\6388\110678\137910E#\SOHA\SOz"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Nothing, newUserPict = Just (Pict {fromPict = []}), newUserAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], newUserAccentId = Just (ColourId {fromColourId = 302}), newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("sxoqYJZ3wF1I6j-QlcN4DummI9Db")))}), newUserPhoneCode = Nothing, newUserOrigin = Just (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("iqK4D15NyWMK25uZzw==")))})), newUserLabel = Just (CookieLabel {cookieLabelText = "&\DEL2\SYN"}), newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AK, lCountry = Nothing}), newUserPassword = Just (PlainTextPassword "d\1098333\1074651\1048997\6082\1077880\STX\168867\f"), newUserExpiresIn = Just (unsafeRange (407998)), newUserManagedBy = Just ManagedByWire}
testObject_NewUser_2 :: NewUser
testObject_NewUser_2 = NewUser {newUserDisplayName = Name {fromName = ";V"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (FullIdentity (Email {emailLocal = "0\161051k!Y\DC2}\1111862;", emailDomain = "\SOf[!\1095093\ENQ8\ACKV\1003749\1088494\1084272\1030311"}) (Phone {fromPhone = "+89436926994780"})), newUserPict = Nothing, newUserAssets = [(ImageAsset "9\STX9\1067385a\1066837R\RSI\t\94505(\65408" (Just AssetPreview)),(ImageAsset "\USp<\67709f\1106129\1068618t\DEL\1073914U\CAN\v" (Just AssetComplete)),(ImageAsset "n|m\1014317\&6;\DC2\1005405\&7" (Just AssetComplete)),(ImageAsset "\1051720\ENQ\ENQ\v" (Just AssetPreview)),(ImageAsset "\131666n\1052836\1002828\ETX\DLE" (Just AssetComplete)),(ImageAsset "&\SOs\ESC.\RS" (Just AssetComplete)),(ImageAsset "Q\1055327|I\1027129%Ipt<\SUBch" (Just AssetPreview)),(ImageAsset "Nd\14788^VR" (Nothing)),(ImageAsset "\DC1\1072452e`\EM*\DLE\f\43491C\100764\1071841\GS\98768S" (Just AssetPreview)),(ImageAsset ":h\SYN\SOka" (Just AssetComplete)),(ImageAsset "\b\DC2" (Just AssetComplete))], newUserAccentId = Nothing, newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("ysl9yUy8yww=")))}), newUserPhoneCode = Nothing, newUserOrigin = Just (NewUserOriginTeamUser (NewTeamCreator (BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("y\ENQ\182237\1048453A\vF\SUB\DC1")), _newTeamIcon = (unsafeRange ("\134861\&1\CAN|\DELn\EMq,\bT=l8\r\bN!#-wX8\55262\&0:?\r\1013437t\ESC\182420\&3")), _newTeamIconKey = Just (unsafeRange ("\ETB\ENQ-%\b\48788\&2\fk\CAN\180209GG\SO\1088497w:\20981\SIY)T\14250\1047800\NAK\168665VK({G%L@C*$Y\1060771IH\1047054.K\97158\1101594Q\1005519\b\NAK\ESCY\SYNr2\DC26Yu\1080417$G2\"j\60614L@\179611.p_\1052408q*Y2o\15685miae\b1\EOT\1010120\133204V\f^{\1077494.4Br\1045059\tnm\1052364\166668b\DC2\t\ESC\CAN\153176G\1104834\1034401>\1039897\99802K\SO\STX\21789x\v\ETX?e;\59941\988684\&7\GS\SOH\SOHG\"X\191241<m4\1048301\1028152\ETX\DC4\1000354lt\\\30593h\US{u\12313")), _newTeamMembers = Nothing}), bnuCurrency = Just KYD}))), newUserLabel = Just (CookieLabel {cookieLabelText = "\5418\ESCEp\67398\DC3x\FS;\1054733KH"}), newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SN, lCountry = Just (Country {fromCountry = KE})}), newUserPassword = Just (PlainTextPassword "e\US\1074258\1084924S]\135670\132498,Cz\DC2X9\1070392\987800}&y\164837s\1032094u1&3\b[X\ETXgF\50589\135367N2\EMoFNP\170098\ETB\136896\163674\29333\1061005\38993\DC2<?(+c\149904|z\995423v9\SI\1026387\n\\v\EM\1062389\1000422\13003\SUBz\1015404Ae\t\1037265>M/\ETB\DC4\1009237\26997v\172147.;f I\DLEc3J\DEL\GS\DLE\SO\&H\1040853R/\SUB\1011560~_Y';\RSX\61279'\DLE\1036240%\1061494\67081:\1110197|g\179423\1008778BPl\NAK\1031358S\1053510n};=>0\USZ!\1045042]f=\1031212N\4741\1107939\v\t\tB3t}e\1020329C\SI=\1063367\1013255\&4JG\1056531B/A&\EOTu,X\3485)7X\aO\GS\72286\EM0\b\ETB\35804\a{\EOT\149887]\141372k\18912x\190526Oq\1050347\f\ACKpd\78233\133645\&2\b\26587z\NAKQ\DC3uH~S{%\33986\1043418\DC4L\61778[I~\bQ\1032884\r~v\\?\30523j1~95m\NAK\1078644\1099000\37203`9 =w\STX^/M\CANe\148184\990814\&2(\1047497q+2\ACK\1031736C\EMp\1047070\"qW\DC3\32304D7=\ESC\119621\&7\1107487\\\1049005t_W\EMc\42760\1046410\RSA\1046495\1008244e\120683z\140830~^j\f\1048170^yT\SO\190176!=VH\1021089\NAK\1016061\NAKp%\NUL\95181aK\1014936\ETX\SI\1085582a\125127\&5UZ\SO\1000662\DC2s\US*f\190357_\1061183XK\DC1\1032022H{$LeM1)zu\v4Y\1082942bSr\DLE\fj5\144814\1105759eP\989046o\CAN$L\f\STX\DC2JK\DEL1\ae4b\168629muV\CANBUlw\vq\30271\&7\ETB\RS\f\63234\&6\DC1`[K~5J\1109085\163009\NUL\142819,\SI\1068736\a%\r\1018851\SOHf\ETXl[\a0\12991"), newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByScim}
testObject_NewUser_3 :: NewUser
testObject_NewUser_3 = NewUser {newUserDisplayName = Name {fromName = "\vMrp8\NUL3c=\31146\&2Lp\1062754\ETXo\1072332 \1013270\17156?~\1054778HOR7#\160274\185300\DC1\SOL&"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (EmailIdentity (Email {emailLocal = "\1006560\ETB&B\RS\n)W\989741\EM\v", emailDomain = "|M\v\24070y\1023574h\159685\STXKx\156405\1083454\101039<\a"})), newUserPict = Nothing, newUserAssets = [(ImageAsset "j9\ENQZ\b\"\1036884>\vJ6\15654vW\NUL" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "Rp\RSO" (Just AssetComplete)),(ImageAsset "D\DC1S" (Nothing)),(ImageAsset "\STX\SO\SI\165456)a\ETXr\132682qV\vw5\1089394" (Just AssetComplete)),(ImageAsset " \21414&\1099170%\"\59731$\DLE\1005209:`,\US" (Just AssetPreview)),(ImageAsset "i" (Just AssetPreview)),(ImageAsset "\SO\ENQ" (Just AssetPreview)),(ImageAsset "Ne\STXH\1020217" (Just AssetComplete)),(ImageAsset "\164120\95578p/z\158924`\SO\53369;)" (Just AssetPreview)),(ImageAsset "\DELJp\SYN\ENQLIR\1015359\7475" (Just AssetComplete)),(ImageAsset "\172856X6\1032385q@\DC2`gi\40232\ENQ" (Just AssetComplete)),(ImageAsset "UL\1047631\984981>X j\1105177W\1041817" (Just AssetPreview)),(ImageAsset "\185105b\1004418I\62484{Wl\1098366\EOT6\1102708\1082266" (Just AssetPreview)),(ImageAsset "\35231fx\78303\1024599}.gF" (Just AssetComplete)),(ImageAsset "\989669)\1060403\DLEc\n\1087327_P" (Just AssetPreview)),(ImageAsset "tS\1050127#" (Just AssetComplete)),(ImageAsset "JL7BR\STX\168505D" (Just AssetPreview)),(ImageAsset "\b>\SOH\"\n\1022469\1006408\1061184" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset ".\SYN*" (Just AssetPreview)),(ImageAsset "XcM\1107825Q\1005647\1107042y\97748\SI\SI\CANw" (Just AssetPreview))], newUserAccentId = Just (ColourId {fromColourId = -28421}), newUserEmailCode = Nothing, newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("KLxR")))}), newUserOrigin = Just (NewUserOriginTeamUser (NewTeamMember (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("STDRTMVELZsCmPUSg6ysAuo8hwk=")))}))), newUserLabel = Just (CookieLabel {cookieLabelText = "bG\DEL]\\"}), newUserLocale = Nothing, newUserPassword = Just (PlainTextPassword "\SOH\46505S\DC2\181382\1025598E\1112200\1016787\SOHE\40138\&8\1102952vMB-F\f\74637Rq\13349\134879m_ti<\158380a\DLEp@\EM)_a/\v\47100\f\991083T\999165Z]\ETB!Us=F,\987069\1058223Q\NAKp\153481e}fAS\1013809\RS]m\bE\RS1\DC4R^}\as\ACK\v\EOTX\t\US{\NAKpn\43325t^\997946\134175\1008196BZ\NAK|\ESC\t\1013843i\b\111161\&0\ACK\33143\RS*yk\33413\\\"\1098822\&1L\SOH?m\8682p\1104475\1103386~9\SYN~:\RSq\1110105\36185Le\1023775\EOT\DLEw\171842p1u\183707\DC3P\n\DC1\SUB\DC3 ~\50306\149135\1097305\1027547\SIQ \991373&}6\FS/j+;\GSW\1106971\158341\&9\3265D\99816\&9g\40801\191285roB37wM\1064679R\EOT%f7^\1036893\83461\1083943\1048421G\68007\175148\&4\49713S\DEL\\\f5<1G\1091856\ESC\12626\1057925\\\29120_\ETXQ.\DC2\EOT\DEL\1014919<\1022100jO5r\r\173860q\1087037\SOH\149804/\DLE\45278\43506!\7657J>\61696+\1001129\DC4GS_\185107`\1076708E\DELBBp\SOH\1035641\96101\47248{\1062230\GSd\1109661\ACK\1036785\997208\n\1069550f}H\132284\1093306\CAN\NAK\179900*\1076854;\152195_7\997276\DLE\NUL+b\DC2\78271I\36228\175661\SOH\1101021s\ACK|\FSTj[0\169365PF B2Y4fr\1035966bkW\118926\1057942\ETB\7858qj5\1112282R"), newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByScim}
testObject_NewUser_4 :: NewUser
testObject_NewUser_4 = NewUser {newUserDisplayName = Name {fromName = ">\\\1512"}, newUserUUID = Nothing, newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+781281692405"})), newUserPict = Just (Pict {fromPict = []}), newUserAssets = [(ImageAsset "\1008658I\SUBC" (Just AssetPreview)),(ImageAsset "\DC4D2%|\986491" (Just AssetPreview)),(ImageAsset "?@mo\994961\1056710" (Nothing)),(ImageAsset "\r8\34759\992077Fy\tT\SO=3gu\155352" (Just AssetComplete)),(ImageAsset "\138034g\1054950|\SYN\15038h?d\FS\STX\48209+D" (Just AssetPreview)),(ImageAsset "s\r\1112140aA=S\f3\RS\1018967" (Nothing)),(ImageAsset "\1100712s\NULP\SUB\1067963%" (Nothing)),(ImageAsset "rb2\176584\1060204\71433\1032813g\ETX\1062475" (Just AssetPreview)),(ImageAsset "-\1017609\ENQp4\986913G\DLE\177640\EOT/O\SYN" (Nothing)),(ImageAsset "\n]\DLE\DC3\177918n,\73880Y\GS\DC1\n\SI" (Just AssetPreview)),(ImageAsset "\SOH\1068474`u" (Just AssetComplete)),(ImageAsset "\1103972\147818\\U\SI{\DC1" (Nothing)),(ImageAsset "r\1046646^{[%" (Just AssetComplete)),(ImageAsset "\1055886\DC4$ _XJ\184682g\51053\64021d" (Just AssetPreview)),(ImageAsset "0\ETX\DEL'\DC3\48408$;o|" (Nothing)),(ImageAsset "\157693+_L{&G[)&a" (Just AssetComplete)),(ImageAsset "\147484\1024250J2h3n\62218*\t" (Just AssetComplete)),(ImageAsset "V\95075i\990008G9\NUL\1086407\SYN\1108190\t\SOHK" (Just AssetComplete)),(ImageAsset "lV$x\FS\159813\DC3HnW\\" (Just AssetPreview)),(ImageAsset "\ENQ\185158{\1036979s\157255\18150`W\148638kt\39349\985473" (Just AssetComplete))], newUserAccentId = Just (ColourId {fromColourId = -16373}), newUserEmailCode = Nothing, newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("_eUQ")))}), newUserOrigin = Just (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("smVNo22vDvxKcQ==")))})), newUserLabel = Just (CookieLabel {cookieLabelText = "\SIOY\SOH\62187\DC32\\gX\ESC}!\SOH\bP\v \NUL\r_@p\ENQ"}), newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DZ, lCountry = Nothing}), newUserPassword = Just (PlainTextPassword "vfeX\DC1\DLElL\1015901\127542d\1017246*J\ESC]\159081\1011410\SOH%V\FS\ESC3`\EMT\EM\DC2\1006605HC\ESCHI\US\ne\1109680}\ETBrT+88D\169865M\NUL\ETX\141314\DC2\1088205\1012933\r{7\f\1021730\FS \151888%^X{@LO\SOH\EM^v\SOH{be\135396\&9\aa\1047439\&7\120328\1073779H\917948i\"]h\25699,)/t\SOHBW.\STXJ\n\63475:l\b\135876>S+\67083\CANGSG\a7\SYN\nj)\DEL-^UW\1073378\1104062\&1\74198/\1112328\18746\30344#A\1031062\991699r\NULb\DLEA]\986670\FSG}9\1028920\57486\&1\995302\144922\1111307#/g\1021455\1007777UX]5\t\161340xIgo\999676eJ\1096804\1090541b=\172890\1063433\1065902\1101949\FS\ETB[Q\nzt\EOTh\1023261\&2sZ)\917601v\1018543Ij\159951g\ETB\13170Q\1028403\"\1077006\21964U=\CAN|)=s\NAK\DC3\54713\1077715\&8ao\f\998820\97866\ETX\58932-+\1022165\1100452\n\983791\1068970W7-\136835`tZ\1102318ynqJTiw\138363\170973/\SOHg\SOH\1088818\171006l|0I2k\27455\1066807^@\n\US\DELw\68051\&9cS\159238\&8_-\DEL\SI|dsk9UJ\t-.^:\DC2P~\DC1y\a-[K\127585\DLE\48525Oe\SI\97457C[\74182}`G.;CC\ACK\vhdiXs\DC1y]0K\r\ETX`#vs<\EM`x #D0\DEL\100167b9\1028063\1045235\ENQ\26238C\171888\&9{\f\97563hly\164215dm^\ESC\DLEbl\CANV>\1041250DP\r\SI\1080500O\1079290\STX\36989q`w\"0%/qy\GSE\1010272b \145303R/DC\NAK\r\ENQ\1107112\ACK\22304\GSXvpz\145777\1085748d<\SYN|V\SOH<\DC4\v/X8s\\\164737\1065317\120274YpvP\1023648\\\1073305/\STX\73864\1018074\181533)!\CAN\ESC3\n\1110691C./\136469\&2$4Hb\144924@\95787\15486E_\1087526/\153325,\62760\141248\a\1012305AdU\1034899\ETB\147453j\tv8Wv\1113345\15482\&2R\996005\1011594\179204R.',C\f8D4\CANP\66275pJ\24079c\1020174aUr\43177Z\ESCr\\\SOH\aV-\EOT,)Q}I\r\1092972\39519`+OIl:\1100881f\16311(\ti\40489\999201\145204\1005398Lc\"9\1032018[2\31769V%_$\n7\121112|\n\23352g\GSFI:M0\ETB\1088284\n\ETX"), newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByWire}
testObject_NewUser_5 :: NewUser
testObject_NewUser_5 = NewUser {newUserDisplayName = Name {fromName = "Q\1083768\DEL\nS[\1048278Q=\ESCbn0]p\78697^\180192\ACK\163294Lj\SI['F\SOH\993311\1010307x\SUBqt\52262\173036m~s\1002961<f:hs8Ox\te\1001873\166701]D&v\149882r\STX\t\1012800\1093471\DLE'AT\1015650;\DC3"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (FullIdentity (Email {emailLocal = "f69\1105758\993357\ENQG\GS", emailDomain = "qo\1067433\\`I\13859s"}) (Phone {fromPhone = "+10868179"})), newUserPict = Just (Pict {fromPict = []}), newUserAssets = [(ImageAsset "\DLEO\173896\1009815" (Nothing)),(ImageAsset "\176392P\37221\1001950" (Just AssetPreview)),(ImageAsset "9A=YU\2675U1O" (Just AssetComplete)),(ImageAsset "tW1\22136\&6kSi=N\a^e\\`" (Nothing)),(ImageAsset ":\bm\DLE\\`" (Just AssetPreview)),(ImageAsset "\54410\tZ_\138318fOAh" (Just AssetComplete)),(ImageAsset "b\n\1066597" (Just AssetPreview)),(ImageAsset "`k\1058686\t`z\40473;-}2`" (Nothing)),(ImageAsset "m_LL.&yy\FS P\FS\137625Jv" (Nothing)),(ImageAsset "\1009918\DC1\ENQ\165208z\996716\FSD\EOT\b\9851\&5\26815" (Just AssetPreview)),(ImageAsset "\179951g\GStz" (Just AssetComplete)),(ImageAsset "j!c\EOT\t\1044893\1000524C44\1020062\161905b" (Nothing)),(ImageAsset "\ENQ\ETB~\EM|\988854\SI0A:\1097662b" (Just AssetComplete)),(ImageAsset ".\1051655f\138925\a5\n\US\1071571\1108132*\62694\STX" (Just AssetPreview)),(ImageAsset "K\f\1077702\&9(o" (Just AssetPreview)),(ImageAsset "\1039168\1033894" (Nothing)),(ImageAsset "\EM\1070808a\42437>\v" (Just AssetComplete)),(ImageAsset "\1108649\SO\173761\1020450E$" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "w\USm" (Nothing)),(ImageAsset "\1025177+" (Nothing)),(ImageAsset "\608%\983984\16824\SYNN" (Nothing)),(ImageAsset "\RS-_s/0h\21684," (Just AssetComplete)),(ImageAsset "V\179963h\CAN\15643F\DLE" (Just AssetComplete)),(ImageAsset "H\69932)\60403< \1065640\\" (Just AssetPreview)),(ImageAsset "\995846" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], newUserAccentId = Just (ColourId {fromColourId = -5596}), newUserEmailCode = Nothing, newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("YoHlel1pAA49OpqgFNVexyBsD_L6JZNzlNk=")))}), newUserOrigin = Just (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("eag_SJQfOBeMvw0qpQWMCwzNzcm96LU=")))})), newUserLabel = Nothing, newUserLocale = Nothing, newUserPassword = Just (PlainTextPassword "\1043805\1079717t`*?kl#\990197\1109652X7\1106068y\"\EM\ESC\EOT\100460RN\ESC\EM?\161023\a\n\171460/O\\y\GS\SIKwrc<\144851\153075\989822\1068037{\SYN\1089333\50503\127983\bl\bD\b\EMe\61837\1098289\989474\&6P\23206D\DC4E\b:\SUB\DC2.^%\NULO\78572\ENQN\1079307mY\ETXL2Uf[@2\917543\bD(T\181020>}5\986327\ETX\16255!wg.\987892\51509\120931\&3_N\98121[7\STX;f\175207\1039557\ETB \1051800D;\36824R\STX\\Q\GS/\152305~\60410\t\SO\STX\167954\181078jx\21236\1088222\988520\SIaAXtV\1086628\1066280oVL\999860T\SI\35766\52135\127776s\1019844\fnt(.k]\RSZ5v\917576\DC4HFx\a~_7?\1016219\&9]\149110\128351(\92273\1035660L\NUL1TN-?y4\NUL0(73n\NAK,\DC1\1092642[\1092768\41674|Q\SUBwd\ACK\195065\180140mgjP\SOHZ\30070$r-Xy`Y\t\DC3\EOT\26071h9\97198\185783\ETX\1050561'<\1013787GE*\1104182\1076968\DC4\1047656\1055875\SYN\ENQ$\40566\1054543\CAN\r\\\143568[&(\1068789y40\SUBF\39536o\1050256\EM0\1093854\14036 %!)\1006316\98374\1049523$\12600\134068\GS\1020319\EOTOj\EM\1011377H}5!\SOHI\RSU]\66842\1004437\148157~\7453\45919&_\3373\95525_@\1053273&1DT~E\STX\"\SOk\SIN{\NUL\166205\165208[o]{_^\NUL\STX\989718\154720g\11267\144905\fXi\1018948\164956\ESCD8RlY~\DC1\28678<guj\RS\1106605\38964y&$\DC3\DC2\1072343p\\)=dwJ5f\CAN\\-$\992679\&8$\NUL;z`o"), newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByWire}
