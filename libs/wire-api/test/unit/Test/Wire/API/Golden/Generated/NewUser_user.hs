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
testObject_NewUser_user_1 :: NewUser
testObject_NewUser_user_1 = NewUser {newUserDisplayName = Name {fromName = "gTG 6a\\\1107884H{aS+\96711d\"\1006263\171819\EM\24365.\NULx\DLEe\1016583Y}p\992709\993330\1047108W\v\134466\995273w\ETXm=d~\ETB{\\\143192\1096806j6nU\55119\&9\33522|\177580q\43543{@e\1070377+=Km\SOH@;V\ENQps;j\t7\1072129\1034564\41999a+3{u|s\ACK\1102985\vo&\1026939\DC2\44880\DLE[A\1004093Y"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (FullIdentity (Email {emailLocal = "'z\8806+7\1060576\160266\1024124\ETB\SYN~~", emailDomain = "\fT\25469n{\188091?-=r\1026936"}) (Phone {fromPhone = "+480394388909"})), newUserPict = Nothing, newUserAssets = [(ImageAsset "wl\170366 \1045514]R\173966\994202\&7" (Nothing)),(ImageAsset "$\36949\1110667\1053201j\va,\r\b c5D" (Nothing)),(ImageAsset "\1040607\92609\ETXt" (Just AssetComplete)),(ImageAsset "\ESCL\52306\FS" (Just AssetPreview)),(ImageAsset "]2\129092\CAN\1077905'4\1080044\&8nNr" (Nothing)),(ImageAsset "\SOH21s\n6" (Nothing)),(ImageAsset "\FS\ETBw\SYN>}\120285\t^3" (Just AssetPreview)),(ImageAsset "5\EOTX\131870m/\1023093T\ACKolx=\1095511D" (Just AssetPreview)),(ImageAsset "xJ\DC2,\1089144\EOT}" (Just AssetComplete)),(ImageAsset "Ex\1077324\SOH\ENQ*" (Nothing)),(ImageAsset "9\78442/\EM\159735G,\\\b`i\DC3" (Just AssetPreview)),(ImageAsset "QS\1059479\&65\1030034\46087|1Q,Z" (Nothing)),(ImageAsset "\SOH\1081062z\n#_X\9113)u\49799" (Nothing)),(ImageAsset " \STX0,=\1095512T\r" (Just AssetPreview)),(ImageAsset "3]f\1019883Om40\EOT\1084704\59073\b\DC34" (Just AssetPreview)),(ImageAsset "b]\DC3\1010601\1079698\&4\SO*6" (Just AssetComplete)),(ImageAsset "\SUB\1031656w+\54984\28286" (Nothing))], newUserAccentId = Just (ColourId {fromColourId = -27721}), newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("xBcJj0yz51QXvf2s9VyzDOFTVb_nESp_")))}), newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("VyWPDsp7sUQxH5f5DRWb4w==")))}), newUserOrigin = Just (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("UK5KfqLz49Y2e8mvsiraLrQ6Ld8oStRc6IOe")))})), newUserLabel = Just (CookieLabel {cookieLabelText = "$\78198\1102341%u\"\aP[MU\38170qLUG\12969u\ETXf\1097805\NUL%\1049860o]\32404"}), newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Just (Country {fromCountry = NL})}), newUserPassword = Just (PlainTextPassword "g=Du\1082696o\60225aP\1048197\nOi\169419*7\1090424\&4j\1062284@\1004115\DC1'\135869\110610e\129526\bEQ\v\nSA\992164\97760\986198#\NUL\"QX\ETX\174001\38196u\RS\1047534\DC2;A\22973\ETX\DEL0}\v97\STX\986731\1106230\ETB$-\1041202\DC1\SUB\DEL\26411\4178\1085318\858+\1056527)\1081379u\SOH\n\98021\174050\988149\&2l+\ESC\SOs\1036900\EM\ACK\162430@\n\f,\95478\1056370\FS\b\STX\USxhF!\140330\&0\1074049\CAN}8)\DEL\61634O\63941w&\25403O\1074780\ETX\ETX;r\30427\tPGp\1096244\14224$\EMzq#W\1048489\35374\n \1022157\f,{i\985208\1086612>\ETX\23693\163867}7tA\1043383\b\182444\998640X{\DEL/\1074070*\1103644\50749\&8\EM@\fx'\US\1044029\153848\47214\138608i,\168446g\STX\1013482\162310M\\=\SUB\25671n=\ACK`OS\1000379o\1057734\r\\\\XF,\1089365a\8164$\999733\DLE\NUL\1076548\ENQc0@\nM]\992884\1087939FEN\1062997f\1031459etT\153719)\ESC#NV\1044066\21298sc\1018795\&6J`\ENQ\1052902Tp\ACK\995810\STX\1019130ijK\1004492\US\49602xX\156840\26754E4MS;Lx\1008952\DC1\SYNjSa4R5\34853\DC2*\FSc\SI\EMxn9\b\1011241\&4\\aL\DC2>\DLE\59218\1084662\ETX\GS(\a`\ESC\ETB-,4rp\133721^\EOTX\r\EOT\1005378<uI\ESC7~n?\ETB\97738\EM9\bRM\58191%s\33946\"%\f\164569hJ\DC3=\1069335#A\\\n\v\va~/?z\b\1087592\DC4\EOT\SO8\r\1008393\153371\DELr\992451ft\FS%m\CAN`hCvb\b\DC48\SI\1005260o\1002128g\1010125\SYNQmWn\NAK\1101434W\as\999816\"\100837'\165323\120228h[]z\64103T>a\1092810\&9\51082\t\NUL\t\68160\101043\1094966\RS]&f(\NUL\1099811\&9S\t\1003606\72141b1B\NUL\1071643\&39\63781~`\1013073I5Y\DC1\65113V\a\RS\176450\&5G\142665\1101564 \ACK~\1042443&?\66699D\n)\\A<\1044521h2T__q|ua?\SUB?CgJ\CAN`\b\162412Q\1102988\DC2\4166\EM@=M\ACK3\1097323{I\ACK.\17573\1058288\bf\188924S\a\40766,@\CAN\EOT\RS'\73869\n\NAK\13225\DLE\188872\1033330G\1076926\1050313{^*\ACK\b\34220x\ACK\STX\1039202\29253H=\1095769qZ\1023450&)\a\1093838)[\EM1\US\"Z\20020N\ACKP\1093982A\194903S\1030605\42259\179953\186112-\1086430RW\159618T#\ETX\1087272gycaO@\EOT\128055\DLE\SYNiGb\51091\&0\131177\82983tT)G!g\1099816\&1"), newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByScim}
testObject_NewUser_user_2 :: NewUser
testObject_NewUser_user_2 = NewUser {newUserDisplayName = Name {fromName = "_\NUL\139542VO\"4\989740t\a3<e|Nq\GSr\1002466u!Xy\43542V_#`\SI\b&\SUB\f\989699\ETBm_TH\38004-R\1040732(Q%7n\1060005W\v\DC47]z9"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+464709744215"})), newUserPict = Nothing, newUserAssets = [(ImageAsset "\152238h" (Nothing)),(ImageAsset "\SOqp\1022754\SUB" (Nothing)),(ImageAsset ",\\R\1060306\nq\nY\DLEy\163078\1066756\NAKX" (Just AssetComplete)),(ImageAsset "\US<\SI\131104\1090125\&0\f~\DC1)\1002651(" (Just AssetComplete)),(ImageAsset "\1086710\n\DC20d\ETX\FST'o+" (Nothing)),(ImageAsset "^Q\1073894\1026363\1053278]\1029735" (Nothing)),(ImageAsset "6\160365%\1064655\ETXQ~\t<" (Just AssetComplete)),(ImageAsset "(\21604\GS\157517\US\ENQjU\CAN=\1048679\SO" (Just AssetPreview)),(ImageAsset ":\ESC\172147\161082t" (Just AssetPreview)),(ImageAsset "\2735\138624G\br\995494\ETX\1051458\&4\156343f\r" (Just AssetComplete)),(ImageAsset "]\1062098\135640" (Nothing)),(ImageAsset "\3959\ENQa\1096333~F\160322" (Just AssetComplete)),(ImageAsset "\100249\t\167149_r\v5\ETX\998856\1110983" (Just AssetPreview)),(ImageAsset "8d\995018t\160570[B]" (Nothing)),(ImageAsset "\NAKX\1011962;A\149176k" (Just AssetComplete)),(ImageAsset "ydQ\r\ETB`\EOT\1033924W\v" (Just AssetComplete)),(ImageAsset "H\n2\DC16\1037858rt\1058006\&7]d1Ul" (Just AssetComplete)),(ImageAsset "--q\72323z;dd_LYx\1107697X<" (Nothing)),(ImageAsset "\EMy/\131454\1113181L\SOH13\145305N,\167875\1064221\&2" (Just AssetPreview)),(ImageAsset "]\ENQ\tb\983765,!" (Nothing)),(ImageAsset ",S\NUL" (Just AssetComplete)),(ImageAsset "\1013872\132317\r\1082\r\159863e^I\SUBs" (Just AssetPreview)),(ImageAsset "+\\~" (Just AssetComplete)),(ImageAsset "\161567\1006324\1074993VGv\vOM\19915\a\b\24943\129060" (Nothing)),(ImageAsset "z\1057369:\EM\1045399p \188685\1105842l\1113630\&0" (Just AssetComplete)),(ImageAsset "Dn\1099576rV)\1024654\1024980\STXzI\USN" (Nothing)),(ImageAsset "To\1062550\EM\22644\SOH/@1N2R" (Just AssetComplete)),(ImageAsset "\ESCa" (Just AssetPreview)),(ImageAsset "u\FS\142688#KR\49345\CANVV\DC3@\177894#" (Just AssetComplete)),(ImageAsset "\r\1036673" (Just AssetPreview))], newUserAccentId = Nothing, newUserEmailCode = Nothing, newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("atBWMGAL5SbjPlKVRLlFVRpd9w==")))}), newUserOrigin = Just (NewUserOriginTeamUser (NewTeamMember (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("KGU=")))}))), newUserLabel = Just (CookieLabel {cookieLabelText = "#&\36544_w\ENQ\2024jw\EM\EM\RS?f\SOH\144502ST5vTbp\1003129%\1105922"}), newUserLocale = Nothing, newUserPassword = Just (PlainTextPassword "\139330\v\EOTVKb\1057586-,\44968\49387y\1091592%a\NULz\1102858l\DC3\1112422&.\1048466\146566bd\"aOhO#\1097898],y\138236\77950\1087339\1026034<\aPYt\1050061w6^_\1101893l\13888=\1039432]a\t\137415\170563;O\SIohQ$xOZI\SUB\ACK\CAN\97179qQ'\1102040C\\,\126617\FS\93972AE\DC2v\118791N(|\35994Pnr\39329l-\179094T\42928\45647E]\162906\1082110\&1\1109810^\1012528}\ESC\DC4\SI\986300\45541\bIcN\SOH\a\r2IM\vO;\RSv\1016149_\ETX\SI\ACK-\136820\1031980m\175224\NUL\NAK\ETB6_oX%2\STX\DC1?9z|\\\f\36070B`*^\1015558\&2[\FS\164451E.0F"), newUserExpiresIn = Nothing, newUserManagedBy = Nothing}
testObject_NewUser_user_3 :: NewUser
testObject_NewUser_user_3 = NewUser {newUserDisplayName = Name {fromName = "\983406V\n_M\65296uQ=w2xcm:?\140190\aVOUG\854\1109436\&0/e{#\170688v\145694?9:\NUL_\\wk\94344;\1007465\1099319"}, newUserUUID = Nothing, newUserIdentity = Just (FullIdentity (Email {emailLocal = ")/$nZ\1086420\1093689(q\DC2", emailDomain = "\1096867T\DC1\1048636\985432"}) (Phone {fromPhone = "+305458541"})), newUserPict = Just (Pict {fromPict = []}), newUserAssets = [(ImageAsset ",\1089595\ETBA]p\\m\35080{" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "\28209\&7\DC1\78477\STX:\895\1022238i\1023924" (Just AssetPreview)),(ImageAsset "\1088467e\1020588_\1021887" (Nothing)),(ImageAsset "\25980\1113958\36032" (Just AssetComplete)),(ImageAsset "|N7\1043081\SIS\19513" (Just AssetComplete)),(ImageAsset "\1076666" (Just AssetPreview))], newUserAccentId = Just (ColourId {fromColourId = 8245}), newUserEmailCode = Nothing, newUserPhoneCode = Nothing, newUserOrigin = Just (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("NM8CBA==")))})), newUserLabel = Nothing, newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TY, lCountry = Just (Country {fromCountry = CI})}), newUserPassword = Just (PlainTextPassword "\1102767\146765C\1002217hc\34514\SO\ESC\986268I$z\SUBr%OI=B^\1081217\DC4A\58869e\1073165\&2~|\171403\r\ETB\1071453l\23284\1004208Ro,\1015098pT\1100952\SIo\1028813v\1042382#\DC1DZHa3\167380G)E\US\132413\180989f\RS(\35732\&5\ACK#\DC3}s+\160160!\RSli\SUB\182453\1057234\997272#c\140151M\v6~(-i~\DC2G\174382G*K\NAKa\ENQ\US\1029542\r.iX\163508\990580<\SO\\\DLEY(\f\189609\1081908\135530\&7z\28945\95784\&7\24384\42605p{x'\SOH\a\162646|\ETXc@:`\21824i#\12162\1047635o\177221Ji>\1011806V1\1035701\120900\ACKf^\DC4J\19953\&4\1020463\RS\98334\DC4\166254\&8%r\ENQ\1073963\CANM\GSi\ETX\47441\1058639>Or\EOT+;\8182hZpl\1104373+\DLE\DLE\nH\8236|\998682ZWQ\1006907+\1067066\RS\1059321\137595\22343S{e\1043943\134575\&5\999789\100737,:K\bM\131470\ACKr\t\174240EX^6\1062816\t|\129049,#\10601\USX\46722\1110896N\DLE\1033317\983564'\1049487RHB\SYNTx\21833`U\ETBki\SYNc\ENQ(\1101372>\1043151\DC4\38942\"\ACK\NULiL;\1100142V\t\SOQo\1017523\36486\ENQk\ENQ<\SOY\183394\998968\b\DC2\16841}d;Eu>\133771xD\STX\100071\"SR=I\1086709\NUL@G\141343\&5xqc[\"\ENQ\172339 c\165979s})>\NUL\997117v\1098313\&6~P\131272\&4kHU2{\SUB\1059712\STX'tW\53308d6\STXT8\1036090\1076724\128330\rc\18478j(_\41654\RSPI}*\70313\1099923TBxQ\989776\SOH\1059566*\t'\28224\&0\1005224\DLE<\nk\b>@\156877<a_L\47315\162917\EOT$\1028986\&2\USp\FSl=t\STXT\DEL\1071136\&7F\1083897n\28946q\161349Si\31108i\10919xzS$\1030839\987577T&\61925z\1048265\11357\FS\1073857&-\991263a0\1013330\CANQqe\1037992\&9\DLE{\1052660H\1004050\SO\1106138\\\DLE.\ETXA\NAKt\a\ACKe\1075803{=\142492W\EMB\t \1055258yNWU|1\FS\163128v\1051146$,%\EMfZH\GSe.>\SO\1027572\SOHo\RSJ\DLES\5198i\185464\8740/\96225\DEL\DC4 \45541\&3{9\DC2'\9570\SUB\118851SU\CAN\NUL\SUB\1059161\DC2P\nX-1\1077876*\95060E'w)\DELZz!O&3<kgi/p\DC2\1011919\1060141G\74771\983123\53244\1111882\&8\NULZ38tC\21221\1098599(\ETX\DC48h\DC1l\ENQ\FS\1087122\NAK,\1014494T[2`\178512H\ENQ/\1065209\ACKC\44867\1048867q\FS\50302\a\ESC\b\23131O5\\+\1100240\&2jCsYIp08q\ETX:F'\v~{\97727?Ec\990255*6A\1089526g4M\1027525$2uUL\1050036~\1039496\1002970`\1024385\NUL\1082843:\b\"\STXb\t\38933\175059\12521\1045773r\984172;\DC4\SOH\DC3R\GS\2355dKNY\97388\1052100\"\1056196\52460_\38769$\41382(\GS\DC3\SO\DC28|\97882\&7\1032388M~o|\NUL<"), newUserExpiresIn = Nothing, newUserManagedBy = Nothing}
testObject_NewUser_user_4 :: NewUser
testObject_NewUser_user_4 = NewUser {newUserDisplayName = Name {fromName = "\179325\1008583\1046117Z\FSbV\DLEU!tEC\\\1104823\EOT\143598UUo#\152121\29206\ETBx\1063762\1000821\&9v\1049300"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+66166079"})), newUserPict = Nothing, newUserAssets = [(ImageAsset "M(\t\DC4" (Just AssetComplete)),(ImageAsset "\22999\bX\f\1048193" (Nothing)),(ImageAsset "%\EM\SUBR\ETX\STX\ETXa" (Just AssetComplete)),(ImageAsset "\1048757v\SOH\184510MNA=" (Just AssetComplete)),(ImageAsset "t\NAKz" (Just AssetPreview)),(ImageAsset "\ETB\95612\1045038\EOT=\1075318_\ETX6" (Just AssetComplete)),(ImageAsset "\94833LF\a" (Just AssetPreview)),(ImageAsset "s qm\141858|z{iM?" (Just AssetComplete)),(ImageAsset "$r\5327a" (Just AssetComplete)),(ImageAsset "\1087895\994383" (Just AssetPreview)),(ImageAsset "\1082762Z\1031477>\1008977\73854\998313\US\187946\1004854mQF\1072405" (Nothing)),(ImageAsset "^\165980I9R\DC3ev\190614" (Just AssetComplete)),(ImageAsset "l" (Just AssetComplete)),(ImageAsset "\1076916s#" (Just AssetComplete)),(ImageAsset "\83086pE\STX\ETB\DC4m\ACK\1085169\154217#\1009992\7011" (Just AssetPreview)),(ImageAsset "#" (Nothing)),(ImageAsset "V\99727BW-3l{\ETXxk*7\1048830" (Nothing)),(ImageAsset "7\991124J~a-If\1071919S\NAK" (Nothing)),(ImageAsset "\DEL\ESC\SO0Fb-3\999548\ACK\r\35172\1035750\1065899L" (Just AssetComplete)),(ImageAsset "c\ENQ\57947\DC3/5\t\136203\v\1015085\DC2:\1011443p" (Just AssetComplete)),(ImageAsset "[Gaa]p\EM-z\RS" (Just AssetComplete)),(ImageAsset "\DC2\EM\1086207Rn" (Just AssetComplete)),(ImageAsset "P\DLE\1107217{7_\1058264\GS7$aK\1098615\&5" (Just AssetComplete)),(ImageAsset "\EOT\DC2]\t\187440oU\t\1069727) ^\t\DC4" (Just AssetComplete)),(ImageAsset "a\156720Bw84\988905*\1015203o\133495" (Just AssetComplete)),(ImageAsset "\1075324\EOT" (Just AssetComplete)),(ImageAsset "`" (Just AssetPreview)),(ImageAsset "\DC4}3/" (Just AssetComplete)),(ImageAsset "\STX*#\NUL" (Just AssetPreview))], newUserAccentId = Just (ColourId {fromColourId = 10489}), newUserEmailCode = Nothing, newUserPhoneCode = Nothing, newUserOrigin = Just (NewUserOriginTeamUser (NewTeamCreator (BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("zpjg\1054322H\ETX\"\175111\SOH\NAK>\CAN-e\1094522_\DC46u<o\SYN\DEL6_Y\1012092\31210AA\DC2\1016097\DC4\1012020;X`{")), _newTeamIcon = (unsafeRange ("A{\NULyeh\CAN\46428B\12469x_WA\65706\STXz\r\DC26h\1105775\NAKj}\1065780B\1021688\&1\f]i\DEL)\SUB!\STX\1030100\1083096TH\159375\&4\159132Q\1065417~\184548\140398\167405{)I\7285\STXtwF\"|jf\ACKom\tX\NUL#{R\44837N\120339HRJd\190466(\10850\993478.H\GS\CANa\NULlM\US\r\1018490\ETX\ETB\164743\12798|\ENQ\96987m\1097300\1004571%$\172456K\998616\US!c\12362\16638\&6d\SOg\95604\137467+\72148\ETB3$_\1009743\173016\36069&}v\33332\1029631/\1072512\42640\139739qq\1098624\f~#\n\1090434`\11576\1084853GM\t\1081267E\23140\EMC\DEL;/\59425\"\62478b$,\FS\1068966_\134814V\34861\STX\n.\186612\NULI1`L\ENQ\156749+\STX7\v\ETX0qftD?^df\ETBz7[\1087561\1064278\1015375;\96694\RSK\1049781\FS 1\tn\SUB\54759\1053808O|\DC11")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just SVC}))), newUserLabel = Just (CookieLabel {cookieLabelText = "{\CAN\GS\GS\178559\v\1040529].&\1091257jf9D\NAKa %\n>QX\r"}), newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LT, lCountry = Just (Country {fromCountry = CR})}), newUserPassword = Just (PlainTextPassword "1\1088591\fG\188040\144764\177353HO\1081835g\1024139V%x\1057095\19355T\ACK\1028941?Ql\1042697\&7\177335f|kCG\1101679<j-}bs#pc\8857\187515\74469 \154102W\US\SO\"y\1079091\133393\1045740v#|O]Y\t\bsWy=1\984216\n]S\37690\171457\1090514\DELzXz^"), newUserExpiresIn = Nothing, newUserManagedBy = Nothing}
testObject_NewUser_user_5 :: NewUser
testObject_NewUser_user_5 = NewUser {newUserDisplayName = Name {fromName = "\DLE|\1018083\ACKE0\1024689\SUBo\189447G_w\1039797\57772~\SOg1\aM\b\67296uq\5666\178953\b%\SYN~O\"yWbeJ\1048356\FSya\47746\ETB\STX\r6\SOE=\\\DC4\64559\DC2:*\v/\1020201\DC2Z\USa+?\EM\DLE\1094327\tk\171181$)\1005448\DC1\"\ACKD\54877?\\m\FS@\15922$r+P&H\1039928\SIc\57671\992197\t 5\r\1099476\163655\DLEJ\169896\&34A!\52876!\1010711y"}, newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), newUserIdentity = Just (EmailIdentity (Email {emailLocal = "\140362Y\RSh\184793\FSj\DC3\CAN41\CAN;", emailDomain = "r&v=vv\59955Y"})), newUserPict = Nothing, newUserAssets = [], newUserAccentId = Nothing, newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("qnfONo5oKA17JYe1I-L88aKx")))}), newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("8w1o_eJTwwbgaiIjiQ9biR1H")))}), newUserOrigin = Nothing, newUserLabel = Nothing, newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = KN})}), newUserPassword = Nothing, newUserExpiresIn = Nothing, newUserManagedBy = Just ManagedByScim}
