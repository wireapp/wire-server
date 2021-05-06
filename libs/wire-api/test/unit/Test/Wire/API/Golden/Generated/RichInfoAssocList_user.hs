{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfoAssocList_user where

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
testObject_RichInfoAssocList_user_1 :: RichInfoAssocList
testObject_RichInfoAssocList_user_1 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\1085894", richFieldValue = "Q\185720\DLE\1039017\DLE\13154Y\1054933[@bSwpNy\f}P3;\1027602\EOT4*"},RichField {richFieldType = "p%\1010193kQ\EOT'\30007", richFieldValue = "HM8K\RS$J\1019168f\1052484D$_r!\20785\&8\f\NUL\1095043\1029017DAw$o\45311"},RichField {richFieldType = "vw\136884\FS\65370J\f", richFieldValue = "\ENQ\v\1021846\\\DC4&kg\EM"},RichField {richFieldType = "A\1079592zGcOlFi\SOf\52347\1046069\18829w\1035838", richFieldValue = "P\1027706(#\1068600\14969"},RichField {richFieldType = "\186338\&6F!\78046\ACK", richFieldValue = "\1007305\1113194\1105944\7927"},RichField {richFieldType = "\1081944\72737\988570\1019505r.\ESCTW\r*lL", richFieldValue = "a/\t\63660"},RichField {richFieldType = "!]hb\t\1062207p\165141B\1054665$\1069495\63724\123209\ACK\ETB\1083962aw\996158\FS\1102869", richFieldValue = "YB\1058348a,\1083523h\STX\toG"},RichField {richFieldType = "\US\174478F", richFieldValue = ".z1kP\v7\38989\USu:\ESC\1014288\&3\DC2"},RichField {richFieldType = "\DC1\165854", richFieldValue = "\1044030\46936\EM,J8f\DC2"},RichField {richFieldType = "\vB0-\1029585Y5Eto", richFieldValue = "w_IZ"},RichField {richFieldType = "h\120965.b&\SYNH3\ahj\1113680\92209\1046783W", richFieldValue = "~a\DC3\1056037N\8065\1071822]\GSE$\1011189\188986X\174503\161359eO\1019179{j\1036794"},RichField {richFieldType = "", richFieldValue = "m\"\25208'1\ACK\1056165\28586\61981\vg\1031997\5929"},RichField {richFieldType = "hR\a\30733\&9\ETX\97877\SOHs\FS\1047784OZq3Y\984025>", richFieldValue = "\983275'\78030\CAN\RS<\1038227I\\\NUL6H\\\128025xf[MJ\DC2\1074657\ESCx\ENQ\1024012\40063"},RichField {richFieldType = "8&\ACK\1023364-\EM\138058i\1109403nJu?\22011\USNh\1023656\156081\&8I\NAK\DLE\tV", richFieldValue = "x"},RichField {richFieldType = "w\1077856x\aA\55154?UrU\95791\EMF\1078867/(VBg\152766T\n\f\ENQ\ESC\ESC", richFieldValue = "}]g\n`\EM\SO\1037403~`\SUB\1108304\&1"},RichField {richFieldType = "b\1057937\1036510V*(!i\DC3", richFieldValue = "\20801\USx\62859Th\SYNSQ=U\1009483fy\27789h\1084583\GS\64849W\151650\&5l@\992872\&4-?"}]}
testObject_RichInfoAssocList_user_2 :: RichInfoAssocList
testObject_RichInfoAssocList_user_2 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\1004044+v\bL\1060849\1093638\1052433\167578q\1038172\NAK\47206V\FS\132504\t\1081260,Ky\1090474#\n.\39871", richFieldValue = "-\1093935Zo\1037311}\46755\EOT0}fY\1067033\DC2C\993244Y\169779EO\EM$`j "},RichField {richFieldType = "\1099615<\SOHW\998293 \996377^67=Ro!l\35349l\ENQ{TG\nS", richFieldValue = "\nk\ETB@C\36855\20062\&0\STX~\62394J\SYNA\SO\v\DC3\165132\CAN"},RichField {richFieldType = ">:^m\999794\16634\n7S6<\ESC \165731+p\"Zg\EM\990194\DC2\997695]Hp\1074964R7", richFieldValue = "*7\154984/O\1066785\v"},RichField {richFieldType = "\GS\ESC\161540\1019959D\bAg\25763\CANka\\#", richFieldValue = "a(\r\1030750>y\1038256 \994950\\Ox\DC2RBQ\DC2.\175182C|\DC3\35011k\b\DC1<x"},RichField {richFieldType = "\\E\EOT:\1083045\SO\145294vg\1889&", richFieldValue = "c\DC4\94775\&1M\1011903D/7"},RichField {richFieldType = "\1046813Z~\\\SYN\b\a\1035659\131716\19483\&4", richFieldValue = ",e\1081388\151982Y&tOAh2G\37738Itf~\DC1"},RichField {richFieldType = "{'V~d+[s\NAK\1038\145964`8\b", richFieldValue = "z#\RSK\STX\SYNC\SOH"},RichField {richFieldType = "\150556\EM_\156730\162134 {\146327&\992705#Y\SYNQj\ETB\SIa\78819Gq\SI\183299", richFieldValue = "*\1034559XS\SUBy|\94438\145514;\1016069\26547R!`hg\180668[^#\24131,"},RichField {richFieldType = "&U", richFieldValue = "n\917948\136116P\1080615\&2\EOT\187148\137181g\ETXvF\US\999980#\1023098\b\DC1\1002842\1102668\&1\ETBX\1102805"},RichField {richFieldType = "\94075='8\133580\ESC", richFieldValue = "\r\STX\SIh7\995852\100909bj]Q@"},RichField {richFieldType = "x+{(\r=$OqVVY8\DLE\DC4`\\\1056183`xd\nk\a=", richFieldValue = "\120528\RS\1036156\1013134"},RichField {richFieldType = "\96090\37287X\1112909K\156628fxY", richFieldValue = "_\1080467:}\92770\78438\1092131 \994418\STX\25933+o1\1038182\160333\SIO\28809?\1086578\1026900\nBV{T\\v="},RichField {richFieldType = "\FS,R\FSZ\41694\aU\30912Y\1062121*b1-\182247Y~\65042\\}\1032104\26341d\1011308\STX<\t|", richFieldValue = "\83106\EOT&\r"},RichField {richFieldType = "7h\63963\fo\SO\FSP\15040\&7X9\1024168q", richFieldValue = "=Ukummc\1057241`\1103227Ct\985165\FS\1037639\DLEQ^/\\\1066707-J\55004\FS\SI\DLER>\1062988"},RichField {richFieldType = "D\1075898\DC2F\DC4\37352\1068191\&6c\51069e3 S_w\EM<D\NUL-\1045286j", richFieldValue = "\DC3\1111203+=&I"},RichField {richFieldType = "\a_\CAN!\991365\&8Su%h\997620\ETX\SYNg=z4mUA\a\119855\&3\1005072\99397\1098388Y", richFieldValue = "\1026616\NUL\ESCv]k\181436\&4\EM"},RichField {richFieldType = "\58109*b4et\DC1\ETB-\39759\NAK\GS\134632\&3Z\51931\bOz", richFieldValue = "_MQ\DEL\rG\63362\DLE:\SOHX\53071d"},RichField {richFieldType = "\147009\v6\164979\1112766sT\58063\&7\ACK\47995QtV~l`\SO(\DC27", richFieldValue = "\FSB\f>\184984\998629\&1\1014633\18756{\ETB|\r\ETXv\ETX3"},RichField {richFieldType = "RK\10685\1058256\DC1\23508\SUBM\r\995699vB\34688\176833`tg", richFieldValue = "X\999352]w.F\\qs\1107284-"},RichField {richFieldType = "l\1055591L[OU\NAK%\21540", richFieldValue = "W\1096089!\"\43675\NULN\EOT\SUBk\NUL\NUL>0\1088253<"}]}
testObject_RichInfoAssocList_user_3 :: RichInfoAssocList
testObject_RichInfoAssocList_user_3 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "[\8574d'Mt0\133042", richFieldValue = "\SOHN?\SOHxE"},RichField {richFieldType = "\1043303\992777\96943\SI,", richFieldValue = "\1049649"},RichField {richFieldType = "\158206:!\DC4N\174913\189053\&4\ESC\EMf\999730^0+px/:(Qa\154430^", richFieldValue = "<7>[\993161\FSg\1992\1077920\4969"},RichField {richFieldType = "\NAK%\ETB\1077620\DC2\ACK\177199\1102885|\1061304\ENQ|m\1028151,\1096579ns;\SYN2\rrL\DC4ky", richFieldValue = "\RSH+\NUL.=>\984123$\1081828\15515\r\\\145953"},RichField {richFieldType = "\32511\RS\a\1068015\98312\1111016\ACKOt+\STX\188086\b\999513\&2\DC3Ab\FS\1006899\DC1\\\GS\FSH\SUBCL", richFieldValue = "\990575Z\DC4K3\DLE\998056\DLE\1091802."},RichField {richFieldType = "W\ACK\1000065\&7wd\SUB``@\NULu\31275\b<\ETXrKR\44095\49500\179537\ETB", richFieldValue = "\SO\53982K\1002298\113809\v\1099778\148103]\168078^X\"|<\25377\166621\DLE\EM4}y\CAN\ENQn\1449"},RichField {richFieldType = "\DC3nJ\142742\156531\141920=\n\1090624\166403a\1007346%y{@cem\1000640l\DC1\NAKM\SI:\DC4", richFieldValue = "0\1010824\11136\\%\64621Ygm4n\aZ\7040\1009023\97077\1092638X\f\RSI\DEL\vI\1109823"},RichField {richFieldType = "\"Q\STXR)\1051990&~X2a\t]\135535\1052493\ETXp*e<.\1011449=4\1100680\1090904\&0", richFieldValue = "^u\128162^=U\aB\t[(\ACK\146911\FS\997831\FSrC\SUB|NL"},RichField {richFieldType = "vjy\rH\1060090WC\r", richFieldValue = "&\NUL"},RichField {richFieldType = "\t@\DLE*\166873\USg\n\EM\1073515\161802I\DEL\SO\DC4\"\SO\1053141", richFieldValue = "T\148229;RHi\1107589LrhO:\82952$ /\SUB\ESCf|\DC1"},RichField {richFieldType = "\998710P@WW\1096", richFieldValue = "\1008454\re2\ENQk\172624\r\ACK.y\ETX\a"},RichField {richFieldType = "w3-\DC4v?\1041665n'je\113671\73838D\1055080\SO\1003475\tGF\42381\1071343'\169946\1085398M\167504", richFieldValue = "\1042202"},RichField {richFieldType = "\DC2/M\43020J\t\DEL\f{aaj)Y\1062042\47692\1110819SA", richFieldValue = "\1111531r\STX\174116P\ACK\994890\1033817m\DC3\1073052?\ACKaE2Iar\ETBK\SOH\STXV\1090352l\DEL"},RichField {richFieldType = ",g\138266k\20309d0r", richFieldValue = "\1067222G\STX\20442\USc\9779-t(\51906nE\FSs\153595\1067777R&o5W\DC2"},RichField {richFieldType = "2V", richFieldValue = " \STXd \1032619\42255{\144139\53998}\995708\1048136p)Y\137485\NULS"},RichField {richFieldType = "\t\SI~\ETX!\n2~\SYN24/\ENQ&\STX:4;\EM\1037392\SUB\1064359Y\189169\17926 \7114\142584\SOH", richFieldValue = "\162784\RS\1060919i)\1101"},RichField {richFieldType = "\984193\SI\a\ESC\b\1027294\t\988621C\SUBx%", richFieldValue = "{H\186711G\v2\994629\&4\1100392o\32708\177036)K^a2\985776\1104460X\SUBC\166873\139539z\96973\&0\131789"},RichField {richFieldType = "O\1031189b7\STXu\GS\40376\GS\1108501", richFieldValue = "}(\1090432C=\a\1010323#\1065556\r\61812^\66840[/g"},RichField {richFieldType = "vu[\74645\1084738sJ\SYN\"\1106926)WyIMt", richFieldValue = "#\138825L:}:\SYNs\DC2br\CANQZ6\EOTl\1061991if^D\145320h~1aP"},RichField {richFieldType = "\EOT[d\STX#\GSK\DLEv\"3'", richFieldValue = "E\NULX6\NAK\1064094%\ETBu'nj5\119856\111327\1057345\&1\1000458s\DEL}xc\DC4.0\1042584\SYNJ^"},RichField {richFieldType = "\ESC6\1038423T\14139\999573\ENQl%Bv\1007784\1005422\172776\RSf0\134318f\1083566D\163286*b@0\51556\EM-", richFieldValue = "\t)$\ACK\STXDi\DC3\GS\DEL\1105988"},RichField {richFieldType = "\b", richFieldValue = "\ESC@\67174\1029772\1091751\"\NUL^BX\18302q"},RichField {richFieldType = "|\EMd\1086587QS\31902H\SUB?n\SO", richFieldValue = "\ESC\68645wJ\CANJd[h]f6M\1000330/\NAKV\EM\1006827jV"},RichField {richFieldType = "Za\26190R\181771\1007416\140750\ETXc\a[\t\134134!<Pr\1050575\1005655\SOK\ESC_\1076275!\70672t", richFieldValue = "?"},RichField {richFieldType = ";\1107658@O_C&\v\150951=ey", richFieldValue = "O{WT\SIU-!B\98644Vq\1083421y\139792\187870Q>"},RichField {richFieldType = "D\78424(", richFieldValue = "\ESC&nA\156388"},RichField {richFieldType = "'1Aw\1061783\25320H\127828\165426\DLE", richFieldValue = ">\v{n\1095506\644\158339p\ENQ\96519\EM\150440\1038126\nc*."},RichField {richFieldType = "\US", richFieldValue = "\189599\DC4A\38048\1065662UX\6259J6\DC4\3530;7X\62961M\119903\187736\DLE&[\1049004\1005717!"},RichField {richFieldType = "\DLELr@c1\CAN#Iw\1035728uD_TO?a[\"^", richFieldValue = "\1094826\173858\n\139785ub\DC1q}\1040936Ei|\tD"},RichField {richFieldType = "L^UhCB", richFieldValue = "\ACKg\164267\SUB7\40266FU\EMM\1010150;"}]}
testObject_RichInfoAssocList_user_4 :: RichInfoAssocList
testObject_RichInfoAssocList_user_4 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "", richFieldValue = "\CAN=\tz\FS:q,|y\STX"},RichField {richFieldType = "\1084967\999998\&8\ETX\138621$\42294\44978\SOH\100993NJ\NAK\4108\SUB\164584\50590\NUL#t9\27718V\184399", richFieldValue = "9\vLV\SO\1023641\1047318\&5Ir4LF\185709"},RichField {richFieldType = "*\EOT\1039737DDb!\SYN\15607\983894\&3{\1016074\1036742}#E\ETB<\1759\1037105V", richFieldValue = "w\STX\SOH\n\53522\&4\STX\1086551R\f\\"},RichField {richFieldType = "\156475\EM\16470$?=?\STX", richFieldValue = "3\"\1100470\DC3FEu*\ENQ\1046807\1104419|:\EOT\GSFw2M"},RichField {richFieldType = "q\983296G\136959d[\"\vsc}LU\FS\a89+\165532\142927\&9I\37152P\b44", richFieldValue = "\74592\1079548Q~Eh\t|n\49506\SUB\1043908O\"<[u\12344\SYNF\DC1\SOHg\15622"},RichField {richFieldType = "\ACKNK\ETX\1084437\"\1046580d\48595&O\138214x>", richFieldValue = "\\\139444^]"},RichField {richFieldType = "mX!Syk|\v\STX?i^", richFieldValue = "t\12015B"},RichField {richFieldType = "-\171558z\SOz2d\1048381:\EM%Be\ETX\NUL9", richFieldValue = "5\1012631M"},RichField {richFieldType = "O\161585\1032915\&1B\\", richFieldValue = "}\n\36411BO\173990\74761Y\"R\a\149486q_\127848\SUB3\987194\187922N\1106264\5419c\1040287^o}iK"},RichField {richFieldType = "Z", richFieldValue = "~\154905\44383^\10130%L5\189377`\" 0cl$M\b@~5i\SO_$:"},RichField {richFieldType = "tZ", richFieldValue = "\ETB\FS\1069769\1054264\58206\135049P\DELh\1021124M\\Q\SUB7FeN[~"}]}
testObject_RichInfoAssocList_user_5 :: RichInfoAssocList
testObject_RichInfoAssocList_user_5 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "!\RS!b\SYNK\STX'GB", richFieldValue = "\1021760w]\DC1}k\987036\ACK\NUL\ETX"},RichField {richFieldType = "\1098004\n&\27810\1052466\n`{V&0\166949\98544\tW", richFieldValue = "*\1094586X\\\EOT}\1036527{"},RichField {richFieldType = "\142645\110601_}\68010q:[zv\\i\1054526aL\1079257\23613]\SUB\"\998488\1084327\DEL", richFieldValue = "N\v\CANS\n]:9K\1011800R&\96678\ESC\DC3\r(y\SUB\SO\RS_.\1109421j7A"},RichField {richFieldType = "%\24556B\77827wnN9\1002247(\SOHN\f %\1030999?\1100581W\DLE>$Q", richFieldValue = "\NAK!Lkw"},RichField {richFieldType = "\1083456", richFieldValue = "n&\ESCZYx\RS\ESCn\DC1\99120kg#"},RichField {richFieldType = "=h~Xs&gk\GS`y\1030704", richFieldValue = "\ETB\25857\NAKb\41748\45323q\986862\FSG/\189121\DC2\1111210M\SO0|\b\1061889V\1034957vTX'"},RichField {richFieldType = "F\3393\RST", richFieldValue = "d\1095173rt\1009482\1012936U,O{9\DC3\1037644\183107zd=}E"},RichField {richFieldType = "p\95230 \DC1 \1087739FdaD8\38155g\147566SN\1037832H\1007193\SOHuxd\162339Un/S", richFieldValue = "\aB\GS<\1033211\a\n\173916\49324KD\1030484\ESC\1062972_3A?"},RichField {richFieldType = "q\21180\t\1104733\&2q\STX.", richFieldValue = "\ENQO\49931\&7\1049712b\1029333\1010765\&4\NAK\DELM\ENQ\1015170&\1067082\917879\SUBQuQ\1031928\96599b\100861!|`\DLE"},RichField {richFieldType = "\54354\EOTc\1074505[k", richFieldValue = "h)y\171211\SO\1031327uyBe\RS\EM["},RichField {richFieldType = "\1073518.%P~z\ACKTk4xT\DC4S\SOHB", richFieldValue = "\1017043A\DC1G\SIPJ\1025866qk\121292\83192!I\1036489\r\a\38386\\1\1069120\23748"},RichField {richFieldType = "v/\1015268E?fg\DLE\146027\SYN", richFieldValue = "u9dGp$P\EM\145626kx\ETBJw"},RichField {richFieldType = "'A\EMB\GS0\160185\DLE\SIdp9z_&{Yy~\100884\b\CANQ\1015037k\USu", richFieldValue = "\DLE\165611Pb\1109678\141810\1066225t\995414HY|\1037230"},RichField {richFieldType = "\DC1a1\1077399.[<&sFgwTOo\189649\36084ZIF>6TJ\DC3\FS(2", richFieldValue = "\EOT>."},RichField {richFieldType = "\154606%qM\STX\US{9vB\EOT\985398\1033428\1015912\162210OJ\194926\1100959y\38169X\SYNn\v#", richFieldValue = "q\15253\&49(\1031431\1017088\23728\CAN>X\119557\DEL\1060091\1062129\DC4\SUB\1049950\r\1023209"},RichField {richFieldType = "]d\1068831Hm", richFieldValue = "x\1106503n\GS0XF@\1095720[m"},RichField {richFieldType = "\189888jkg\v\STX_w\991404\"f\ACK\fU#Zw)\raHGE\185758\1065361\ESC\DC2\DELd|", richFieldValue = "\ETBQ#|9\US\"(9/`@\DC2N\1015517~\RS!Z\186108n1NL1\17392\ENQ\63726)o"}]}
