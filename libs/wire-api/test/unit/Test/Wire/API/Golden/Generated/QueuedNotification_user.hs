{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.QueuedNotification_user where

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
testObject_QueuedNotification_user_1 :: QueuedNotification
testObject_QueuedNotification_user_1 = (queuedNotification ((Id (fromJust (UUID.fromString "00000040-0000-006d-0000-006100000065")))) ((List1 (NonEmpty.fromList [fromList [(",\31596#B*Ei \b\83180\991891",Array []),("ZaZU<\viD\GSF\n",Number (7.0e-9)),("\US\46244\DC2227t7\16757\156023N_\1094136}",Array [Null]),("\1040363\176399\1007480\1059405lxl\\2",Object (fromList [])),("u>k",Array [Number (400000.0),Null]),("|\SI\SO\990043\185929_'\RS\69402\43701",Object (fromList [("\\",Number (0.1)),("=",Null),("0",String "\t"),("",String ""),("\135516",Null)])),("-\fS\RS:\1036695\a'",Bool False),("_\152114k)yS`\NUL+4\1082329\67737",Object (fromList [])),("\1053270ZI;\1016340\\\150771",Array []),("b|U)x\27925<\DC1",Object (fromList [("\170598",String "\53663\1068087\&6"),("=",Number (2.0)),("",Number (-2.0e-3)),(">",Bool True),("a",String "{\DC1")])),("\1015779oX5xE=nTM\17814",String "\992204~j\146841,\CAN"),("\FS0\1075496\SUBg\38628~s\35550j6fh<",Array [Bool False,Number (0.0)])],fromList [("\"\178414",Object (fromList []))],fromList [("{;",Object (fromList [("\1083085?",String "")]))],fromList [("",Object (fromList [("",String "\41973")])),("\160209\132741",Null)],fromList [("<\CAN",Array [String ""]),("\USD",String "\1014796")],fromList [("?\ENQ",Array [])],fromList [("o\1106061",Number (-2.0e-2))],fromList [("",String ""),(";",Object (fromList []))]]))))
testObject_QueuedNotification_user_2 :: QueuedNotification
testObject_QueuedNotification_user_2 = (queuedNotification ((Id (fromJust (UUID.fromString "00000006-0000-005d-0000-007c00000036")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("N-\ETBD\159887",String "\1066652;\SOHHz7"),("`7dp)\52799\&9\153991\ESC\1052019",Array [Null,Null,Bool False]),("\1024427\64174Kd\17183|2D\f\SOHx",Array []),("\EMZ\1064658TOu\1091594R\993205@W\1008087",Array [Number (-1.0e-6)]),("\vk\60050{\157604O\13640$",String "BB\1026379Ym1\1039162B\121023\1002594\44629"),("CT\61704\151922\1015894\60029^\ESC\ESC\161966",Object (fromList [("",Number (500000.0))])),("\SYNzN1\1058402*'Y\SOH\SYN-\100975G",Object (fromList [("\CAN\ETXI\29408KH'\f\\c^5E",Bool True)])),("6a\1051570\f",Object (fromList [])),("\186130\1087671\&3@\EMP/_\ESC\1083805",Array [])]]))))
testObject_QueuedNotification_user_3 :: QueuedNotification
testObject_QueuedNotification_user_3 = (queuedNotification ((Id (fromJust (UUID.fromString "0000000f-0000-0052-0000-006100000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [(",\t\148835\&5",Bool False),("7;\27620b4~\66232!QE^\ENQD\ETX",Object (fromList [("",Bool True),("q",Bool True),("A",String ""),("\STX",Null),("%",Number (0.0))])),("\FS\RS\ACK",Object (fromList [("y\19845Q\11363?)-\1008763\SOHd\1052546\40768\&5\SI",Bool False)])),("\b\1009026_\166621\1022935\EM:j\DC3\NUL\EOT!\NUL\167403\68489",Array [String "",Bool True,Null]),("\1008816\ESC\v=]A\78486\30430\RSDd",Array []),("3",Bool True),("So~\1068988",Number (4.0e-3))]]))))
testObject_QueuedNotification_user_4 :: QueuedNotification
testObject_QueuedNotification_user_4 = (queuedNotification ((Id (fromJust (UUID.fromString "0000003f-0000-003d-0000-004f00000070")))) ((List1 (NonEmpty.fromList [fromList [("\SOHv\v",Array []),(")BW\152285\b\GS\83019\DC36\b1\NUL",Object (fromList [("H\1022601\f",Null),("\FS",Null),("",Bool True),("\1055348\SOH#",String "]")])),("if",Bool True),("\179852\45158Us",String "\63370\7299\145660\NULkK\SO\v"),("\24660+5Uz-HCL%.*`\1007969\64757",Object (fromList [("\1051479",String "\1026887"),("P",Null),("",String "]"),("\128208",Null)])),("\f\"\1079160y\b\1064557",Array []),("\NUL\178838$\1028887\20950f\1012418O`G\990921\1055139",Bool False),("H\28612V\SI\180714\USn\998376\ACK",Array [String "\STX",Bool True,Bool True,Null,Null,String "\186323",Number (-2.0)]),("z\94762mYOA@\1021732t\180119\1047300",String ">\nt?y(\1012816sb,E+g\GS"),(":2\DC1",Array [String "N?\7147>K\33996"])],fromList [(")<s\SYN,\1052393cQb\993854\ETB\142679+\67705\1020749",Array [Number (-2.0e-2),Number (-200.0),Number (-0.2),Number (2.0e-2),Bool True]),("CZ\156217\DC3]\1040332\1000899poC",Object (fromList [("\48246",Bool True),("0",Null),("",String ";"),("\1049571",Number (1.0)),("\27709",String "\128784")])),("%\165256\148887\\V\ETB\93036\CAN",String "Y\f\179442"),("",Array [Bool True]),("\DC2\44010X\1090677`_\DELC9\SUB",Object (fromList [("w\DEL",String ""),("v\1107855",Bool False),("",Null),("g\ACK",String "")])),("^P\DC1\994393E\1014732\66793\&48",Number (10000.0)),("}\985210\NUL\128200m\140869B\RS\1027255k\74223\1113408K\31809\148344",Object (fromList [("",String ""),("A",Number (1.0)),("D",Number (-1.0)),("\NAK",Null),("\DC3",Null),("\ACK",Number (-10.0))])),("\DEL\183039uF^o.\63469e;\n\f\b",Bool False),("\1094580R]=?7)",Array [Number (3.0e11)]),("\8002\DC2\136687\136732[\184223U\183606E!A\1055436\NAK\98628",Object (fromList [("\164841A\1058769\\\SI",String "^"),("\179443\137523l\1063585",Number (-20000.0))])),("\ra\1041639\1045953\1073300>3\DC2Uu",Bool True),("\CANv\53216#\ETB\61198~\ETBIv\1093243\"",Object (fromList []))]]))))
testObject_QueuedNotification_user_5 :: QueuedNotification
testObject_QueuedNotification_user_5 = (queuedNotification ((Id (fromJust (UUID.fromString "0000007b-0000-0028-0000-004400000010")))) ((List1 (NonEmpty.fromList [fromList [("@\SI\68319#\aO3\ACK^,=b&({",Object (fromList [("7",Null),("",Null),("\1020752",Null),("{",String "W"),("\47000",Number (-1.0))])),("\33184\DC1w\1074017_4I\120705",Array [String "",String "\1083450",Number (0.2),String "",Null]),("\ETB\190215\CAN\1109396",Array []),("t1HZ\ETX\ETX`4\1025736=H4J&\1111066",Bool False)],fromList [],fromList [("",Null)],fromList [("\EOT&\177055",Object (fromList [])),("",Array [String "",Bool False])],fromList [("]",Array [Null]),("ESo",Number (3.0e-3)),("qa",Object (fromList []))]]))))
testObject_QueuedNotification_user_6 :: QueuedNotification
testObject_QueuedNotification_user_6 = (queuedNotification ((Id (fromJust (UUID.fromString "00000037-0000-000d-0000-002300000020")))) ((List1 (NonEmpty.fromList [fromList [("\ETB\DC1u$Y{\142501q:GF%\37565\RS",Bool False)],fromList [],fromList [],fromList [("i?v\28915\1058450",Bool False)]]))))
testObject_QueuedNotification_user_7 :: QueuedNotification
testObject_QueuedNotification_user_7 = (queuedNotification ((Id (fromJust (UUID.fromString "00000014-0000-0012-0000-000300000058")))) ((List1 (NonEmpty.fromList [fromList [("X\EOTF\1113893",Null),("\"zJ==\1028053\166569S4",Object (fromList [("\a",Number (3.0e-3)),("\US",String "\SOH\1009024I"),("e",Bool False),("CmU",Number (0.0))])),("-A",Array [Number (0.0),Null,Number (0.0),String "e"]),("j\1087779",Object (fromList [])),("+R^\DEL\155210N+lesXy",Array []),(";x^~`s,QPp",Number (-1.0e15))]]))))
testObject_QueuedNotification_user_8 :: QueuedNotification
testObject_QueuedNotification_user_8 = (queuedNotification ((Id (fromJust (UUID.fromString "0000002e-0000-0051-0000-00660000007b")))) ((List1 (NonEmpty.fromList [fromList [("\1019793Gp\SO",Object (fromList [("\149922",Null),("\DLE",String "\ESC"),("",Number (0.0))]))],fromList [],fromList [("<",Array [Null,String ""])],fromList [],fromList [],fromList [("\180547",Object (fromList []))],fromList [("",Object (fromList []))],fromList [],fromList [],fromList [],fromList [("}",Object (fromList []))],fromList []]))))
testObject_QueuedNotification_user_9 :: QueuedNotification
testObject_QueuedNotification_user_9 = (queuedNotification ((Id (fromJust (UUID.fromString "00000018-0000-003d-0000-001f00000030")))) ((List1 (NonEmpty.fromList [fromList [("2\DC4\172508\DC3(%\173506O",Object (fromList [("",Number (-10.0)),("\1048293",Number (100.0)),("yg",Null),("f?",Null)])),("t\1076573\1032388v\52815\993296\DLEh\DC4-\1045580fk",Null),(">*\1098640",Object (fromList [("FA",String "[;"),("\1091966\NUL",Number (2.0e-2)),("",Null),("\DC1\STX",Bool True),("%5",Null),("S",Bool False)])),("",Array [String "",Bool True,Null,Null,String "xT",String ""]),(";h\3673\DC4\987084aV\\)\DLE\DEL\FS",Object (fromList [("\1094813R",String "b"),("x\ETX\1092104",Number (-300.0)),("t_^",Number (300.0)),("0",String "}>"),("",Number (-20.0))])),("q#vp0O\148378\"H",Object (fromList [])),("xFa0]||\1095824j=",Object (fromList [("b\CANV",Bool False),("\CAN|*",Bool True)])),("\164287pUo\1081671v\b",Array [Null]),("\144245t=\DC1o\ENQ\DC1\1067573",Object (fromList [(",",Bool True),("",String ""),("\SOHTd",String "\1039260"),("\1027387",Number (300.0))])),("\120925sAa>m ",Null),("(\DEL\FS\NAK$D\SOH",Object (fromList [("\190010",Null),("\NUL",Null),("",Null),("1",String ""),("\148755",Null),("_",Null),("\US",Bool True),("H",Null),("v",String "\158708"),("\29094",Number (1.0)),("i",Bool True)])),("H&:\74769_\GS3\996325T\40211(\35030",Number (4.0e-10)),("\128961\&4\1048824\n",Array [Bool True,Bool True,Null]),("&6\1041661\SYN<\EOT`\1082659\1077308)9",Null),("'IH\f|\n,\ENQA\140095^\151816F",Object (fromList []))],fromList [("\STXN",Array []),("P\SOHj\602",Array []),("xY\ESC",Array [Null,String "",Null,String "",Bool False,String "",String "",Null]),("\r\167753\DC2\127376/",Array [Bool False,String "",String ""])],fromList [("",Object (fromList [])),("\DEL",Object (fromList [("",Null),("Q",Number (0.0))])),("\SI\1089757h",Array [Null])],fromList []]))))
testObject_QueuedNotification_user_10 :: QueuedNotification
testObject_QueuedNotification_user_10 = (queuedNotification ((Id (fromJust (UUID.fromString "00000052-0000-0054-0000-004100000004")))) ((List1 (NonEmpty.fromList [fromList [("\24428/\128204\1054866o\t\GS\1003774\180091",Object (fromList [("z\1101448",Number (-2000.0)),("",Number (-2.0e-2)),("D!",Null),("gYe",String ""),("ih",Number (2.0e-3))])),("\DC22>",Object (fromList [("_rD3\26618<\179618r~(.\63779\&7",String "X6\RSvt\NAKna2$\1078184GI\645y")])),("=r,\DC1c\45123o@\DC3\1106982\44992y\1071059",Array [String "",Bool False,Number (0.0),String "\ETX",String "",Number (0.1),Bool False,Null]),("\48605T1\191168\&7\ENQ<\ESC",Object (fromList [("m\bV\1043515\1069896\40608",Null)])),("9\SI\154911L",Number (1.0e12)),("\a\ACK\SYN\986135DU}\NULK",Object (fromList [("L\1017382:]\150192\EOT",String "R\1076557\&8\151613d3)"),("_)\179177\DC2",Null)])),("Q\STX\96952J",Array [Number (-1.0),Bool True,String "\160478",String "7b",Null,Null,Number (1.0)]),("\NUL\n\1081104",Null)],fromList [("L",Array [Number (0.1),Null]),("\vT\1090689",Array [String ""]),("\149334p/&J",Array [Number (30.0)]),("\ETX\1070573\SYNgi",Number (-300.0))],fromList [("\1087108",Array [Null,Bool False,Number (0.0)]),("",Object (fromList [("\1108406*(`\1062051",Bool False)]))],fromList [("@R\nE",Object (fromList [])),("\1081982jwQ",Object (fromList [])),("",Object (fromList [("",Null)]))]]))))
testObject_QueuedNotification_user_11 :: QueuedNotification
testObject_QueuedNotification_user_11 = (queuedNotification ((Id (fromJust (UUID.fromString "00000069-0000-0024-0000-002500000006")))) ((List1 (NonEmpty.fromList [fromList [("(\DC1Z\EOTu1F\74632\120755\f",Array []),("n\42772\DC2\SUB\SYNF7BM\1008585\995550a",Object (fromList [("m",Bool False)])),("(~?\997224T9\113767lMH\DLE\1099515\1109698\176219",Null),("v\3095\f*\DC2n\STXu\7655\STXP@\r",Object (fromList []))]]))))
testObject_QueuedNotification_user_12 :: QueuedNotification
testObject_QueuedNotification_user_12 = (queuedNotification ((Id (fromJust (UUID.fromString "00000015-0000-0067-0000-000200000027")))) ((List1 (NonEmpty.fromList [fromList [(",\GSY\RSp\1009675JHL\ENQ\DC1\DEL\41249\DC3\DEL",String "dt\SYN\1058594?\ETBQ"),("[[\SYN\ETX",Number (-9.0e-2)),("",Array [Null,Bool True,Bool False]),("?\40658p\1105823\1083514\1063438\ETB",Object (fromList [("#\1055499\12978",Number (-2.0e-3)),(" ^$",String "\NAK\995845d"),("E\20361",Null),("S\SUB",Number (3.0))])),("e\1090838=]\DELFu\ACK\8868-\177346\983174{",Null),("]_\US\FS",Array [String "\NULOu\DLE",Null,Null]),("\ESCEb7\1046841\24027\40374\SI\ACK^\n*z\DC3\GS",Array []),("\61765\&5H\SOH\25393",Object (fromList [("\1092933",Bool False),("|",Number (1.0)),("'",Null),("",String "O"),("{",Null),("\DC1",String ""),("\DC2",Null),("#",Bool True),(")",Bool True)])),("Y\1082354\1004735+WQ",Bool False),("\r\1069449)\NUL",Array [Number (500000.0),Null,Null]),("\1077874\"S\1109537q\57452\CAN(\US",Object (fromList [("",Bool True),("k",String "'"),("O",Bool False),("\"",Bool False),("\1053039",Number (1.0))])),("Te<V\1007977\51864i\USd\SYN\GSUa",Array [Null,Null,Bool True,Number (0.2),Null,String "\DC3"]),("\44611kG",Object (fromList []))]]))))
testObject_QueuedNotification_user_13 :: QueuedNotification
testObject_QueuedNotification_user_13 = (queuedNotification ((Id (fromJust (UUID.fromString "00000043-0000-0079-0000-001100000035")))) ((List1 (NonEmpty.fromList [fromList [("g\22104G\1061226\&4\1047911\f\STX",Object (fromList [("",Number (1.0e-3)),("xF",Null),("\STX\138192w",String "IjS")])),("\140654\165695\SI@",Number (-5.0e-9)),("5<\STX\140152 \28376\43702\ACK\170028",Array [Bool True,Bool True,String "m",String "\SUB",String "",Bool False,Bool False,Bool True]),("",Object (fromList [("C\1055376\991162\94414\1062158\vj",Null)])),("\EOT\1022915z\SUB3\98889y\NAK\59619:",Array [String "\142129",String "T",String "",String "",Bool True,Number (-0.1),String "",Number (-1.0)]),("^\178345[7",String "\FS\144490t\bMdzN@"),("\STX;7b",Array []),("5I|\1050790\ESC\38964\139737AA",Null),("6\RS\984092!)?\96478\53719U\n",Array [Number (-7000000.0),String "t_\49434\f/\1037425\42153"]),("\1002676\CAN\110820\1107709\&6\138745!\DEL?\24764",Number (-30.0)),("\n*\1075615\&6+\14484qXA\34121j,\b\b",Object (fromList [("{I",Bool False),("\FS\SYN",Bool False),("-",Null),("",Bool False),(" ",Number (0.1)),("y",Null)])),("F",String "Z\70146G\8697\1022898N\SO"),("N\46657w\26366\FS\"\999255U\b\DLEw\ETX|/H",Object (fromList [("l\12480",Null),("",Null),("\US",Bool False),("\184762`",Bool False)]))],fromList [("\fZ\47845E",Array []),("c\\\DC3",Array [Bool True,Bool False]),("?4]5",String ""),("Cb\b",Array [String "",Bool False,String "",Number (0.0),String "",Number (0.0),Number (0.0),Bool False,Null,Bool False,String "",Bool False,Number (0.0),Null,Number (0.0)])],fromList [("",Object (fromList [("",Null)])),("\1095876\ENQC9",Object (fromList [("\13857\DEL",Null),("\1059773C",String "{>")])),("\166676\SOHY",Array [Bool False,String "",Number (0.0),Number (0.0),Number (0.0),Null]),("\ENQ\1110994",Number (-3.0e-5))],fromList [("\SYNC7",Object (fromList [("\f\1032335",Number (-5.0e-3))])),("\1060995f]\1052112",Null),("",Array [Bool True,Null,String "G",Number (-0.1)])]]))))
testObject_QueuedNotification_user_14 :: QueuedNotification
testObject_QueuedNotification_user_14 = (queuedNotification ((Id (fromJust (UUID.fromString "00000063-0000-0025-0000-003400000021")))) ((List1 (NonEmpty.fromList [fromList [("0",Array [Number (-200.0),Null,Null,Number (0.3)]),("eA\EM",Null),("#\RSK\178843\1038505\fm)",Object (fromList [("\STX",Number (2.0e7)),("l\11654\30346\GS\GS\99070\1040473",String "\".\r")]))],fromList [("\11825\DLE3",Bool True),("Y\160654M\997494,\SUB.\ACK",Bool True),(";\1005289c\ETBU\8409\&1\SO",Array [Null,Bool True,Bool False,String "\ETX{="]),("vJ\ESC\35470\ACKgb\45764&kou?",Object (fromList [("",String "\ENQ"),("`",Null),("[",Bool False),("\1039736",String "")])),("]\131854\1034266\ETB\15636z\a}[\DC2\153182",Array [Number (-1.0),Null,Number (10.0),String "\1025768",Number (1.0),Null,Number (-10.0),Number (-1.0),String "",Null,Null,Number (0.0),String "",Number (0.0),Number (0.0)]),("\174917\\_\v\152841\NUL\DC2E\rj_yK\179280",Object (fromList [("Q\NAK1",Null),("",Number (0.0)),("\1032675\ESC",Number (-3000.0))])),("\165711",Array [Null,Bool False,Null]),("]\b\t9\SOH\184881",Number (1.4)),("&FO/!\1095078p'G\72727\US\DC1Z\1100810\1020939",Array [Bool False,Bool True,Number (3.0)]),("(1\SO|fE)\\}\ENQ",Array [String "\38211",Null,Bool False,Number (-100.0),Number (-20.0)]),("8",String "\US\ETX\DC2p=z=\US.H\a\a{\155632\164739"),("2\26793\CANZs8n\SOH%.C]",Array []),("\v!\STX\NAKE\FS\132367",Object (fromList [("?\r",String ""),("m",Number (3.0e-2)),("x\DC1l",Number (0.0)),("\176622\ETX",Null)])),("\NUL9",Array [])]]))))
testObject_QueuedNotification_user_15 :: QueuedNotification
testObject_QueuedNotification_user_15 = (queuedNotification ((Id (fromJust (UUID.fromString "00000040-0000-0067-0000-00000000003e")))) ((List1 (NonEmpty.fromList [fromList [("s\1093512",Bool False),("{\1079502\1039894V\136065_[7DT\144576\EMg",Object (fromList [("\n",Bool False),("",Bool False),("\v",Null),(">",String ","),("t",String "\r"),("$",Number (-10.0)),("\ENQ",Bool False),("\988759",Number (1.0)),("\t",Number (-1.0))])),("Fllj\1038845\189078\DLE \DC4Fj\78032[\990888",Object (fromList [("",Number (0.0)),("\1026565\DEL\1050633",Null),("_\ENQ",Null)])),(",E\74344\&27\154097\17968pi\vq\SIBS",Array [String "\165597\1058671\183726/"]),("\1063819\NUL&b\151547\1003549\GSO ",Null),("$\\\\0\SYNWRR",String "\1072494X|p5"),("\CAN",Number (30.0))],fromList [(".b\29014\EOT~\ENQG",Array []),("U\vO\24094",Object (fromList [])),("\GS!",Null),("~O\t\DEL",Object (fromList [])),("vV\1052114\&6\SOH\1068582\128633",Array [Bool True])],fromList [("1G0",Object (fromList [("\a",String ""),("",Null)]))]]))))
testObject_QueuedNotification_user_16 :: QueuedNotification
testObject_QueuedNotification_user_16 = (queuedNotification ((Id (fromJust (UUID.fromString "00000020-0000-001a-0000-005600000031")))) ((List1 (NonEmpty.fromList [fromList [("\1033743\RS0\"5",Array []),("",Object (fromList [("g",Null),("",Null),("{",Number (1.0)),("D",String ""),("\125033",String "")])),("\157711H\991061\USK&\SYNXY8X8\"",Array []),("{h\ETXl\1065459:\46517O\178781\71862\1051559\t",Bool True),("J~qxfOB",Object (fromList [("*",Number (1.0)),("",Number (0.0)),("n",Number (-1.0)),("\985903",Bool False),("U",Number (-1.0))])),("^\DLE`\70070\&2",Null),("\ACKS\DEL\1111764@k%",Array [Null,Bool False]),("\1078812\&50x\1004575\ETX\1064801\1036366&\SI",Bool True),("l(01M\EMeB",Null),(" &",Object (fromList [("",String ""),("\1036271",String "\DC2"),("T",Null),("\189625\SYN",Number (0.0)),("\DC3",Bool True)]))],fromList []]))))
testObject_QueuedNotification_user_17 :: QueuedNotification
testObject_QueuedNotification_user_17 = (queuedNotification ((Id (fromJust (UUID.fromString "00000067-0000-000c-0000-006d0000000a")))) ((List1 (NonEmpty.fromList [fromList [("\1111572*\996616\13282",Bool False),("Vg\1048285-Pv\USTl\tE\ENQ",Object (fromList [])),("\1065880^\176869\&5\28912 [\145469\1011021\CAN\1027259gI",Array [String "\171533",Number (1.0),Bool True,Bool True,Number (0.1),Bool True,Number (-0.1),Bool True]),("\1010705\132830kZ\b<!",Number (-8.0e7)),("\20909\"\999938\"5",Array [String "gb7c"]),("\1071460}\DEL=]\1109580$\9439.IV-(",Object (fromList [("",String ""),("~",Null),("\1099420",Number (-10.0)),("2",Number (-0.1))])),("\r\38906H\15774?\\q\v",String "\1099585\SUB\175261\1007477\181298G)\1078966/=Z\USm"),("%\100302\1097039",Object (fromList [("",Number (0.0)),(" ",Bool True),("\1075202",Bool False),("#c",String "\37987")]))],fromList [("\46443\1089858\991076\43417\r",Array []),("jc\EM\DC2",Array [Null,Bool False]),("",Number (2.0e-2))],fromList [("P",Array [Number (0.0),String "",Null,Bool False,Bool True,String "",Bool False,String ""]),("",Array [Null,String "",String "",String "",Null,Number (0.0),String "",Null,Null,String "",Number (0.0)]),("z\153799\1074662",Array [Bool True]),(".\DEL\1091559)",Object (fromList [("\1022491",Null),("J/",Bool True)]))],fromList [("\1079252bh&!",Number (0.2)),("\1074192%\150007]o",Object (fromList [("",Bool True)]))]]))))
testObject_QueuedNotification_user_18 :: QueuedNotification
testObject_QueuedNotification_user_18 = (queuedNotification ((Id (fromJust (UUID.fromString "00000029-0000-0023-0000-002500000078")))) ((List1 (NonEmpty.fromList [fromList [("!\EMD\1062170K\DC4MT3\69379O\US\ACK\21448",Array [Null,Bool False,Number (-300.0),Number (0.0)]),("\DC1<\78405\ACKgj\65590b*<Bd`\1059622",Number (-100.0)),("\21149p 7\1073281\145879\235",Object (fromList [])),("$+\SI\2070M:\1106696\b\1112909Fz8",Object (fromList [("\1075183f\158677~#!@0\1040379!r\990801\NAKD",Number (-1.0e-14))]))],fromList [("E\ACK\1010644\1054447\1046272\133460~",Object (fromList [("G",Null),("\DLEX",Bool False)]))],fromList [("\83224",Array [Null,String "3",String "",Bool True,Null])]]))))
testObject_QueuedNotification_user_19 :: QueuedNotification
testObject_QueuedNotification_user_19 = (queuedNotification ((Id (fromJust (UUID.fromString "00000011-0000-0037-0000-000f00000026")))) ((List1 (NonEmpty.fromList [fromList [("0\\ \\wH",Number (-2.0e13)),("g\a\154019\SO[$&S\NAK8w%",Object (fromList [("\t\r",Bool True),("",Null),("U",String "&."),("\996087",Null),("\57913",String "a&")])),("{&S\1086604\1003887\1088032\&7^",String ","),("\25814\1033691\r>\NULA3<C\SYNIe\1052612\128293\1105028",Object (fromList [("",Null),("\r\SOQ",Null),("\40619",Bool False),("\21392|",Null)])),("\"\1046616H/\ETX\DC1w\34317\SI<X\1105473\DC4k",Array [Null,Number (-10.0),Bool False,Null,Null,Number (0.0),Number (0.0),Null,Null,String "d"]),("\tp\EOT*\CAN)\141408",Array []),("\SYN_J{",Array []),("\1024214\1043352#Zo+\STX\1055521\34715&m",String ""),("\175889",Array [Bool False,Null]),("\ESCrL\DC4\1101625\1080623:\1056723^\aI",Array [Number (3.0e-3),Bool False,Bool True,Bool False]),("\995183tk`\b\ENQ",Object (fromList [("\998441",String "\61708"),("",Null),("5",String ""),("o\133583",String "Gd")])),("Dc\1057365\&9&\".\ESC(",Object (fromList [("<",Number (-7000.0)),("/",Null)]))]]))))
testObject_QueuedNotification_user_20 :: QueuedNotification
testObject_QueuedNotification_user_20 = (queuedNotification ((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-006700000014")))) ((List1 (NonEmpty.fromList [fromList [("8}Ar]\1109220\DC1\1084111\1061092\1017946?g.b6",Array []),("\aF\73763\26649",Array [Number (2000.0),Number (30.0),Null,String "kn",Null]),("\189127VOmJ\185478,\58858",Array [Number (-1.0),Null,Null,Null,Null,String "\13932",String "",Number (0.0)]),("",Object (fromList [("\1026716",Number (5.0e-6))])),("`fn9u\62184J\1056138JOv\1091150",Array []),("\1038251F4\175644\187760Al\134758\\\EOT\173005",Object (fromList [("f\1087917\994399",String "F\\Wk\75024=1\\")])),("x]\1108969\&9\182025>\RS7\40229\191162p",Number (-12.0)),("\11526x%\nQ<",Array [String "iJ\DLE",Bool True,Bool True,Bool True,String "*\1031676\NAK"]),("\51819:B`v\DC1\FS",Number (3.0e-5)),("x\SYN\2224\131672\992697\1004040\SO\177869\62762O9nn",Bool True),(" \CAN\1041688\t\72971@\1076045\1038503\1045679",Array [Bool True,String "D\EOT;\194911?cj"]),("T\"Y\1005417?\\FzI\1090610:?\FSs\35245",Array [Number (0.0),Null,String "",Bool False,Null,Null,Bool False,Null,String "",Null,Null,String "",Number (0.0),String "",Bool True,Bool True,Null,String "",String "",Bool False,Number (0.0),Null,Bool False,Bool True,Null,Number (0.0),String "",Number (0.0),Number (0.0),String "",Bool False,Null,String "",Null,Bool True,String ""]),("\990558\ENQfyL|\"V~",Array [Number (0.0),String "mo\1042021"]),("\984226\74339dJO\SYN\ETX\DC1(%Q\1017091g(",Array [])],fromList [("\1028673",Array [])],fromList [],fromList [("",Object (fromList [("",Number (1.0))]))],fromList [],fromList [("",Array [])],fromList [("\184358",Array [Number (0.0),Bool False,String "",String ""])],fromList [("Z",Array [Bool True,Null,Number (0.0)])],fromList [("",Array [])]]))))
