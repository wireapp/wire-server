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
testObject_QueuedNotification_user_1 = (queuedNotification ((Id (fromJust (UUID.fromString "0000006d-0000-005f-0000-004c0000006a")))) ((List1 (NonEmpty.fromList [fromList [("}A\31810\ENQ\v?OXp\70294",Array [Null]),("\GS9\vR\DC4B~l\135908vn\1051726",Number (400000.0)),("\ETB",Array [Bool False,Null,Null,Null,Number (-10.0),String "kA",String ""]),("\1031314Ay\1091647N`\SOH3\NULhz\51946",Array [Bool True]),("\EM\59776f",String "p\1017787\DC4\as[\154757\134892T\NAK'\184473P\1113279\1083627"),("",Object (fromList [("",String "\1015570"),("\1075507",String "\29967"),("\SI",Number (10.0)),("\CAN",Bool False),("\30133",Bool False)])),("7mkc)",Object (fromList [("",Number (-3.0e-3)),("\t`",Null),("\"",String "\ACK\1089168"),("\"\1074602\&8",Number (0.0)),("&",Bool True)])),("-;",Bool False),("\151335<*/\DC1E{H\SYN\63299 *)I",Null),("4f\1094090\995428ap",Bool True),("V5\162277\1101934\&5\1073589\&1e_",Bool False),("\1003647",Array []),("\1698\&5\v56\1068571Dv\171563\153145U&\b=",Array [String "",String "\1017319\71709\1855F",Number (0.0)]),("\39624y\t\1013930<r",String "\EMI\183621\990504E\\M\1062522\r"),("\DC3y\1087297w9p\SYN\983721n\ACK4Q",Number (1.2e-2))],fromList [("\998585S",Null),("\46227\&0",Bool False)],fromList [("",Object (fromList [("",Number (-100.0))]))],fromList [],fromList [("Pj",String "R\SUB")],fromList [("",Null),(")",Bool True)],fromList [("",Null),("{",Object (fromList [("",Null)]))]]))))
testObject_QueuedNotification_user_2 :: QueuedNotification
testObject_QueuedNotification_user_2 = (queuedNotification ((Id (fromJust (UUID.fromString "00000042-0000-0069-0000-002e00000017")))) ((List1 (NonEmpty.fromList [fromList [("$\1059732\1056681\161910\1063233\ENQY\SOH\1023267\&8\1045378\DEL.n{",Array [Null,String "\r\1068756\ETX\1092114"]),("~\166510mB\USK\1104705N\9490X\t",Object (fromList [("hfP\73913\1089260y",Null)])),("\NUL\1074586Q\ETB\SYN\45076\156226\RSB\1069359\1080056",Object (fromList [("\ETB",Number (0.0)),("\1071530",String "v"),("",String "\1075156"),(" ",String "\52039"),("\1003455",Number (0.0)),("D",String "\1094983"),("%",Number (1.0)),("\1013980",Null),("\1082466",Null),("f",Null)])),("",String ""),("(}\16772\150212\164612b\166256\SYN\v0\FS\1089191",Null),("cWd=F\17853\r^d~\1078958\164216\ESC\1102921g",Object (fromList [("",Null),("N",Bool False),("\156117",Bool False),("\32213\FSa",Number (-100.0))])),("\ETBfb%\US\US\24436\\",Array []),("\991939",Object (fromList [("",Bool False),("Y\98888q",Number (-2.0e-2)),("+<\DC2",Null),("</",Number (1.0e-2))])),("3;\EM\"",Null),("\142283\1098034\n\100943N\1093932\DEL",Object (fromList [("",Null),("8\CANQ",Bool False)])),("D\DC4\163239\17306H\1017635io\1108126\129313\DEL\ETXd",String "w\99454/q[")],fromList [("5\SO(S\DC2\1053988\143536mQ5",String "\DELeg\54762\138691c\1030214#-p\US\1021124"),("u1\ETB\985510\SIn\GS\r\14296K,\DEL\37380",Null),("W$ft_\1035303",Bool True),("",Object (fromList [])),("\139995)z\"K\1110994+3\144141\1057949zY\DEL\a)",String "\917584\987352&l\172749\1007314Ig.\SI\DELT\DC3\n"),(">\SI+\DC2\ENQ1\FSN",String "\DC3^\ENQ\983382D\ETB ZaF\99023\1016321"),("k\1089676\164706@\CAN",Array []),("\a2\65682j\SOHq`\SYNS0",Null),("\97649\DEL2q\SUBkH\119238*ST\DC4.\EMH",Null),("]\EM\v\132502",Number (-8.0e11)),("!<",Array []),("\n\996073E\\iU\1007075H\1084793A@\1019774i\175574",Array [Bool False,Bool False]),("\55113\SYNDB8M.\EM",Object (fromList [("\NUL\CAN\GS<\1078257\DLE\1081888y\SYNNvL\ESC\1002566",Number (-100000.0))]))]]))))
testObject_QueuedNotification_user_3 :: QueuedNotification
testObject_QueuedNotification_user_3 = (queuedNotification ((Id (fromJust (UUID.fromString "00000019-0000-0004-0000-000600000056")))) ((List1 (NonEmpty.fromList [fromList [("",Array [Bool True,Number (-10.0),Number (0.0),String "L",Number (-10.0),Number (1.0),String "\DC1",Number (-1.0),String "",String "\f"]),("\43745\1039873\SI",Number (-1.4e-3))],fromList [("\161406\96934\148400U\SUBg",Array []),("\1000882\7792",Array [Null,String "",Null,Number (-1.0),Null,Null])],fromList [("~",Object (fromList [("",String "\1007655"),("\b",Bool False)])),("\48652\ETX",Object (fromList [("=)+",Bool False)])),("\DC3",Bool True),(" \bBc\185532w",Number (5.0e7)),("\FSxl\v",Object (fromList []))]]))))
testObject_QueuedNotification_user_4 :: QueuedNotification
testObject_QueuedNotification_user_4 = (queuedNotification ((Id (fromJust (UUID.fromString "0000006b-0000-003b-0000-00000000004a")))) ((List1 (NonEmpty.fromList [fromList [("tc/\\#\RS\SUB\177832y\1047750I\fr",Object (fromList [("",Number (-300.0)),("\1051194LF",Number (0.0)),("p\41124",String "XV7"),("u/",Number (-2.0e-2))])),("M\SUB\1077052\51560\1007849`IvTVXIe]",Object (fromList [("\v^)",Number (0.3)),("",Bool True)])),("\ENQ",Array [])],fromList [("",Object (fromList []))],fromList [("\92443",Array [Null,String "z>\SI"]),("\ESC\153049\161576\r;\ACKp",Object (fromList [("\1077664\EOTuI\17207\1039078",String "\985006\58283\&7\1057845NB")])),("",Bool False),("|N\FSz",Null),("\562\DLEt\1025300I\r&",Object (fromList []))]]))))
testObject_QueuedNotification_user_5 :: QueuedNotification
testObject_QueuedNotification_user_5 = (queuedNotification ((Id (fromJust (UUID.fromString "00000069-0000-000e-0000-000500000057")))) ((List1 (NonEmpty.fromList [fromList [("EG\GS3?\US?\t\29910\154394\DC2o",Bool True),("A2\1051166^~\1017113\1026926\95572\NAK1g\ETX",Array [Number (-1.2e-2)]),("yt\SIO\190357=9H\ESCN\188294@\1028400\DC4\DLE",Object (fromList [("\GS",String ""),("",Bool True),(".",Bool True),("n",Number (10.0)),("\SOH",Null),("\STX",Null),("h",Bool False),("\10593",Bool False),("&",Bool False)])),("\55047\GSh\1023427\GS|\983740\48370]+G",String "/\v"),("\DC44\DELg",Null),("",Array [Bool False,Number (1.0),Null,Null,Number (-1.0),Bool False,Bool True,String "",String "",String "",Bool True,Number (0.0)]),("\\-N<\60071\176144\62155",Array [String ""]),("EH`\6601l\1081485\FSz\DC3?\24537\1103103",Object (fromList [("\990297-",Bool False),("",Number (0.1)),("k",Number (-2.0e-2)),("\STX",Bool False),("\1034293",Bool False)])),("\59210\ETXf<\ETBI+/x\SYN",Array [String "\1033542C\94310\180260\78089"]),("\66320o4\ACKN[\1016940.I*\136343(\ETX\51054",Number (-7.0e-15)),("{\1034469/&",Array [Number (0.0),Null,Null,Number (1000.0),Number (2.0e-3)]),("\EMp\42340\CANRI#$S(1",Array []),("\176166\142770}\1033026<uydBj\ACK\ETX",String "O+WX\24573"),("\77943\135119\1104614\SO9w\999364r*\SYNe\"\n",Object (fromList [("k",Null),("2",String "<\t")]))]]))))
testObject_QueuedNotification_user_6 :: QueuedNotification
testObject_QueuedNotification_user_6 = (queuedNotification ((Id (fromJust (UUID.fromString "0000003a-0000-0061-0000-00540000000b")))) ((List1 (NonEmpty.fromList [fromList [("N\SI\SUBtj",Array [String "\CAN7I\DC4VmD\12237\&1"]),("n\CAN~\95556N\141826Bo\14945\SI@\ETX",Array []),("",String ""),("\182819\NUL\"/;\44765\992626\162388",Object (fromList [])),("\"*\1088218YG\DEL",Object (fromList [])),("-D\35197*\1009388\99052B;|\b1:%r#",Object (fromList [("o\EM{7",Null),("+\1085691f\ACK",Null),("_",Number (-5.0e-3))])),("\1106539Q\DEL\1013129\1027642\1000683a\EOTL\v\50514\DC1",Array []),("L6=h=b].,YH",Null)],fromList [("\NUL-",Array []),("",Array [Number (3000.0),String "\1044378"]),("\SI+\r",String "\STX(\119995 \STX"),("O\1113180\DELED",Object (fromList [("\GSy",Null),("z",Bool False),("{l",Null)])),("8\1136W\DC3:",Array [Number (0.0),Number (10.0),Number (-0.1),Bool True,String "\6359"]),(")2;\SO\CAN\1013442\a",Number (0.0))],fromList [("",Array [String "\31023\\;\1084368"]),("8m$}\DC3",Object (fromList [("\2280\1054181",String "! (N")])),("?",String "\1025718"),("+a\v(\1062871",Array [Null,Null,Number (10.0),Number (1.0),String "o",Number (1.0)])]]))))
testObject_QueuedNotification_user_7 :: QueuedNotification
testObject_QueuedNotification_user_7 = (queuedNotification ((Id (fromJust (UUID.fromString "00000005-0000-0044-0000-001f00000019")))) ((List1 (NonEmpty.fromList [fromList [("O\RSA*\1081812f\SUB\987755\ETX\1055709\1065272",Array [Null,Bool False,Null,Number (3.0e-2),Bool True])],fromList [("",Array [Bool True,Bool False,Number (0.0),Number (0.0),String ""]),("d\98638\ACK",Array [])],fromList [(" XC",Array [Bool False,String "",Null]),("9W",Object (fromList [])),("Y",String "W\49263")],fromList [("\1068267",Array [Null]),("$",Null)],fromList [("\ESC",Object (fromList []))],fromList [("\FS",Null),("@",Null),("f+\\",Array [])]]))))
testObject_QueuedNotification_user_8 :: QueuedNotification
testObject_QueuedNotification_user_8 = (queuedNotification ((Id (fromJust (UUID.fromString "0000002b-0000-0079-0000-00390000005c")))) ((List1 (NonEmpty.fromList [fromList [("!\1010895R\1027122}B\1072927\1022964;\1094453i\1039048m",Number (1.2e-8)),("\ETXk:",Number (-9.0e7)),("qAY\1111665\\3Q\aTX\SOH",Array []),("",Bool False),("\EOT~\172353\DEL",Array []),("1*^\1092663\SO\f! *\1070104\32910n\1030917",String "\ETBj\1060045Vqe}\1025333\98699r\169119F0S"),("q",Number (-4.0e-8)),("\FS6\1031799\r\166929\ENQ\1096690\DC1\DLE!",Array [String "",Number (4.0)]),("Fh?VLe\CAN\\",Number (-60.0)),("b ",Object (fromList [])),(";4aA\ACK\1102677\t!n\133968\1044392",Null),("o7a~4n\1109132\ACK\22560\GS",Array [Null,Null,Number (0.1),Number (0.1),Number (0.0),Number (0.0),Null,Null]),("\1017021L7\6474\&7A\1058823\1062304\137317i",String "\59985p\1074065\&1K <\NUL\1038756~[-"),("\DC4\"\1100670#=\42173\ACK\1033666Q\RS'\1094532\DC1",Object (fromList []))]]))))
testObject_QueuedNotification_user_9 :: QueuedNotification
testObject_QueuedNotification_user_9 = (queuedNotification ((Id (fromJust (UUID.fromString "0000004a-0000-0035-0000-002f00000042")))) ((List1 (NonEmpty.fromList [fromList [("W",Object (fromList [("g",Number (0.1)),("G",String ""),("\a",Number (-10.0)),("*",Bool False),("",Null),("\32373",Bool True),("\EOT",Null),("O",String ""),("#",Bool False),("\155751",Bool False)])),("",Object (fromList [("z",Null),("",Number (-0.1)),("\DC2",Bool True)])),("\181873\1076983B\DEL4\NUL`gd<`\171558\DEL\162250K",Array [Null,Null,String "",Bool True,Bool True,String "",String "",Number (0.0),String "",Bool False,Bool False,Bool True,String "",Number (0.0),Number (0.0),Bool False,String "",Null,Null,Bool True,Number (0.0),Null,String ""]),("\1047242xVKB.",Array [Bool True,Number (200.0),Number (-100.0),Bool True,Bool False,Number (2.0),Null]),("\3598",Array []),("\ENQ\10032\50623\t>\1023303\58367",Array [Null]),("y\EM\1045233y\172312vl\ETX\ETX,\SOr6\ESC",String "\DELD\100361Uy\ESCq\DC3\7983\&4\996496k\991809\1018951O"),("\EM;Ma\57599j\FS93>DE",Object (fromList [("",String "")])),("\STX.\1108251\1087140[B\181176\42706x",String "\reqY\148618LK)+<]G\1054409\SO"),("\fP\ACKB\ACKHQ/Gqa_",Object (fromList [("*\186956`",String "\f\EOT\1013319#"),("\999263\&6\CAN",Number (-6.0e-6))])),("\40705\1039743((+5u\NUL",Array [String "M\DC2J[Z/~"])]]))))
testObject_QueuedNotification_user_10 :: QueuedNotification
testObject_QueuedNotification_user_10 = (queuedNotification ((Id (fromJust (UUID.fromString "00000064-0000-0020-0000-00050000007a")))) ((List1 (NonEmpty.fromList [fromList [("\99154\US\1111081\838]\72228P\15049",Object (fromList [("\28698l2",Null)])),("",Bool False),("\EM\1098209\12807\24236\1088397Bg\NAK\SOJv\\l4",Object (fromList [("",Bool True),("~",Bool True),("\160506",Null),("\STX",String ""),("E",Null),("\DC3",String ""),("\1091463",String "\ETB"),("F",Bool False)])),("\1059332S\SOH\990022yyIi_\ENQ",Object (fromList [("",String ""),("\bh\SOH",Bool True),("H",Bool True)])),("!-|\bx\ESC\DEL\1060703\STX\22032",String "\1108850'\r.=#\"J\ETX/"),("'\1086933\161416\RS:9\1097830\171069*7\US9\ETB\155639\1043543",Object (fromList [("\FS",Bool False),("\36925",Bool True),("m",Null),("M",Null),("",Number (0.0)),("%",Number (0.0)),("\991703",Bool False),("Y",Null)])),("I\169527\154247",Object (fromList [("\64132\&2\f.",Null),("\DC2",Number (0.0))]))]]))))
testObject_QueuedNotification_user_11 :: QueuedNotification
testObject_QueuedNotification_user_11 = (queuedNotification ((Id (fromJust (UUID.fromString "00000005-0000-0025-0000-006b00000003")))) ((List1 (NonEmpty.fromList [fromList [("G\24251\1097429\DC22f\ACK",Number (8.0e-12))]]))))
testObject_QueuedNotification_user_12 :: QueuedNotification
testObject_QueuedNotification_user_12 = (queuedNotification ((Id (fromJust (UUID.fromString "00000060-0000-0033-0000-007900000026")))) ((List1 (NonEmpty.fromList [fromList [("ZkL\NUL)5\DC3\1034007\49382",Array [Number (-2.0e-2)]),("\DLE\ETX/:\1060368\1072021o\"i",Object (fromList [("\noQFU\a_\2438uL",Null)])),("QS@?\10571\22493\t",Object (fromList [("\DLE",String ""),("",Bool False)])),("\n\184563\120649\&4L\t",Array [String "\178011\\",Bool False,String "\1041328\1109683"])],fromList [("\SI]",Null),("\1001788S\rj\1017900\DEL\1067129g/\998894H|\996831\GS",Null),("\1112000e\DC4,\146798+$\154660\1081007",Array [String "",Number (0.0),Bool False,String "",Number (0.0),Number (-10.0),Null,Number (1.0),Bool False])]]))))
testObject_QueuedNotification_user_13 :: QueuedNotification
testObject_QueuedNotification_user_13 = (queuedNotification ((Id (fromJust (UUID.fromString "00000018-0000-0027-0000-004100000004")))) ((List1 (NonEmpty.fromList [fromList [("~l\34076\990831\&4(\f\3420\&1",Array [Bool True,Null,Number (-1000.0),Null,Null]),("\1048470\r\b\1066658;,Co\21200\1003466",Array []),("J\1058025\1002073",Array []),("\134772\f:\ETB\1059204,N_\fh7",Bool True),("c\f\59105y\2550\DLE@\DLE[g\74250f",Object (fromList [])),("`KF\1106687%\"\42341cJ\SUB1\1062694\v\171068",Object (fromList [])),("\EOT\DC4",Object (fromList [("L\EM\1025750\rVqj\156726\1071644H\DLE6k:u",Number (5.0e14))])),("uHrhFF\29427\f$\142695:fI",Object (fromList [])),("\CANMHHZ6 \1063020(\n",Array [Bool False,Bool False,Null,Null,Null,Number (1.0),Null,Number (-1.0),Number (-10.0),Bool True,String "",Bool False,Null,Null,Bool False]),("Hf",String "\1023795L0gD\74515\170733](\164163]"),("5a\95674r\998274}\NUL\FSc\1102192xw[4",String "3ma3 y\ESC\170914Izm>\69946"),("8L\1033334\SOH5\r0",Bool False),("\1084021XqJp\189691\&7%\1071443E\1012351",Array [Null,Number (1.0e-4)])],fromList [(",",Number (0.0))],fromList [],fromList [("\t",Number (-10.0))],fromList [("Z",Object (fromList []))],fromList [],fromList [],fromList [("\SI",Array [])],fromList [("S",Array [])],fromList [],fromList [("",Object (fromList []))],fromList [],fromList [],fromList [],fromList []]))))
testObject_QueuedNotification_user_14 :: QueuedNotification
testObject_QueuedNotification_user_14 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005a-0000-0022-0000-002900000018")))) ((List1 (NonEmpty.fromList [fromList [("\1001776A",Object (fromList [("\194607*p\145328f\n",String "]9((\1036155V"),("4\NULqH4",String ")\"\11438\&3\DC1a")])),("\ETXVv\1095464\\\aSmE",String "q\33337")],fromList [("%\142425\1042762\CAN\fUiL\ac\59478\33835:",Bool True),("\38517\31215cJ`{r3\20724z\1106078",Array []),("\156155\1033059>cU&\US\1088698",Object (fromList [])),("~Nbb-|\48266\1064166\189865\1053046pf",Object (fromList [("\DEL\v",Number (-1.3e9))])),("$h~~\FS\1089749S\ETB",Array [Null,Null,Bool True,Null,Bool False,Number (-0.1)]),("uR\1070769\NAK\37821D\83488\14316s",Number (800.0)),("\1110808}\DC30\ACKm\t\\F\1035772H",Array [String "\ACK",Number (-10.0),Number (-1.0),String "Y",Number (0.0),Null,Null,Null,Null,String "\673"]),(".`\EM\SIuf\DEL\DLE\EM",Object (fromList [("\GSX",Null),("_",Null),(" \EM].\DC1",String "N\1017203=")])),("\t",Object (fromList [("\SUB",Number (1.0)),("",Number (0.1)),("\ESC",Bool False),("\SO",Bool True),("\44635",Number (10.0)),("h",Bool False),("\b",Number (-10.0)),("\1065612",String "\\"),("\t",Bool False)]))]]))))
testObject_QueuedNotification_user_15 :: QueuedNotification
testObject_QueuedNotification_user_15 = (queuedNotification ((Id (fromJust (UUID.fromString "00000031-0000-0054-0000-002c0000007e")))) ((List1 (NonEmpty.fromList [fromList [("\EOT\1023523\rF:PkY7",Bool False)]]))))
testObject_QueuedNotification_user_16 :: QueuedNotification
testObject_QueuedNotification_user_16 = (queuedNotification ((Id (fromJust (UUID.fromString "0000007e-0000-0007-0000-001e00000011")))) ((List1 (NonEmpty.fromList [fromList []]))))
testObject_QueuedNotification_user_17 :: QueuedNotification
testObject_QueuedNotification_user_17 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005e-0000-005c-0000-00650000002e")))) ((List1 (NonEmpty.fromList [fromList [("|\ESC\DC2\1056633nA\1019136qS1;2A\1099073",Number (1200.0)),("\1066084O\1090037\1065418\&6\1016132r",Object (fromList [("b\SOK\63790\FSM",Null),("",Number (-4.0))])),("\41375%",Array [String "`",String "",Number (-1000.0),Null,Bool True]),("\17465H)?*\a\a@\n\1097357\SYN\FS",Number (-90.0)),("}f",Null)],fromList [("",Number (0.0))],fromList [("\1001963",Object (fromList [("",Bool True)]))],fromList [],fromList [],fromList [("",Object (fromList []))],fromList [("",Array [])],fromList [],fromList [("\GS",Null)],fromList [("A",Array [Null,Number (0.0),Bool False,String "",Null])],fromList [("",String "Q")],fromList [("",Object (fromList []))]]))))
testObject_QueuedNotification_user_18 :: QueuedNotification
testObject_QueuedNotification_user_18 = (queuedNotification ((Id (fromJust (UUID.fromString "00000063-0000-003d-0000-00230000000f")))) ((List1 (NonEmpty.fromList [fromList [("\ETB\ENQ",Object (fromList [("\STX\8633\&7",Bool False),("\149724",Number (-2.0e-2)),("",Number (-3.0e-3)),("?3",Bool True)])),("P\ETB\fj-M\USm\FS\ETB`\v\14673J\SOH",Object (fromList [])),(";\DLE7Q\1032286\DC1K%;r\1006912\1097305\40378",Bool True)]]))))
testObject_QueuedNotification_user_19 :: QueuedNotification
testObject_QueuedNotification_user_19 = (queuedNotification ((Id (fromJust (UUID.fromString "00000034-0000-005d-0000-000700000006")))) ((List1 (NonEmpty.fromList [fromList [("\\\160086N\170629q\18927\n\"",Array [Null]),("v\1079833\DC4\1058892\&3E30u\8984\EM",Number (-7.0e12)),("\nLN\ETX\a3\FS\ETBu\\\184344N\ETB$",Bool False),("3]\b\23300Le-\155270-{\1062823",String "d\111078X"),("!\147835\STX,",Array [])],fromList [("M",Array [Null,Null]),("[m",Array []),("U",Bool True)],fromList [("=t",Object (fromList [("",Bool False)]))],fromList [],fromList [],fromList [("\SOH%",Object (fromList [("\DC4",Bool False)])),("\nlf",Bool True),("\SYN",Array [Bool False,Bool True])]]))))
testObject_QueuedNotification_user_20 :: QueuedNotification
testObject_QueuedNotification_user_20 = (queuedNotification ((Id (fromJust (UUID.fromString "00000024-0000-0018-0000-004f00000018")))) ((List1 (NonEmpty.fromList [fromList [("gSXV_=\b\1110207p\59167",Number (-3.0e14)),("%\74238\&2 |2\DC2\66359I-",Array [String "\NAK\SO",String "\1109710\1004196"]),("\ENQxyI\t\SYN",Object (fromList [("",Bool True),("z\1029002j8\\i",Bool False)])),("\985866",Object (fromList [("L",String "p"),("Z",String "\42737"),("",Bool False),("\97563",Number (-0.1)),("i",Number (0.0))])),("\ENQ.Qw\f\ESCk)",Array [Null]),("0e\EMv/\167350\US\148093",Null),("y0",Array []),("\NUL\166111",Array []),("\ESC\1084371WXI",String "\152935\SI\49328"),("k7z\1017762i\\A\1093687",Bool True),("-\97658'}\1060813\11594\RS\CANw\DC1\FS",Array []),("iG\EOT^!Z\ENQ.5\1080805nFX\GS",Object (fromList [("x\STX\SUB",String "rcQ"),("\1101419=u",String "5K"),("\DELr",Number (2.0e-2)),("H",Null),("\EM",String "d^")])),("Kc#",String "^\US\ESCJ\nD\FSFVa{\1035312T\US\v")],fromList [("\SO\NAK7\ETXk\DEL",Number (4.0e-2)),("\150423\155618\CAN\US1Q-",Null),("g\NUL\57958W\1082057\ETX",String "\176325\1097147\1103700\EOT"),("BYxd_\ENQ",String "-SW"),("\10158x\SYN",Number (6.0e-7))],fromList [("&v@<",Array [Number (-1000.0),Bool True]),("\14405R",Array [Number (-4.0e-3)]),("T",Object (fromList [("G",String ""),("",Bool False),("\v",Null),("1",Bool False)]))]]))))
