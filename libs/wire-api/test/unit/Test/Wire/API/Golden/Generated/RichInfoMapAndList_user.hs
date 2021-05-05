{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfoMapAndList_user where

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
testObject_RichInfoMapAndList_1 :: RichInfoMapAndList
testObject_RichInfoMapAndList_1 = RichInfoMapAndList {richInfoMap = fromList [("\n\SI~9n\f\EM\ESC5FO\1008550FVxF@dl\SI?B\1051232;f\190904","\1004656\134928<mj|\194755\DC2Eb\984008\18810B\1130\RSo\1049696Q\1109923\996022F\1005243H\1173Ms\EM5"),("\FS\68066\&2\SO\RS\DC14\SIm\EMf\\W!]\57564\96669WK","\1103682\190963\t\f6r\191157\1037031B&\1113864U"),("\RS,U[\1079564>+\EOT\23153uB)\988076!\FS][\r\26710\STXg5\n\ENQ\1065595@\NUL.","`w}\DLE&\EOTw\53397\33433.my<\48688OCM1z"),("%","\1043568$\1026435}\58520\"\DC4K2\133876*\SYN\1038024*!\1045155\SOH!\1051204\&1\1079378)\a\SO\169204rm\182534"),("M\176708\DC3\1068029O\1100195\31250\ENQ\SOH5;Uw}~\DLE4\1030971\&6u\ACK|aN\EMo\52959","-\148495F\1086928dn,"),("{\185292@\n\1097126\vAbyjpQ'\998603\142237","\46818B4%\t\37465\v4\175479jr\174034G\SUBo\SO\26800\1086657\38735\USX\151899n\917896O=!R\131769f"),("\DEL\EM`\FSoF\CAN\191135}\1005477!\120709m\GS\CAN\DLE-\33247p)c\ETX\1111591\151336\1101582&","\\7\1095036\1098254\&7\1051095\35691\"\999039U-m#M^\RS\996364E*.Cr\24837-\97405\DC1"),("\DEL(0'3\ACK1F;\1061774$\CAN\nx\183376JE-\ETB_\DC2","s$5ERp%\1023940\RSR@C\1077134\NAK\66906k3\aw1\RStH\1051518\DC4r\1071235w\1000578"),("\17359e\ENQ\22320\SUB\DC3","\140486^G"),("\141030\1062346z\1026401\52375\bi\1039815\FS","y9"),("\1074337U1s\STXq1\r\ESCA\134205\1093970\78134U\ACK\EMkyEcrj\98877%","\26595\&0jh&\39625>\"\1062241t\1062463M]\DC1h+\110857\119117\DC4m+\1068638k\5752Ck\151732\1006873\RS"),("\1082359","I0l~j\110714RU&\156880_\10957\1105471E")], richInfoAssocList = [RichField {richFieldType = "<4\1092926@\DC1S\996593", richFieldValue = "+\35312GFQ\f\1081580\ENQK\1032075\32004=@\DEL\1014896\1050138"},RichField {richFieldType = "f\9925\1049046s\1108084\23302\STX\1002855_5", richFieldValue = "Q<yha\"\1034026)a\1038260\1087074\1052003mN\22438\180079V\DLE1\ESCcC\SO?Z"},RichField {richFieldType = "M}\155574G\1066904\182475\987624L\143630~TG\ACK\DC2\62649U\1029452", richFieldValue = "p\94561\&6\ESC\DC3\1084434_(\2990[r\SOgz\29478"},RichField {richFieldType = "\USrW*}\1076175t}W9\131644\98042", richFieldValue = "\987430e>X\DEL_\34346\1002509Rh1/~\144710\152492L"},RichField {richFieldType = "_\36395sy\1059330i]]\1009758\&7\1053722c='", richFieldValue = "/\\Z7\SI\1047471"},RichField {richFieldType = "\NAK~4\1001646~8\bL\14464\1106222\&6\174183\1008427*j)\STX\1088840;\SOH\10374!H$", richFieldValue = "n'\aP\180910\1061974\18255>)\1046551Q\ACKX"},RichField {richFieldType = "\NUL\RS\r\19668\68433 \155408\181134\DC1l!\1054573", richFieldValue = "H\ESC\177555\US\1065838\SOG\DC2"},RichField {richFieldType = "\1017694\1008556", richFieldValue = "8\US]\"\1075138\1043973}H\CAN\ACK5B\49541YM}\b\45758r\1012193I{\1090951A'\1006705"},RichField {richFieldType = "\1097156\1091317){\50194\40423\EOT\SOHh\1003613\DLEB\ENQh\DC4", richFieldValue = "\35426\1013611i5\25660\1047224D\USD\a\NULRfF"},RichField {richFieldType = "GU'\1036662\EMN\27960\STXg\ACK\132870g\32682\RSt\1101716\&8\"O\147670\131367j\"\SYN", richFieldValue = "\NAK<y"},RichField {richFieldType = "pk\184662\r'\ACK\139079\&9\1078401\DC47/;#", richFieldValue = "F\tu\DC3I\151882\b\CANT\1015581\US#C\ETX\FSa\177323\\"},RichField {richFieldType = "\1004600", richFieldValue = ":\1086957\SUB\1111462C\171241\1036880fm&S\1092952iH\1085338!Mx"},RichField {richFieldType = "\"\20120\1108084a\118968", richFieldValue = "\987253APoK\DC3\1056886\111042\1084851\1096037\f\CANu\r\64525\1105844"},RichField {richFieldType = "f\138099", richFieldValue = "\144533^\999972\1083358\DC4\STX-\1092467\1028489\&0W`\t\DC2S`0\DC1qo"},RichField {richFieldType = "E", richFieldValue = "\v1&\DC3\n18\"\DC3\1093566\&3,-IX\1041942u"},RichField {richFieldType = "D\1086091\CAN\46441\f\1082646J\NUL;N\1024458\157968Bu[", richFieldValue = "\127110\&0\164945^\136995\r\1063107\&9y\1113451\SUB\987814uAX>"},RichField {richFieldType = "\3759\SYNNp\150032\1007749sok\NAK\DC2Q\1099628Vt^\62403\1099262Z\1006304\996056v=\DC4", richFieldValue = "\a\SOX>`d\1077809"},RichField {richFieldType = "\DELU\1092129y\CANQ1s\133041\98956U\SI\178673\987997}\1079023", richFieldValue = "\190569\8276\STX\992135M"},RichField {richFieldType = "\985693Q@\998480", richFieldValue = "\1067706Ck\1029893]ZrWa\992261\156523\1083691\EM\133885nxv7\170603"},RichField {richFieldType = "\1024620zO\DC2\NAK\RSG", richFieldValue = "\RS\144074g\SI`I\92249u\26253\&4\DLEeJ~\SI\13066!"},RichField {richFieldType = "0R\ENQ:4\179660kZy\v\DC4i\23287", richFieldValue = "H&\23297,<R.\tl"}]}
testObject_RichInfoMapAndList_2 :: RichInfoMapAndList
testObject_RichInfoMapAndList_2 = RichInfoMapAndList {richInfoMap = fromList [("","]\186362\1105470y\1011388\1052297pDW\38402s#2\ACK\83358\40193\1077141\a;\162461\rv,<\183231\DC1"),("\t\US(\1019696\SUBo\65840|a\1106521\32357\SOHYs\1049733j0w\bc&y;","%fa,%\DC1p+\1058366\1112770\1072414\1045424'\986693DD\NUL\ETB-\118992d\24843\24665N\ACK"),("\SO\&HC\NUL\GS\1041847\1030673","*\1050854*\1106673RLD\ENQ\154887\EOT\RS"),("\CAN\1072744u\f\ETX\1039871","\SOu{-}r#\165209@\EOT"),(" `\1037740\27095x|\1093199\98643\f\GS\1104308u\GS\1996[\v","poW\v\1018163b#|\35517\b:\8627z.wcjE)\23091s/\1025129"),("\"\ENQM375\ETB\1025884\28246J/G","\1100081\120978\986055\NAK\EM\SYN-G\ACK9\1100140\1107542<\150330\44406\DC23\100922\DC4lTJ;"),("#:A\ETX\27468EI#\21258f\RSP&\160773JN&+\20255(\1056044\1055462\US\NUL\97437\&2\SO\SI","Z\1111913\EOT\r\30779\1094861zN"),("'\1108519V\FS3\1002190\1027635\\\SI\NAK\US\SI","\1091720S)VF\1025561YM\1055586T\1013342aS\n\152392fl\11375\174283\119592\r\1104560"),("/\1060326J\r\141117\1012584\186587\1096405`:\NAK?MN(","\r\1039939\16199F"),("22a@\35442\1029150|[\RS\1091765\162075@b\NAKC\DC27=C8!\SYN#\983094","N\ENQN\NAK9\42034\DLE\ETB\ETX\b)\144673?@\STXeBa=\CAN\ESCf\165538}"),(";F\DC4Tn\f\70154\171160&\1058905\1001721s\NUL\SYN\ETB_d\ETBmg\DC1\n\148927h?e","g43\142103\&9-!\168660\CAN\DLEz)\97221F0\ACK\990834guPn\t\STX\98741m\US\1007820"),(">hr\1028041\1052670\145914Obl\1024709S\173690\rq\SI9-\74909\1037754l*+\SOHK!!","\26963\ETB\162994`!}F\SO:i\1070406R\1092745\182786\FSnF\35437z"),("A\DELzem\SIU5\142386\22938\94381?@k\NAK)\1027651","}DA\44390\t\EOT\DC2\1033828\ETBF\1062198R\991773QxM1e["),("b\SISJ#\1093597~w\1064521","\1037425?MW\ACK!W\a\b&\29969\989616\1029796\&1\f\39932\1005979"),("D'\DC4!E\1027401\145252\72122\&0\GS\1092394\995042;\v\1054780\1085519\168174\RS3\SOH\146567R\1005745SAK\STX\SOH|\ACK","a\49978GJ\45608\32060\156545;\131333\52669$\ACKi\ESCIE\ESCTG\173354\EOTi\997446\v@,`"),("j\SYN\"\"7\DEL\33171c\53660\98361","\51352\rww\146585\985935d\1089290\FS)C\35429\SOHjQ\STX2=\SUBv\ESC\53032\&7f"),("m\50184\ETB;\186089\NUL|K\CANB.\100818iD\1071006\ETBB;","!\142266(?D"),("p","\1079443\1051784-\67234r@\1004480\NAK\ETX+\DC2Y\1101316 \1098552 N0c9\CAN\1112758\&4v7s\181358"),("Q\SI&\1026483hN-\ACKU \1093163+\tF\68181~\143985r%w65>;\NAK\1033920\184355?\1037735F","\1076283\DC1{\137674J~WN'GCm\n\CAN\f \47923F"),("q8PT\1110465\183255)`\14689ml{\1075548PP\186232[[ug\158455=lkt\ENQ\RSu\1072638","f9q0\f\DC4}\1111427"),("R_\1006265\1101554[).^C\ENQ\neq+","A\1028948'i\1066531\&6@(X\nrsY"),("T\1035950<&","c-1E9\119077h/E]\v\EOTL\ENQn\ETBZ\1112007\14320D-\GS\60179"),("X(\1087497{A^\999459\111311\994344\999791r\1661\DC3M\SOH\b\995830\SI6br\ACKn\45575$e)","/\33028L;y\170071Pi\110720\1067496\1049407$\EMF\1097800\1021127\1014194\162103\"\NAK\48146\SUB\145850\78106\ETX5"),("\14830\&1\1004329&\DC2/Dd\997238","7\ETB\61773\&7\SIx\154121*\1010047\1031869!=\1093914<\132257f\DEL\190536\v"),("\19786\RSa\DC34\1037652x\1023277\SOH\1106504\&0\1066874\SO\CAN~\137114\99009'<(Dg'l(\r]\46494\DC1","\r_(4^"),("\1052668>\173893I<1Wi+\1019109^\161159+\t.<\1033931\ESCC8K","z\1040589b\1031475/c\CANYG+\GS H\SI\br%\EOTs\DC2+\23703\1018766\24035\190981\&9l*\DC44")], richInfoAssocList = [RichField {richFieldType = "\r\1053741_\ENQ\100553\1095247v@ \RSO\CANgr\SI\23009clK\DC3\NUL\994904\1104473b'\SO\190862\1035718", richFieldValue = "xQ\ACK\1072453J\SOHUt%\170985t\ETB\998666\DC2\USQ\997806\38606\DC3\1028834<p)\GSb"},RichField {richFieldType = "\n\26576@?\154787\154751Q\EOT\t|Bg\ESC^\DC3'\1113968K(\1050129\1101126\DEL\183455.nB\v6\ENQ", richFieldValue = "\DC4&86R(GBZ\1008512Y'\1092201\DC4A\98755&o"},RichField {richFieldType = "7\177726\DC3A\30750J5\DLE@%", richFieldValue = "C#f\1071038ZT\987427\72325t\28078\DC3H=i\ENQyxG;\163600AJ\186061}b"},RichField {richFieldType = "\36822>c5\155707\n\172708\1038639R\1101161s[\167399zf\STX\187016d;[n\1091383\32291X\49169\65746\US\19497\DC3\999290", richFieldValue = "i_\rN\\K\1098624\GS @U"},RichField {richFieldType = "@", richFieldValue = "(pw~H\DC2C,<\1023314\&6\EOT?PC29\EOT\SOH\SOH_\DLE\SYN"},RichField {richFieldType = "", richFieldValue = "qc\15283D1SIB9\142659\2030\1090912\&0(\RS\12587\34162\19583\44584a\1010552\49968\37867BIc\183906L<\96483"},RichField {richFieldType = "Fg\ETB\40464[423\1076806", richFieldValue = "\180168\40876\169575\DC2.5\1084401\184853\180523W\ACKX\987308\f\\6\US\SO\1019272\22346\NUL\988867>:\983965"},RichField {richFieldType = "n\52455N\EM\25995v\32685:2~&\SI", richFieldValue = "8o_Y\994743\t%S\1044681N)\ETB\45456\1012232\&3O\168773E\159276\62819+!kY"},RichField {richFieldType = "Rl\37126#u\1014232 \SYNou\DC3L:\1084370f&4", richFieldValue = "\DELl\1046433E"},RichField {richFieldType = "\SYN' ", richFieldValue = "[t2v\1113469P\1049549"},RichField {richFieldType = "\EOT:Q\179385\&60\ETX\\\78257\21696t\993598Q/rL", richFieldValue = "\RS\b\1067183]\1084469\35788<S\EOT\STXR\172385\1108260%"},RichField {richFieldType = "_\43854J\174087\46097X&-\DC1Z*\131426;m]\1569\DC2y\1092032%L_\\\SO", richFieldValue = "%Rs\162210v\18047\146157e\31683\73794\&9"},RichField {richFieldType = "m+\ay\49306\18448\992888\SO\ENQ$X^]s\RSs\1023801\FS5\1100723\153442\984174\&4", richFieldValue = "&BHj\SUBa\SYNa9R\ENQ)\EM>\153441O\ETB\19106\1080396\EOT"}]}
testObject_RichInfoMapAndList_3 :: RichInfoMapAndList
testObject_RichInfoMapAndList_3 = RichInfoMapAndList {richInfoMap = fromList [("\r9\vk\SOK\10374\SYN\74591G\NUL","1\119970\fe\DC1\SOH\129596&o~T\1108192q\EMu>\DELD\CANJ#ozfU\74753\1064614\44323"),("\19502\SOH\DC21\3513\1001379-\SOH\STX","u~hb\1031077\aa/X@yjrg\1022394\1053681\US;\146786\GS.\159428\64036"),("\1068925#\1024092\7473\1045975","\DC1,\1023577\1095845\DLE[hL9\b\\\35126RaY` \"H\ETX\29779/+\64562\41838"),("\1111545\NUL\1102018:\US[p\171907\EOT\RS\SIU\SUB5W\1021250\SI","\SI\24469E\ESC#")], richInfoAssocList = [RichField {richFieldType = "\NAK\ENQlB\1032830\1028436\EMD5\\\DC4jR@\DC4\1008943\1072527\1049608", richFieldValue = "E\998549\165954\163464\999144i\1111268\STX\fb5\991672uG\1101997h=o\165235\131443"},RichField {richFieldType = "I+{\EOT(#hXX\NULwH \FS]~<\DC2o\1006738`-nD\58382\f}\STX\ETB", richFieldValue = "fO8\1023616\DC2\DC1\185201)\a"},RichField {richFieldType = "\ACK\US\ETX\1020287@CV\ENQ#\NUL\172173/\a5\996586\r4", richFieldValue = "zN\EOT\DC2tTXA\FS\CANE\97293R\GS|u"},RichField {richFieldType = "\12765\29581\DC3zR\"\\\39050\ACK\157354^\1038121\bn\ACK\DEL*\48330db\187900\162335\3875j\SUB\DLE6\46642", richFieldValue = "U\168185j,\22462\127208X\STX"},RichField {richFieldType = "\b\991062\6586\GSQ9\999399Z\1069618\&4\RS\1063711qj\1071035\1110817\ETBv\SUB\1075995CT5Q\ESC\45293", richFieldValue = "\SOH\5800\1033162\&4Fsr\v\128587uR \172651~"},RichField {richFieldType = "i\SUBu\STX$\1054929W\SI\65447\180713S\ETXANLm\28877", richFieldValue = "U+D\FSt\FS8)(`MT\f?\ETX].%]"},RichField {richFieldType = "4\ACK>C\US\DC3dI=", richFieldValue = "\GS\1055950~"},RichField {richFieldType = "s", richFieldValue = "Tp}\6320%\1030715v\DC3\1088289\1686\ETX\naB\41166mUI"},RichField {richFieldType = "r\"", richFieldValue = "VndJ:(\1110716\40806\34852p.~U\1032685\SYN>Rv\FS\"R\DC3\176715c["},RichField {richFieldType = "r\187847\27037\NULgP=d\SUB\t\US\ETXt\ESC-\DC2d\169220\1018399^\135738_O\1058593\144925\987483&\1076868", richFieldValue = "sn\SYN\17123"},RichField {richFieldType = "A\148087(T\133501#\GS\DC3UuC'\1084503,\EOT*\61807", richFieldValue = "*5\n\63126\r\1026426GY\EM>\1010349ea\989512\n\1085857\&2\139444(l"},RichField {richFieldType = "@>\1054489\DC2l\NUL\34863:H\1096452Ji\DC2\NAK_l\1113028\DC3-\"*]X\1067712\1083996", richFieldValue = "\1006777\&6\ETB\983329Jsd"},RichField {richFieldType = "fz-\124933\25078Z@ Ij\RSY\n\35122\&8\1016610/\NUL@\995612X\SOH\CAN", richFieldValue = "\FS[HD\183962"},RichField {richFieldType = ":\DEL]\RS.\SYN\24436\1099749kr\r%\54613R\DELk\SOH\187508/\2266w\44232@\158403:u(1", richFieldValue = "P\14480W\ENQ\4947A\1111134"},RichField {richFieldType = "", richFieldValue = "E@e\SO\1031213\67202b"},RichField {richFieldType = "sQX\\I\149119-\FSc:\SO#>/\44146\53933|\DC3p\1076221\DC1", richFieldValue = "\EOT[\1081335O\43872\138988u\\Jz\121103>W\SIK\1013783Q\f}:x\1104777Z\159148]7\US\135159"},RichField {richFieldType = "\985965f\135658\134135\985081]\186651\1260O~{", richFieldValue = "F}4\1050986V"},RichField {richFieldType = "R\GS\1009552\1080935\1009399QEC|\SYNe", richFieldValue = "Nd\STX`P\185351\n\1082047"},RichField {richFieldType = "\v\1090149\SI4\4128\32432!*U/", richFieldValue = "\4685\1053983\t#i\1012707\166320Ba\DC4\160039\1082737{\t\ACK.A"},RichField {richFieldType = "lT}|?\1098480@o\f\SI\STXRG$\DC3&u`\1044610\r{\EMY\STX;\6826\155020c\NUL\ACK", richFieldValue = "@55\av\1024159EEp\1003302>\DC1\DELn\STXG\994550\1054392+\1072776"},RichField {richFieldType = "TCb", richFieldValue = "\NAK\74649Xq]\DC3\9151K\1076765#\f~`y\CAN7A\139401\163354"},RichField {richFieldType = "\1061973\1081795u\GS|J\1069589!Y\1010158\\}/h\184132e\1057324\189573WT~Gf\DLE_\78898Kh&.", richFieldValue = "Rb|/Rdcmi\12203^\156810\1084278X\49128<\1067146\146136oC>\SUB"}]}
testObject_RichInfoMapAndList_4 :: RichInfoMapAndList
testObject_RichInfoMapAndList_4 = RichInfoMapAndList {richInfoMap = fromList [("\t\161687\58335u","\52284y!7U\1101766\59909t\26287\990199\vxqS7(J\78721\RS\162619\142017f"),("\DC1E{\27813\FS*KZ\1055869#\191324X","\1107564\1710|\1052247\SYN\DLE\ffR\DC3} \DLElD\1015089\&6s\186477D\157212!\f\1061507\SI,\1008543"),("\DC2\r?","\RS& Mi^F\ENQr\169907\1112002\131786\92765\SO q0(\119085\191444\1111835"),("\EM\34765C","*V"),("\RS\1027128\SI%E\tN2C\1085964r]NX","Sw\ENQ\165238\1062789\28674\EM\ACK|L\fMTd"),("\"\v<\DC2\SO:;\185810\95566\60840\DC3zV\167480f~z7`f\143750\&0\SI0\1033947\178093\1052156","P_;\fq\131470\121452\1075731$'\GSwZ\RS)\a\148534\rC="),("*\182022\ETX\1034975\74004e\NULQ|u\65568\992173\985291N\v?o\DC3yt","o+\SOj_\182147\DC1|\DC1\111132nK\b\SI-\1048914"),("8\EOT\1056855<\94351g\STX-.p","z\64115\fx~\r\57995\DC4"),(";eQ\a}\ETX\141308\&3R\172361rA&\11153\DC2\NAK3-m\1045211","O0\ETX\1047894x\1068402E\100425O\183075\\@\1067549\ESC6xu\NAKv\CAN5\1001773"),(";~h\ETB7\169928+S\1092223&E,?\51379\&5#\EM\USb\SO\ETB.7\DC3\160606i\111286i\GS","-\155493\129577\15424cnXC|\DC1ce\39046e)D\SI\DC1S\132720Jpq\1032598\CAN"),("I\1007315\1106865\54097X\1081470cq%RR","pB\1026073\DLEM\54191\STX\DC4rFp\DC1%~n\DELrh\ETB \NAKo\1106250"),("jW\NUL\73122I\62027\94240\141760i\163855\USaw","\EOT\8234#\1018479\EM \SOV\1036645\21541<\DC1\FSY\1069860\1093494\1056283)\STXQ\155664[;@_F\989750\94541"),("K\50221O:xr\fTd\42588Lj\1101953\1014435Q~\1044700\1021964]e'q","\1094954jF\CAN\1092405w\NUL)r1F2"),("SB\1113871\&5QB?)","\54189\1094464 \SOH\187940\1021474=1;U\v\170392NZ,71\43019#t\996958\1022788\ESC\ETXW\DC1\1097166>T"),("T{s\rm%\DC4\120069\1026573^(\DC4\176095\1034087\16854\v\16137c\NAK;$\1089537","\FS%#\177853Mvc~\136995~[\14930\31381k\1085880cZu\141096Z)\43982\&6\\WT\1049690\SIj"),("xnZV|:\"!c\NAK\NAKt\DEL\SO\1009871\DC35)\SUB\176066\756\1060213\986077\&7\131127N",""),("\19877\161009\b\US\1026769\f\50620x\1027456\NUL\NAK\b\SYN?ju%\179051a\186314\US\1084112h\177347[\DEL","\96586\1089673L\168245#\1110720aM\128024\RS"),("\51443",""),("\1008651Zv;z\"<>\NULR\SIfDuH\20690\NULi\1111947\1023358+X>\170157\1099695j\DC2u\tH","b"),("\1023352\33766R_uW","\166982N!"),("\1030346\NAK\13760lJ\FS\GS","\GSd"),("\1072239\&5\1014124\SI\bS\1065440\146796M:6\1055172$\100896\ACKC","\157155^\FS\987207E'n\rAsL\STX&%(\ETX\1025505\&79$a\DEL\1014444"),("\1089472\1109095\31896\69224:\1089201x}(@\NUL6\STXn\136807\1014535","I5K\a\CAN\SUBl\GSr~\160776Kr\171859\NUL}vt\22880\&6h\ACK\31358(^"),("\1090405\189187hv\62717>\4420\36308?(\1009390D\189764\DELms\118931YGt\998014\180450","q\1791_4}\n\NAK\1080514bX")], richInfoAssocList = [RichField {richFieldType = "m", richFieldValue = "\152308\1072629\EMnS\72877\1075879Z\1055048\STXi\169414"},RichField {richFieldType = "\1110659\&5aMm<'\US\74454t8\DC2\ACK\vh\3784u\CAN9N}\n+\CAN\161212f", richFieldValue = "\1190$\1035877<$t\16148\DLE\GS-\972vV"},RichField {richFieldType = "\ESCB\144000-|\99371\SYNt\1104633\CAN\DC3k\SUB\1106697\64666[l6\7292=M\1104172", richFieldValue = "C\991936ilN\1027848\182376+|\a"},RichField {richFieldType = "\STX\RS\166029w\26651\GS\1048695Z", richFieldValue = "]!^\1071480\RSm\1104290^rrh\96370\7501\ESC\984288\1011614{rb8\SYN\1048535\&9bF\170214v\GS"},RichField {richFieldType = "\RS", richFieldValue = "\1010923\DC1]\ETB\74772\1033283\EOT\ENQ\DC1\1016809\175520\&5"},RichField {richFieldType = "\ENQ\986928\1002124x\CAN", richFieldValue = "\b\144630MrPG\12055\STX;`+\45811\1001212\"N0HWc\SO\48150\GS;"},RichField {richFieldType = "x\1071177\NUL4Y\tMb(\20788E\1025032]*V\n\145815_\123178#\1105333@Bo", richFieldValue = "\DEL\ETB\136623\DEL\DC1\187214#\55173D\DELQ3\985595D\NUL\r\"\1019330\EOTd\1098835\10502\GS"},RichField {richFieldType = "", richFieldValue = "S3,"},RichField {richFieldType = "S\1100035\151529i3\155799{r\SI:#N", richFieldValue = "J\30167\n\STXx\1033251\20112\SUB\NAK\172899k\31122"},RichField {richFieldType = "~7V", richFieldValue = "\139851"},RichField {richFieldType = "\187146\147311,\157940\53381", richFieldValue = "w\NUL>~\51853\NUL\173472\147130\SYN\134409I\n\FS\1027320\1055654?\DC3Z`f{jy}\1044402\EOTt\13943\1029631"},RichField {richFieldType = "\20166|0oH\SOHJ@n \ACKQ\986678\156785R\171356", richFieldValue = "t\r"},RichField {richFieldType = "q", richFieldValue = "F=\ENQ\20884K\n_.;4m\184504\1090972\1109032[?Q"},RichField {richFieldType = "Xd4wa\GS_", richFieldValue = "\50961\1042656\&7IU\NUL\1061967y=T$ZOI\10612g"},RichField {richFieldType = ")%G]\DC3\37677DDc^\1040502\1097105\141177", richFieldValue = "\SI\STX\DLESk"},RichField {richFieldType = "uvk\25412\158060enYG\EM\181446X\SYN\1094945}\CAN\SUBAWs\154688\FS\185230sm", richFieldValue = "\ETB\ETX0_P\b\DELkuLv\1017682"},RichField {richFieldType = "-\100569Y)u\53125m\DC2o\1047230\&0\"Vj\1030555\&9", richFieldValue = "\SOH\167511\1113119\NUL\f4;j\1101839!'\1056300\1038010\165960\1028645"},RichField {richFieldType = "-0JTV\1060732\1019678\1839\"Wbx-$\SUB\DC4\v\NUL\DC3s`", richFieldValue = "\998094mNn\164786\DC1\GS\187803\ah'`\178138\166790\1109213%D\US?&\62185D\ETB\DC1"},RichField {richFieldType = "&]\1090757\156756-\26521\1090471\1064971\&6\r{", richFieldValue = "\1089476p\1000417\vO\STX\1073508\1021617\DC1\983265\995872\SO\160170p/@\EOT\1017985"},RichField {richFieldType = "=Wsn;5\1053818\SO\"\14941c\SUB\SOH\SUB", richFieldValue = "\DC3H\fY\EOT\f\SOHrw\152389\49690\34066rvQ\FS\164767\174119a\174624\59671sp\r`\1024508]5["},RichField {richFieldType = "uwF%$;n7\170381C\161180\SOF)\27182\STX8}\1094888\35950", richFieldValue = "}\160534n3\1112188d\\R\rw\132950\&7y,\126995\EM-\1002197Ns2L"}]}
testObject_RichInfoMapAndList_5 :: RichInfoMapAndList
testObject_RichInfoMapAndList_5 = RichInfoMapAndList {richInfoMap = fromList [("\CAN>\1089826:\988480\45244#S\1002340\DC4\ESCH\1012635x>\GS\120492>\DC4\f5\EM","B\1018925\"}\DEL0+*\SYN\1051710\166037\993852\1026908\SOHV\1015890\95552\&1\DC4\51790H"),("#\5184\1012648\55131j\t\1041370\vOL>/\1049050-f\1073934\1102310R-t\SOHG\aF\ESC\DC1","[MA!o\1044171 c\51945z.\STX{\32397]53\ESC#"),("&","\ACK\1666\&3n\46553\1041565J\FS5\164530\999671_z\161190G7.0\120613/\DC3\1023A\SI|\178519_"),("0\b'\DC3\7685","\1074875c;"),("12\1060131A`\47724","\"\rB1/s\142157\ENQ"),("<Z4o8i\ETXn\NUL\1088441\997052LB\fnzs\49147\1038956\1106831u\143457\1067505","\1083474Qll,\ETXn"),("\1095781_D0\t>a0\v\1103857","sse^%\17968\995691\1048703\DC1/GdcnX\158362\44192#4")], richInfoAssocList = [RichField {richFieldType = " \DC1\ENQG]\1044933\SO!|4\1044059fT\999615\CAN$)\DLE\1027859%\1000500/r\ACK\995094", richFieldValue = "Hu4\ENQ&\EM&\DLEjh\3560\987578\1000257F\44874\1050955s\SI4O\991086D\STX\136759!:\n\RS\161110"},RichField {richFieldType = "\1040116\FS\96305\&7\f\1110397m", richFieldValue = "y'\173842\168134\996895D\1082188s\184020WG\158802\USez\47924(\DC3>\57682c\66578\24591E\1088384\1543RhO\DC3"},RichField {richFieldType = "\SOH1\SI\170436]\987136\DLEE", richFieldValue = "Ec7^eX\1046278\71445(\EM0\DC4\95763\&6\NAK\1065542\a\DC4\999847n\DC4KWZjE\1102506"},RichField {richFieldType = "A6B", richFieldValue = "x)]\179257\ETB\SI\1102916\EMG_*\1030069\180776`b\1101224q\CAN\1005258/\39109\ETB\985872tyj"},RichField {richFieldType = "\FS}`G\20063F\r\1111553\n\180560\DLE\DC3=", richFieldValue = "ND9\991112\tM=\121375\28506c}[:"},RichField {richFieldType = "\170943\100093\&0\ETX+c\1098297\&0\95996Eo\ESC\DLEU\1084297w\48689dE", richFieldValue = "3);\SYN\17835UF\NUL- \SYN`\\q,\183540'E#c\1046019\1031353z\64142"},RichField {richFieldType = "b\EOTJ\65776\EM5>1\3358", richFieldValue = "\1038324T\987343\US\NULf;}4] ?\ACKq\ESC$\SO"},RichField {richFieldType = "\1057589QuNE\27158\62250$\1091136\f\SIc{B\40509\156590`8\STX\FS\1103486\988400\1027669\1050786", richFieldValue = "8\179797\v\998662R=\r\176248!n$\ESC\8493C\SOH\"]\RSa=eA\avl"},RichField {richFieldType = "\159423\188453$nH\ENQ", richFieldValue = "\167892\f"},RichField {richFieldType = "2\1099287\&5#\ETX\53023}g\DEL\SYN\139301Pd<$f", richFieldValue = "N\ENQ\\w\1084156"},RichField {richFieldType = "`m\1007522\139784GFP\SOe\vT\DLEX\995520p\1062386\1094972", richFieldValue = ">\1104742A\DLEl\r\azXz"},RichField {richFieldType = "@\\\SI\120608\1016000h\165510\1080620?\"V\1099399\1107571M1\r\137728\188688\ENQ5\STX", richFieldValue = "\51392\EM\1008259of+\1039679\&3"},RichField {richFieldType = "n5z\SI\1090427of\DC2\bD~l", richFieldValue = "\168159`"},RichField {richFieldType = "HSPu\1021284uvDApi0\1091849\1099135\tQz\1005567\DEL$X\18067\49092\&2O\1066616t\ESC", richFieldValue = "z\96709'%\58450y\189926D\DC2}\15603J3\1032110F\23932\DELq\164760\SOHE\NUL\SO\1107130\ah\1045187b"},RichField {richFieldType = ",$\1034854\1047361G}`\167146Cm?'\1096245", richFieldValue = "\137355K\r\ETBoq\1031597\&8\1076250\54680>'f\b\RS\21039\v:\ACK\36829\SYN\f~\b"},RichField {richFieldType = "o\41261\&5(\1076368\1062987\EOT\1032689\\ySS\148819", richFieldValue = "t\100841\ACKK%\ACK/\bR\ACK\1012181"},RichField {richFieldType = "?%\150053DYwtQo_\1112118D\NAK\DC1\182598\74527AL1x\984757\r\98188f\US\ETB\181921\36119", richFieldValue = "J\49452\&4Kj$|2t\134128-FSsQ\SUB]V,\175517_\DC4\SOL"},RichField {richFieldType = "\1113734fi`\CANT\SUB\1105949Ko", richFieldValue = "\11783o{xA@\rY,\1064751\ESC\CANr>*oq\997927\990727\EOT\\6\STX|:"},RichField {richFieldType = "#\1108114N\1006341A\1107378*j]", richFieldValue = "\ETBD5u%.<-6m\170049!+0T\STX\1071822\131231$"},RichField {richFieldType = "PY\1066455\45856\DC3\194724~\DELhv\ETB<(\DC4\DC4\187441\1030341$*\t4{", richFieldValue = "\bl\1036253=\SI\1053348\DLE\DC1&\140185Uq\121236@l\r9O\1089037x\r\129361:"},RichField {richFieldType = "Q5/\186390p\37055\&2#\15134\CANa,%tM\99848]", richFieldValue = "X\1018073b*\GSy\"le)!"},RichField {richFieldType = "%lop\f*\n<v '^lL\CANHM\98057\63338Z\135104\NAK\DELoq\\@", richFieldValue = "W=\ENQL'\184910\151831\140843h\CAN7PDA\r\137048D\1066561,\1079981f-nZ\1028942\996578"},RichField {richFieldType = "e", richFieldValue = "\23515-"}]}
