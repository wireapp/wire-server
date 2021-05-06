{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Message_user where

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
testObject_Message_user_1 :: Message
testObject_Message_user_1 = Message {messageText = "tX:D\32343!rl,&K]v\SI#T]i\a\r;8w\b\STX\1060604i\DC23\DC4QqO~\1044097R\\\1106880{aV9\58813}\188390N\5489\1048062q"}
testObject_Message_user_2 :: Message
testObject_Message_user_2 = Message {messageText = "Hd\EOT\nj\ESC\ax\"(\GS\STXzlQyz\182643Y-&\48764\32765G=\154424\167853\FS\n\US\120547\1020395kH\43590\101097d\SI\159893\DC2wrh\ACK0E\94326v6`\r@\USCs\1013597\ETB+\vR&\1072674*+(>,\a{a\1009158wy\r\t\t\ENQLb\FS\1040301MA#x\35833\1078053^p@\EOT\DLE\vod\1102711\&2\GS\NAK\DC4\1020024R\1051693\ETB\b\EOTMV\NUL{\1076566i.mw\\N\26624Qv|\\an\187671c\1017612\20927\9584"}
testObject_Message_user_3 :: Message
testObject_Message_user_3 = Message {messageText = "~v\ar"}
testObject_Message_user_4 :: Message
testObject_Message_user_4 = Message {messageText = "%E5nL\SUB\54341"}
testObject_Message_user_5 :: Message
testObject_Message_user_5 = Message {messageText = "!T\ni\ETX\135469V\DC12\1019279[]\29414Ym\n\1101810w\a1\a4\EOT\NULR-or\1085497\&3\GS<k1:}oH\STX"}
testObject_Message_user_6 :: Message
testObject_Message_user_6 = Message {messageText = ":\991486$\DC1G)w\182535w\1011227\1068812Sq\1036680h\DC1WHmbG6I!)\r\8869\SYN\1073703M\1108787P\95708}+u\138627\US_\GS#\1077912\ETB\fd\EM\1045272{x\SYN\1022720h[\CANi*\DC2\GS\ETX\29417\1031631~*&k-\ETB\1026684\54794\154840b\1028#\1019255\1068686\984669r"}
testObject_Message_user_7 :: Message
testObject_Message_user_7 = Message {messageText = "|@8G\1057314\&57\vn\1053484 \1020619E\185307jK:\b\DC2%2R\120024+o@\\o\b\1022817!\94830jn\1004957\CAN\DC4+V![=\SUB*H\DC3*p^7\187995\33723WQ\DC1P\51609\NAK\1084960w\DC3F\1056458\35106sWzZ"}
testObject_Message_user_8 :: Message
testObject_Message_user_8 = Message {messageText = "\ETX\35846K\1009663X8\US^\1015757]\ESCW]\110974~\63165\110765K+<KIjk}<rH_\1015788L\ETX\153006s\NUL@\1058383K\1036413\6120\136593\189053\60623\SUB\t(\DC4\f\161109\GS>\83231k\FS\1079613\tX\1014216Q6\NUL9\ETBZw>- @0\136243\1100852/\1002815:M\995703m\"\132718=\DC1\EM\ENQa\f!J5\22416F}\154258F\ETB3FQ6}?9"}
testObject_Message_user_9 :: Message
testObject_Message_user_9 = Message {messageText = "bMV\145831R\DC1\1106359\"\13670\RS~b8\1073910<"}
testObject_Message_user_10 :: Message
testObject_Message_user_10 = Message {messageText = "\182677-A4r\138266,Eew5y\57677#\986961:\1065868\SI\DC3\SUB\1062664\160634\a\DC1,\1085713\1046332\EM\NUL"}
testObject_Message_user_11 :: Message
testObject_Message_user_11 = Message {messageText = "$6+\ETBD\1003118^y\DELv\SO\1011004\159198\1016597YZ\1000834G}\1001335\&7g\SOHr\DC23\74057/v\1039708\984250u?0I X\RS\t\21406\150149\nA\1110497\1054688|"}
testObject_Message_user_12 :: Message
testObject_Message_user_12 = Message {messageText = "R\64511tY'N!g\1096365\10952O\GS\138600\NUL\FS\SI'\RS \989924d\SYN%nC.\US\59055;\1001295?}O\1107615\1074602Y\163799\SUB6\CANaYj\1094560Hgn\NAK\137523\49513lv\100426\fu%4a\1097854\1100376\SYNA\132239)WYR\DEL<\63005m\47618\1087535\NAK$\r\US\v-\1068037>\EOTz7^\1015841C\1003737(\992000\52766\aG\2150])\1022991G\DEL\1002658\43525PO\74625cS\1081022\1080817\b=\100529\10190}C\RS\72871\STX\SYN(cn=\23725\141476d1C\1077815a^4\25093\1001904#\FS|\CANxM2vH6\142131C@\1018953\EOTd\1081394 I\ENQ/@(jJ\DC1\63437\DLEJ^x\1077399\171003*M\SOH\1062083\f\r\DC1A\\\157161\1077223\186590\53988\1063659Yp\190563\1105417\ETB$P\RSx\17307\&2\SUB\ETB\ESCH5\180298\998789qg=^\195004\144815"}
testObject_Message_user_13 :: Message
testObject_Message_user_13 = Message {messageText = ")-\154935g\994641#~\b\r\US\ESC\52538'\STX4-u(IeB9\DC4\SOL\1019983BKmBp\1014924\47744\&0\ESCO\1097429\&3\20721V\28719=9\27121\1010482s\SOH\SUBF\143180\&7v\SYN\1078614\1029999\133470LDL\1000971J\1082460XZ=\b$\1022978\&4SE\DC3\v\ACKG6e*\DC2\\\SI=&9\154794\1024255\nas\1036250F\9218\96971N*\1104228'|\697\73780Yjq}c#\ENQ\US\t\1034502\\\ESC\STX0O]\b5\aF\ETB\1040242\1099865gr\52389\RS\1057930YS\1027577r\997012\n\92404\US\135021F\95754\&2-x\1089934\1004037S\177718\1104701\170871H\r\1069226$\DC4H\ETB"}
testObject_Message_user_14 :: Message
testObject_Message_user_14 = Message {messageText = "|J\1030984\DELK|\ACK@R\NULQ+\194730_X{CaDt\1042513\1095122e\nf{X\SOq3RF\1061128\aX\3311g5\EOTgaZ\RS\DELfv7\\p\1057535b2D\1048689\1064437\1034496Mp\43330\DEL\1003867\GS\FSd\SOH-\20513\1011774\157817{n:Q@Vi\NUL\53312\1099426\7328 w]K\EOT\995942\155825\DC39\1002309\1112337\1004924\125128ZFX\1106424a\189469\SUB@W\a\aq6\EOT\ETX@\1046746\1029865\v0\10194nv@\EM7B\13405\1096887L\r\99353x\FS{A`KP\1029131\&0Gx\168678\96282\8220\DC17\18420\ETX\1090809tIs\ENQP\tvM.,Kc\96722\DEL\101095O*\DC3\154559\&5"}
testObject_Message_user_15 :: Message
testObject_Message_user_15 = Message {messageText = "\34898i\986043-t_V\DLEr \DLE\RS5\15535\RS<IN\991738\57895\a\a;\150030-\"<\1043964\140756\66779\10550\v\t?\1027860FUD\158992>DS`\STX\1090124\DC2q\"\1112807\1111008=y.N\145057\ESCV<E\14101\b]z\DC2\SIf_\RSb\"\43397\FSv\1114069\f\54945g\1051376dW\120632[\"p\USn\ENQ\ENQ\EM\1033435$\US7\t\DC2!O\1010149d\110680I\\D;\t5c\ETBA$e!\RSK\ACK \34868\RS\1079598B\1015587m\8357)\1094675\t\994323*\1081096\DC3\29670\95860dZ\DC1\SOHa\16564|>g\2538\&0\"\EOT:\1058390\1019380p\DC1Z\t\DC3ds\1043276\984989B\6569u\153697\vj\ENQ){\GSr3%\39944l8\DLE\NUL`u\1028297(O\1039917\141297\r\1025800\&5\178899\f\48561-$\83309,\132276\174328\50724\&4=FcM\1065543\DLE<\DC3%\1090800\68354k<K9/'\2811\165482\137612H\SYN]\152071n\"\EOT-j5kP"}
testObject_Message_user_16 :: Message
testObject_Message_user_16 = Message {messageText = "\1062112\FSA\n\187355\1006038\EMT\1000654\tpL\ENQ\30538\162151D\ESCJtr6\29654?->\92481\NAK\NAK\134741\134577f4f&{p\STX\SIR*)\SO\US\NUL\SYN-G)\148362\1094874B\t/S#\NAK\30272\63280\1068785\DC4x:\ETX.*W\SYNJX\ESC\"[4VnrQ\43149"}
testObject_Message_user_17 :: Message
testObject_Message_user_17 = Message {messageText = ".w\FS\EM\DC4\SO~\1015875\161385k\158353\ETXJucKZP>\26199\SOH\1025167\b\b\\\187774Z\US4ar\187086\1031553\&5.\ACKI^T\188773c'~\NUL`\1092305@\1029547\ETX\ETB^h\DC2d\b\ENQ\1061976L\180825?\1006721\59895\bFP\t~\989807i\51675:\SOH\1037173XT]@\173284g\fnbm'5\EM*\FS\1009531\STXSR\22409\60341\DC1\RSGo\EOT\177387!*>E\146371\1109235\DC2\161708*\SO65qavD\SUB\158937\63903\ETXIcco/Kb\147178\1060186\160450\DEL\DEL\DC2u\USv\ETB\1015204\byJ2\DEL\SUB=2YC\58464\1038288\1085469Uw\993211\149301\67810\&1p!k1\171660\1021618\1070577=\1096954*\5503\GSg\SYN\37302\ETB\v\SOf\146613|\1800\52584,LsS]9t$U(\1056529;^\ETXS\38830\23823E\DC2\137538*E)#\1030653n\SUB\FSC\1003791-jY}\1002905["}
testObject_Message_user_18 :: Message
testObject_Message_user_18 = Message {messageText = "\185595\23393\1054636\1055131z\DLE5\1081384A\935\US\37344w$9\DC3h\161432\1095779\\)\\L\170464k\1109233\RS\1054147\RS\a\12007\\\146517<Bkx\21980sc\EOT\SO\CAN*aHy\154493\62478Ld{(\\\\]\DLE\1046956\DC1_g\5777Qse\ab}.OT\188941;\NAK\989130?)\43222P\30223Aoa\187456=\1010943\23071\120106aph>\1020592\&7zW8\GS\n\140998Nr~\1081710"}
testObject_Message_user_19 :: Message
testObject_Message_user_19 = Message {messageText = "w=\SICt\996555{\fM\EM\98726\CAN,9\1086901-\1106388"}
testObject_Message_user_20 :: Message
testObject_Message_user_20 = Message {messageText = "\991585\1019308\147197\ENQ\tWs7d9;G\1095248OS\EOTio\ETX\4449\FSy\186467\&4X\179164\30856-`l\1108285F\EOT\58123\CAN\EOT\1076327\32624\184168\bN\1112604:Q\DC3\1040626\DLEM\ETBS\97670_\1106410\RSfZK?lHy\1010817PH\1086503/\1081969\US`\28492lnC\141044\&0\1001682I\1084614\v,|;Z\1046678nF[\177382Y\138535\176591,)A\58165\142778\1072924\DC1E\ACK]\1008655'd\DC3}A\95580GhC4\1108308+\125195@\CAN\GS\1095718\&9oh\SOk\1100342@5:\t.\1049928\ETB7g\97102R\1080149\&4+^\153628\ETX\38648O\993497r\"8\SUB\1023444.\1078068H\EOT\ETB\a*\v!F]v\DLEAHi\989067\1076798\SI\US\b\170947;,\GS\1065660\144959\US ,~!0\999080YV0\EOTNCmC8\EOT!\a\NUL\1106585\NULn}\\\EOT\1096129\SYNg\1040456Tc\n\99543H\1077302\1110150\121314]\65034\&8\SOH\154557\DC2\STXe\183215\16739-D\1031922\1024939?(w\DEL\1074817\1008041\ACK\1005987\1051005\t\1050383\DC4N>cJfIB\n$"}
