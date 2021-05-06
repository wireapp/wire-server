{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RmClient_user where

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
testObject_RmClient_user_1 :: RmClient
testObject_RmClient_user_1 = RmClient {rmPassword = Just (PlainTextPassword "\175821\999754\SIl\SYN\41713t\CANp\53196R\n\148428\NAK?p3eb fx\984612@.uvo\RSd\10402\&2F9\STX30=(;\DC1$\1021358\a&\1100540\40006YV\1090082ac\v\taHaU[\160199\994423\&0m\1039593\52003\&1(\170084P@v\1102224\&2h\DC3D\ESC\EOT\1036227\1108386\991837W.\157937\1094766R\83096\SOHbDV$8\1068670,\69728\17124t\1026524Z6nb\159833\121328\DLEY\CAN\nU\t\1050805e\1032449\27482(q\157050f%q\157600l\189957\CAN(\156084-\69403*6]\DC3\18335L}8aM;\n[37\DLE\1062950K\996016J\22025X'0;\GS\SUBw%5L\ETBTBYc\95895]dn-Cw\39934\1068875\1057160d\SI\1108207\1022742\SYN,\EOTp\1106780k\SUB\8451\9378Z\15784\SYN\ENQI<X\166543RH\144633.\150428|U`6\1089473\163690PEV%\1100747'\998022A\1100218\ETXn\995078J\1086565\161717\USG\ENQICXQ\1108296u[JJw7\50449^JaK4\121146\1043088c$X'\SI\SOH\CAN\ETX-8M\US\CAN\190907\DC19\US_\DEL\ax0\96077!\DC4X\98015\USd*H\1102780\172837\&6\EOT5\ENQ`\60734\NULh\DC3i\ETB\ACKZx!\\\SO\NAK\61501\984185YqaiV\139317\161800\30325\1021712~^\1081852|\ACKC:W=F\162429\165827q,PG=\134414\12805\rPK>\\LOUD\999736#1\1071569\ENQ\DC2\174714\CAN\ACK\DC4\NAKrv\1008022U\1014707]BC\1027181ZD\1019843\STXHlS26\6144i\163856\&7A&7\fU$@\SOvI4%\v\DC2\v\988994F\SYNrs\SO\fPo\173032\1027174\14717\CAN\"\SUBj")}
testObject_RmClient_user_2 :: RmClient
testObject_RmClient_user_2 = RmClient {rmPassword = Nothing}
testObject_RmClient_user_3 :: RmClient
testObject_RmClient_user_3 = RmClient {rmPassword = Just (PlainTextPassword "\DC2=\144560\1008400\NAK\n\1024986tyv)h\186511\EM\ETX-[(\31921\1000077\118971\141290\&5")}
testObject_RmClient_user_4 :: RmClient
testObject_RmClient_user_4 = RmClient {rmPassword = Just (PlainTextPassword "mF\\\10630\ENQ\16252\SYNu\991860b\ETXOp\1018096\1084376\&06\ESCZ)\998782\&7|\SYNY+U\1020120*\989596'?\1078989v\FS>>cY{cc\DEL\DELO\f\CANc?#\ETB\DC3\"=\GS\EM<9\98867\1007389,(\vvu\FS5J_\16062\1403h\r~\1073715\STX\DEL%Ha\11792\1314\FSlg O3\DC1HL\1078206\DC3m\ETB\NUL\134599iv\ENQ>\GS\1093216\&3G\EOT\FS\156269';u2\ENQk<<\993683\1008{\1015174F\b\52754C\SO\NULm\1005587bb,P\f\1028910*.\1026553\139693\v\ESC\1068968\DC3w\35163\1015218<\a\ESC'$f@Fg\31178\1019730\995743\40732Hgo\185723*\156581[>/\DC2eyE\46918\NUL4\1084635\SO;d`$\SYN\v\RSH7\1054598\187124\1054384#D9\100156\165190^\1041719\188593\137070r\24625\1023283\STXS\1087521r<\SOH\vv\1070327Ev\166217%f\DC4ur`(\120215SDE\93057J\ACK `U\ENQWyU7H\1074102\&8{\a\vtD\1097134=?}\f\27019&\DLE\NAK\"s\DC3p\1062235xT\DC1\1048857\187206A-\1012679\SOj!Nyvg\DEL\t\DLE\SOU\1012929\&0\149611\DLE(P*#\1074882 XM^\24384G7R\SI\27291&P/\173461f\ENQ\166970'T\138173S\DC3K\1098163K\US\DC1`$\1107738zt\1060174\EM\993924c\83432\&7@\"\ETXtF\SO")}
testObject_RmClient_user_5 :: RmClient
testObject_RmClient_user_5 = RmClient {rmPassword = Just (PlainTextPassword "\150031a\983499\1098008V\ngJm0\GS\NUL92\1056142W \94104qH\1089987Lw1\GS\GS\CAN\1033404\1100070Q#6\1108680\NUL3\1109405\\V\SUB={\EOT\1111976\1107096+ZZ\SOHrx\1054923\1065656UgK\1076043\46048\ENQ\134455\&6#l'\b+\EM \1046872\1050089\&8w\171715\1082091*s\400\NAK\NUL\ESC\FS6r*j\ETB\54897\182321jw6a\125129[\f]\FS \183177V\1073854p\fIjz\DC1]ii4\DC4\SI\RS\1057646\61072}W:\DC1tB[!\1075989\1047962\nI\1090825\153401_\1053719':\EOT/\SYN@\\\175396(0\ENQ\f\DC1\SOHrY|%\FS\DC1I*\US>!o</\187487\EM;\DC1\18565\&9>\DLExE\FSn'Y\STX\1093596T']_\48557.2i\1055065p\1109239:\17846>\a\RS 6/}\NAK\NUL\36409pQtJ7gz-\"Y\988395H&H\1080810j \US\SIJW#,\\\1033855^\64052\EM\EM{\1048940u]ysfp\1002879*MM+^\1094829N\194678\GS\STX&3]#79\1053965\SI\1059092u\FS:\13058\52575\183184\160552$\NULx\1039398<\1096763\NULU\27587#\SInk\159142V\1025117]\9985$PR\EM)xH\f\t]\CAN)U:\GS\r\1038426\&4#I\100771\&8XXd\25849W\18205\&2K\100397$x\174715\1058388\1066460\ESC#7W\165198`\143758@\1108471\n-\185756\DELmEL\ETX\ACK\999668]\ESC]L\29709\1078704\133730\1095746\\i\SYN\EOTy\993152S\FS|W\674\SYN\RS^o\1113005\&8\1112084N\FSk+#c`\1054515\119160)#KH\140719\NAK\1041720,%g\tS\1107894\f\186602\1042972z\149525M\FS\1105588\181704a\1035604\DLE\11110C\t\SOo\70834\33037\ETXbsu6\CAN\STX\EM!\70476`w\DC1\a\1043081 \thYa#47=\1071122CK\b\1113950\1045890Y\151614\n\SOra\1023073C\rZ($ot\1089876Us#O\\IY\8797pG\1098431\DC1\f\\\61909+\68130%pi# \1079097\r_vP\1043990\63823\ACK\143020V\EOTs(\1013730\SO\1019838,}PV\rDQs%\EM y\tdrrN|\SOB\ESC>\DEL#yHl\ACK3G\58599x\1008834W\1099043\ENQ\1065523p[_\41791\69217\15288\&9[~\FSD\1097043L.\n|Q\94681\f\fB\ESC'<\175040\"\19111j\1065334Pc\v\ETB*FW\62827T!\b\1103125\ESC\SI\EMm\USlz\1080317\EM\ETX\GSa'/Z}y+0:\bN\\\1103101#.K\SUB\96998d\1058410\60054S=\1050754\ENQ\83438r{\SOH(\DC1-O2\CAN\CANOD\rMT6\SOW\29625\RS\SYN\170112`\GS\t\ENQ\1035047O5k]\ETX>}\44165\":N\60168xoj\97036\SOH\188115\RS\SI\149985QQ\62560\EOTom2Sg\1033374\FS\SO1\DC28kb<&@\180593>\179269}]D'\1044001oE\b\GS\ACKm\5201\SO]0\SO\GS\1064500\134623\1046016\27531:\31758\1085162\1058911([\ETXN\FS\DC2a\SOH)U\162277\35350U4f>\bL\CAN\v\177880\NUL&@\157496[<\ESCft'F\DC3E5\148266\1096628\1032571m\r\SYN\ETXO\1057388\&9\1094484`X?p|E'\EM\1068989\CAN\187321\1108829\DC3\ENQ\1073877\r-;~\166121S{\66249\ETB\SO\163678-\"\131607\98242A\1011383*8A|\32546S\CANL5\149598`pj\"{\46364\SYN%M\ETX\989839(r:\STXHv\DC47[B\138737-I7\1100201\1094169G\b\1092961Q/$0\1053151\997514\&6\ACK\136883&B\1098616MV\US\187767XS;\132056_4v.(lq7||?\b\1006091\\\163306\1086715\ENQ\53154\1018950}\ETBE\RSS\SO4\NAK\"g\DELWQ\DLE\61893\GS\FS'w\168008\r\1064197vx.%\DC3\98494a\137539[E\"*87q\1006148.E\165742$\1075439\61943\138718J6p2\166084H)S\1001224\1006822\ETB\1061362, +#R\162174\54360%8\DC1\988548$\168006\128928vD2\DELt\fr\1020825_00|\RS4u{\59174\1026874b\1066134")}
