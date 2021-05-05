{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Login_user where

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
testObject_Login_1 :: Login
testObject_Login_1 = SmsLogin (Phone {fromPhone = "+5383556278"}) (LoginCode {fromLoginCode = "\83332W\5303AsW\DLE{\"\f"}) Nothing
testObject_Login_2 :: Login
testObject_Login_2 = PasswordLogin (LoginByHandle (Handle {fromHandle = "wa7jg5qd"})) (PlainTextPassword "\GSn:+0F{\65916\SUBS\\\1091733\&3P\r\986723q\998231\1057257\138902>bS#\n|\t\DC2+\NAK\1085864\SOH\1085064\&1>\1080034N8F\r\40424{3\49887\EOT)\NUL=zt\1096429VxG\DLE!\SOHgU\1108380\DC3R20J\1065362\998042T<\1065790R\1073382\ENQ'*\1072979W|\t\vp2\989778\b3\180279ZG\EOT~s'Z\1085629|\SUB\1103722\1070114\ACK\SOHP\147322V~\SO]\47539Up\EOTvc}d\DC1\1038519K<\31725\57390\1075151>Z\n6fm\SYN\ACK\CAN\149647\vcK\US\1048296\&9V!r\t\41331\119636\USc\r\NUL1*\ETB\146212\1105968\FS\SUB\NAK\1088923\148671\25983\172184\t<|h\NULpbj\78807\&1\1008328\"k\21867&c\tD\999803=M\RSjE!_/h\94838\21672\&7\ESC\1037298\"\f\999052+\1075270\&4C\DC1\NAK\60148wc\1013521)@8\30405:\1102676\61410]y\128397$\97180\ay\139383\143270LO.t\183878}\SUB\b\FSu\30590f.)kr\NAKs`u\SOHcL\ACK\148560\vpX\35004 sS\DC4@\\i8s%\"\1094398&\nz\DEL\138246\1100598k\991886\DC2S\DC4\DC3+B\989322,ESD\1055739\\\182501%@\EM\10366\78195\999763Q0a\1040882&\140979O1_\t\59483\a\1700\99541\NUL\147343\164758\b\DC1\1010494\ETBl\a`\ENQqi\ENQ\"\30668HQe~\143463K\1046952g+\180964#,\1045252'C\DC1\1010137V\DC4Qu\992419[\1064896\1090742\EM\RS\1098611\STX\\\ESC.<\ETXju-\181499*\ENQ\990511\DLE9\SO+P~B!*\"m<\f\SUB\188920VbJ\158534Z\CANj=]fRuQ\132583B{&\ETX\"!\162878H+oD\ESC\vN3!Hk@O~sLk\ACK\58255\63080\45602RB~N+N\148719\1063891\FSU\CAN\7525\154328\&5So\39117\FSH\"~\1000009FG\fSk5\SI\182136E]$i\SYN\1010500\1036889\ACK\EM\1098365\US\SOU\991850\"\155059\SOH&K\99685n\US~R\142349`0'\50255d\96729!\131575V") Nothing
testObject_Login_3 :: Login
testObject_Login_3 = PasswordLogin (LoginByPhone (Phone {fromPhone = "+809005848"})) (PlainTextPassword "_Aq\NUL\1005801z1+$dh\54235V\1017631_UGgR\DEL\992101\DC2\1024373Ea\DC3)\1021820<\STXR\143671\985094\18665b\1103970<ys\997730b\DC2s\ESC:\1096990\RS_\1018772PQ\SOk\GS3\1054380\38593\1110759Hn\DC3\SO\b/\GS\ACK\41964\&9}\EOT<BP\v\n\1002847\fPxuW]o9{\140765.'\1095546\73865\1015548\997532n\ESCy\144358J\SOHt\1026410x\41335\1018666\DC2\ACK>Qs-\b6.\"izW\136671]dw\177308\1021568\1005149|\15941\1026387$Dg9_D\DC2J\13273\141167FX\128371\SOH\GS\120193I\997729rN\1028608a\rQ\1045962Z\170643\&6\1100650\f\163271pG\DLEFf?\t)\1247\1044714\&2Fh\166448:Lc\38902\&64(i\1083656\1080541\NUL\DC1\1074432\59850\&7=\NULo\4591\a\128609g\DC4&5s\bn)\28590B\EOT\74219\SUBo\STX\1015307\SO\98442EUU~\997732\ENQf\1021885Uv1%0f\188769\162635(\11612)e?A\1026336K\SOH7\ACK9l\f\DC3\SOH^j@0\ACKS\1104158aTo|'P0yY\70120\&9`\63836\EMyN 2< $,K_||\68764n\3628Py8\182358f$#\169252\SYN\b7\983986hcM\RS\1027957-L\SIhqk\1089329_39IL\190474(+>\143869\ENQ6\52330;E\13603\1097206\1105585t\fAS\RSj\DC37\vm#I\144013\1015060\DC3\121329\&29\41725\&6\1054126\ENQR\1040965eng=kO4\r.\146248YFW2y{.^A\1074654X1:\68507^\146225\137198\SI\1106503z+so\SI\1005847A\13366\1109809NQ\136481-\ACK#\159520L\1089735r \33112FM\SYNj\186516x4%\\\183558\48186\DC2\47631-aB#\DC3\SI\96585WM\ESC\992079o\DLE?\SOH\DLE\169589\&5$\159960\48001\1007003z&b2%\ACKK][)6\SIK/\29878+\18551\127171\1091954u8pcgk\1088968S\NUL\31995\48058\FS|'PbTs\42848yI|n]%\47317(W!%\GS<;\DC24\SYNo\78110\ENQ\48412,\NUL\1112629zB0n\152232\"-\1000223N\NAK*\NULx\187511&8\\'R\"7\1075747\&5u\CANsp!g4\DC2]dj';?Y`\ACK\19761M\v)E\1100208yCr8-\1002428\b\DC2x\DC1O\\\ESC\10058\v\173707_wYtr\1033301\ACK}_v{\ESCp\1103257\DEL&\EOT|\1050180\1028654\ESC\1065072\&0wU8#\1023771\1092341\2823Wa&%Di\28664\1044758~\191411\DEL\DC4+\1028392\97645e\1101531\FS\33635;~\16769v\fR\1023676{M\189799U\USt.Jr}#&\ACKM\1112064\39103\STX+\ENQ]\150323\DLE\159239R,\SYN[\988860\1085236\1102002\v\1106384") Nothing
testObject_Login_4 :: Login
testObject_Login_4 = PasswordLogin (LoginByHandle (Handle {fromHandle = "o708b.ivx86ttvpdmcntqgpksy7iesh8o.8u68ps1pnuib-ws7v6iq-9upqdf6l5u3d9eg"})) (PlainTextPassword "z\DC1\SOH\45580[\59762!\FSN\r2U\1026786\v\GS_bm=Ii:\1027128\EM2L\vG<#\1059235W9\GS\1098357\1105366\ACKU5N\ENQ\SOHl\DC392i\148345\n'\SI\1062289&\1053234\&6{\ESC\DC1\ESC)\118826\78424,*\GS\ENQ\a\15969+\1030452?\b@[H\1091169\ti7\1009707j^\163138\EM\36568\70188qo\132806\1010028&:i\t\DEL#\DLE\1113467{0\1069875\ao\182785\DC3xN\1077813\&5gV\n0q\f\162482\48781\SO%ebH~\1017682\SUB\992042\144210\173020k An>((>'H\23877Pl\1064841L?wMsdCq\f\"6q\DELfN\1012372b\953\DELhN\DC3k\ETX\CANg\NAKXK/Y\\z|\RS,\988485z&\GSE\16032L\n\DC1ip.\SOH`)&W4A\CAN6\1041108\186947\a.\168596\ENQm(K\166840\EMj[D \62904m<\ENQWS,jZ~\NUL\SUB$\ACK\1062777\141205r*\a\SUB\35886<+\62002 g\993270\1011363&\165816\SYN\29467\34382A]g(Hx[T\NUL8[\NAK\1110387\CAN\167920\EM67L\21292\DC3F\b1\DC1\DEL\179263Zt\170731UO\1083169\CAN\1064832g\CAN\25959\1066297&\ACK\DC1l}Ek\DEL\NUL\1074514\n\32396\USF1\132538SA\1042797o\NUL\1042480\183757Xy\ESC0k\NAKW3K8O?CS\165102uH\50530\1102928\vDx3\1015924k\STXOq\DC24\DC2b\f.\54936'u\bX\GSgyM>%7c\ACKIk\1109049m[)q\381z\1107196\1071097#-JyI\1084205\&5\151049\47181P\NULG:ls?1\DC4+\DC4:&0-;,6Wt\r\185792Z7") Nothing
testObject_Login_5 :: Login
testObject_Login_5 = SmsLogin (Phone {fromPhone = "+9363155856"}) (LoginCode {fromLoginCode = "\511T\FS:"}) (Just (CookieLabel {cookieLabelText = "\1096273\174857t\162658=\989332\&6\23469"}))
