{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Name_user where

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
testObject_Name_user_1 :: Name
testObject_Name_user_1 = Name {fromName = "q\DC2v,\EM\ETX\bl\54665\95921\&5.P-]\1045805Q\3203P\SO\1010913%\5353jygSG\DEL\1103163\DLEpExI\134453\1020940\1046988J\1025958%yDZ?\1086354\DEL"}
testObject_Name_user_2 :: Name
testObject_Name_user_2 = Name {fromName = "\167000\1051736B%tS\CAN7=\50017!\155843\ESC\984778b\EMq\SOsC\\p]||P\ENQ\1017343\STX\1034969l\US3g>\GS\25202SY\FS\993134h<\21969\1025136\SI\50022z\DLE\61498u$Z\SOH9\ag\ETX5\11352\94097Vw\1099954\1076653<?\tFL"}
testObject_Name_user_3 :: Name
testObject_Name_user_3 = Name {fromName = "\NULd\SOH9q\28929\SUB\63075Y\1070757Z8GcO[\62700\16820-\n\DC1(\171889s?1\1050624 \RSS\59121V\v\r-\NAKaNw|\ACK@#=f\f=\n\1114069W\110979LwGE47\176492\ENQ\SYNjk"}
testObject_Name_user_4 :: Name
testObject_Name_user_4 = Name {fromName = "?\1019115\72252nI2\1062037\59049\&6Lo\1057657\SOHK\b!f\150328NO:7\"|E\STX"}
testObject_Name_user_5 :: Name
testObject_Name_user_5 = Name {fromName = "F\SUB\167638\STXu`A]IhC\159236w\SYN\STX\65386\1091959Z\ETXP_0\996532\ETBMQ\a\1052947\168598\1043573+Bl\78481o$L;7\128604u<OQ)\1022221\&5(\173167_"}
testObject_Name_user_6 :: Name
testObject_Name_user_6 = Name {fromName = "cU\1080740\1046252Y\SI\ENQ\ESC R.kf\ETB*RZ1!@V|\nvU\ACK\RSz\SOHC\SI\DEL\1061412s\NAKH\STXc\38078\SOHX\DC2\1060334\ETXa\r\SOHGZD\SO\993327I\992291l\178717s\190469\140735X\58333((5'oh\179098'\149093\985109E\53198\SUBcCh\"Ee\t/\DLE\tcb\1030225\148230\54155O\DC3Fk|D?\1044917][^\1040384\172758\DLETe\aQD\185849\tF\\q\ESC\1075774 qr\US"}
testObject_Name_user_7 :: Name
testObject_Name_user_7 = Name {fromName = "u)n\1055015I@\1088311\25437Z\1072824]\138133m\14215\NAK\163680#1\1073515-\t\SOHW\1010076JL\1111075p\t\14618W\RS0;\93847\4429.\EMFR>RJ\1388#6q0\28577K\1048602\1067960\EOT>R\1172R\1049906\1008898\170831\1096420\&6*=\SOe\a\122904A0u\1034456\DC2gu\1022012\ESC,v^\188480E\1107397\DLE[\1011947\7917\1090803\1010197\&3"}
testObject_Name_user_8 :: Name
testObject_Name_user_8 = Name {fromName = "N\FSf\CAN$L\1026104\FS\t\1106367]\ESC\SOH\58526\UShBY\1035779\DC2t\120837P%\aqr.k\1017202\1079092v}7\1079638\1088813\69437\1088299\1100004qdaOm\ETXQrJ$c\14894\1048910RZ\139859\172571\CAN*[\47353\US\149977'\DLEM>\SIj\45835\95642\FSd\1034633u\1087548-\EOT\ESC\1108054t!i/>\DLEhX\CANU\DLEv<\1080930\GSw53E\GS\1027209\&6]\1038302{Q\US]\985628s\1089092\ENQ\38745\CAN\187370\US?\1068608\ACK\49168\&1\194841\US"}
testObject_Name_user_9 :: Name
testObject_Name_user_9 = Name {fromName = "\38775&\10339\1052162\100467\65683\137616[\SI:\171455\&2?\161591dj.\53570W\SI|\1080909n\b,\12877i\1030098I\1046469\1040794\1069742J"}
testObject_Name_user_10 :: Name
testObject_Name_user_10 = Name {fromName = "d\1057635#{\1108972\DC2\78445\"\992936\&9\DC1 \ETB\69923{Elug\ENQ?\98675\&7\GS\1068428.(\99932Q\1096701\1091069\f;>!\54868\NUL\v&\FSp']\132974\1036209B\SOHX\173010\GS Tc\13518\43996\1021760-6\EOTMBX#-Ad\1024507\a \b%!r"}
testObject_Name_user_11 :: Name
testObject_Name_user_11 = Name {fromName = "\\8]^'\1002849B+_\FSu\131553\49683\USz\1065509=4\DC3\DC2\111068$H\1006653\1048066\1044680f\EM1\33833\GS\NULo\36223\15877\12554\10451)I\40092\f\41659\EOT\ACK\15130jm\1013643\28774\151996\ACK`?\188210\1040590\135042s]"}
testObject_Name_user_12 :: Name
testObject_Name_user_12 = Name {fromName = "\SO\NUL9\94650)?\1006206\1103972EU\986731F\154655\aErVT/\1071165)Ijt\14620K\18630\1078841p&L#dq\11292iB\28214\n\a\189331_ \133044\74981Rw\CAN\a:\989661\1045616d\998099K\147394\&8\1056325|a\1009237\1005358rYZ\67370?\1000844\ETX\DC3\5545\32616rcjk\1050347\20974{\1037905F\18179Qrr=2\17092o[\ENQ\179392\&0JUgB\RS:\1081267\1056776\"n2\1052867\61447\986320v\1082262\&7'\1028525P_\DC4\1069447\46061qz\vuv\DLE\SUB"}
testObject_Name_user_13 :: Name
testObject_Name_user_13 = Name {fromName = "(=\fv\140396{"}
testObject_Name_user_14 :: Name
testObject_Name_user_14 = Name {fromName = "\DEL`lC'n\1060381\170635H\US\SOHg\RS\26172\1077438\1113993\CAN]\172424\6117DT^s\\D"}
testObject_Name_user_15 :: Name
testObject_Name_user_15 = Name {fromName = "-\990064j`\r"}
testObject_Name_user_16 :: Name
testObject_Name_user_16 = Name {fromName = "<\DC2%\1007969u\1098700\\\1049344\ETXy\134864$n\ttL\f\ESCk\SUB\1080943x\1601\1104093&}Ft0]\nCCK H \1076450KY*3{<\FSm\\,\60577\35591\DC3V\1006060\997295;1\19348D5$a5.\ESC\1001956g\1096434$~\SO\18381*7=@\SIPGp\SUB+x\1105209\44971l:\149555\1062436^\DC1h\SO\32574\DC2\98539\t\1009531\1104316d\"-I\1077158"}
testObject_Name_user_17 :: Name
testObject_Name_user_17 = Name {fromName = "%\1089130\a3KmHl14H\ETX\1033407\1103403MXT%\1093882\49979\184682\DLEw\98816\98295\ENQ\1075087\14526\1112830B"}
testObject_Name_user_18 :: Name
testObject_Name_user_18 = Name {fromName = ",\SOH\EMCg[\STXX\150316\STXDB}:\NAK\DC2\139253\&6c"}
testObject_Name_user_19 :: Name
testObject_Name_user_19 = Name {fromName = "\39045\ACK\SUB`g\187083yM\52693r(@nh\1078859\RS\57465\CAN\1060738\ETBI_F%k\182500\1056502!m^00o\135544\6712\1053029@\DELu\35377\NAK+L(\71424\31029\132469\\\DC1i\"\US\SYNA\52679(l\ETB\1011077\FS=U4TVc\153860@2\b\EM\DC1\NAK{\146979S\54405\&4\SOPt\1033297$r5.{\1075291KytJR1\1027609\165537P\ETX\t\DEL#I%\1099207(x-\DC1\10324_vS\1084178{8Zn\EOT\RSf\v?\98487\US\STX$0"}
testObject_Name_user_20 :: Name
testObject_Name_user_20 = Name {fromName = "\67222\41261a8\1087791\1092838\FS\DC3\RS\992141=I=a>\127391v\SOHoPB\1005217gb\CAN4\DC2R\1020494$\161881k^\989258\&8b1\SOH\23730\1018778\&2l\49112\rcN\1107375?C$\GS\\F\n5~)}0t\30740D\t\1027689!\44834\"\rk\f\174871q\1101399p\1111376\1107989\SUB5Z\66687<Hl8\1055737\987709"}
