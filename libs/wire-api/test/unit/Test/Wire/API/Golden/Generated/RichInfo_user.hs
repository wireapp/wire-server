{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfo_user where

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
testObject_RichInfo_user_1 :: RichInfo
testObject_RichInfo_user_1 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "U0\141026\176332h\188255\&4V\1040748qo\1109393B\23530aBC@z\CAN", richFieldValue = "F\47976Rb\ETBL\1021692<3@\161333"}]}}
testObject_RichInfo_user_2 :: RichInfo
testObject_RichInfo_user_2 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\1007875B\1034763\fWj-\FS`Jwd\DC2$W|#\30547\ETBL\27481\&5\1057974D\SI;TA\141895", richFieldValue = "\NAK\EM\NAK\ESC\SOH\r\1092901#\49169\61567\&1\ETB\n\35617T\n\119837\GS\ETB\40629\ENQ5\41008^\1028833\1090859\97707"},RichField {richFieldType = "qUz\ETXz\fc7\SUB\1068168\n\SUBdu\fBp\31396\CAN#?@\38573=\1099226\1053284\143765g`H", richFieldValue = "u$\1070255\NAK\1075394S'"},RichField {richFieldType = "R;\72222\1045633&[)+\r\13839\1026352*\52388\1005074[d\173670/$\f|Q0\1108723C2k[#", richFieldValue = "\990710\33451?\SOH\22897"},RichField {richFieldType = "\\[\998379,_&\1033518\tC", richFieldValue = "\SI\n c"},RichField {richFieldType = "\168079\95499\1059864\55033\178885ejQ\RS\SUB\DEL\ENQ_\1001428\EOT\1068602oz+\30975", richFieldValue = "M\ENQ"},RichField {richFieldType = "\DC3l\1106232)-\r):\DC2\1061719\&8}NVj\187654\21489\29869\190776\CAN\"\158012", richFieldValue = "\1007882g\DC4\42701\v_"},RichField {richFieldType = "ds\171605\43714\FS_ ", richFieldValue = "r]Y\986695J@\19633&`T\1011751\92214&;y\1106255,(_\10358\1071874_XY0Z"},RichField {richFieldType = "7#\1027226\ab* \177182J\1083862d\ESC\b#]Z*]\54158\a\182833n\1004441\1071449\1018758", richFieldValue = "}\\w\28088\ESC4\26078.7c\1108649\FS\62206\SUB_"},RichField {richFieldType = "]\SYNFm_gIY\140707F+\SYNMq\154671", richFieldValue = "mk\DC3#AM\ETBF-*G+y\1100668BqY\a\133707C"},RichField {richFieldType = "Z\SIE>\1094161^\1076645a%',wPf\f\GS))\EMG\ENQogj+3~", richFieldValue = "\139640~84nIa\f[2\1031459"},RichField {richFieldType = "R?>ZJ\74803Z6Z+)\fg0M\1021559\78364)\ETB7O\EOT?\1024602M\1032119", richFieldValue = "4;\ETB\917589ujn\1046995"},RichField {richFieldType = ":\1052293\25746", richFieldValue = "*\32894Y\996030\37178u\CAN\RSR\DC2']\147933Y\139117"},RichField {richFieldType = "\CAN\1074148", richFieldValue = "\1042529\1063964u2\162863j!J\DC2\54360B\1018206D~L6D7\1113749(K"},RichField {richFieldType = "\1018023\167800D\1003972\142561\RSf)UMu\"\1009422%L}\172550N\29214&\ENQ2xw\33416Z\1026160C", richFieldValue = "\95484.\110738\RSAF\140039okpuK\1037536jv%Ujqw)\ETXVo\1033285o"},RichField {richFieldType = "\DC4x\2752\183266G0\154079:ii8\US\RS\SI\ENQ\92261w\1061246K\138953u\1054339\&0\29751c\n\16363\&2\22421", richFieldValue = "\1108709\164652\1056275a\1042100\FSr"},RichField {richFieldType = "\ETB\tbZ\1009778\135415N\STX\993208&C\ETBE\SUB\5009\24968t\175736\59623!\SUB\153812\917832,\989175", richFieldValue = "\155026\SIac\DLE\92610P\EMy\DC1\100071^"},RichField {richFieldType = "\1065202\SOH\DC3hwj\32989\73807c", richFieldValue = "\58156X\ACK`\1050212\nN\DLE\22343S\173097\rH\SYN\ETX@f\DC25#j"},RichField {richFieldType = "PGkd\178202\DC3\NULa\SOHm\152442sR|\1024572\SUBuXJKpG\23424B", richFieldValue = "~E?)\1107672\ESC\ETX\vUt\\"},RichField {richFieldType = "\DLEZ\999572@\1025523&\1078827fk\138670c+", richFieldValue = "\ENQft\1077276\41105`hP\a1T<\133816\t\DC1\53615\GS\RS\STXC\ACKH\r"},RichField {richFieldType = "\1113897EP\1037796\&6\1100666i\EM\SYNGl%S\1015691k\996249,\66404\DC2\SOHL\DC1?i\1077056\&4!;\988530", richFieldValue = "2\FS]9\54075E\EOT}\FS"},RichField {richFieldType = "|zsHD\1043177n'\"\1088680", richFieldValue = "OKGb\1098165\DC4\NUL\t\RSbI\184060\EM.\b\148581Ee@\v\ETB/,k\8027\1050729\27794"},RichField {richFieldType = "Q\FS\1049083\ETB\44324P5{zC\FS\16014'b3'\GS", richFieldValue = "\51520f/y\68447\994034E[p\USn"},RichField {richFieldType = "a\6714>;\1062980.=\1010624\GS\1047237x\1059936\ENQI\63180<\1050706lh^S", richFieldValue = "]\1097191\v3M\1008662#\SUB>1 \ac"},RichField {richFieldType = "2\1018678\EM#\EM\1074852\n\"[\999197KV", richFieldValue = "5\120191<#\1089245\182332\r\1038863h"},RichField {richFieldType = "\1028663\ETB_3_\DC2X\SUB\1098630s\DC2Hc{TYW\NAK\ESC\1100270[\DC4b\184288<;\22073T\178627!", richFieldValue = "BI\v\1050901\SIu3\1021855&\986281\ACKc'_w=\1096732\&3s7\EMd\DC1b"},RichField {richFieldType = "`\SUB\n\1063123j\DC3Q#i \12803P_*8[#Hl\SIk\48851", richFieldValue = "1> '\EM4'\SYN:["}]}}
testObject_RichInfo_user_3 :: RichInfo
testObject_RichInfo_user_3 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\DC3\147584O\\j\185861", richFieldValue = "|/)\996860dCe\DC3U\DLE\1027790IhND\t\DC2d\v\DLE\DEL"},RichField {richFieldType = "", richFieldValue = "\167756\ahC+#\4674\&4\989587\FS?\1041594`\1032345\ESC"},RichField {richFieldType = "[!\173634!R\SI{\59782\&8v\1013563{\46525HuL\DLE\133892x:\62547v\1053413\GSI//b\NUL#", richFieldValue = "\1034630\STX\1065477\144189\FS\DC4D \1013519\DELsx\60259\1091574F`\NAKnj\v1+SS}J\1048651psd"},RichField {richFieldType = "\ETB\51394\RS-%\188175", richFieldValue = "\SOCA\98760\"\1019807htV\RSl"},RichField {richFieldType = "Q\CAN\148481\STX\1001588\EM\ETX\EMz\181702m\STXN\1061595\13301\179926\n\n]\SI", richFieldValue = "=\1054037"},RichField {richFieldType = "wmA/\1012498\59791\172579!\ENQ-\1000305]\ETB{\48545\DC4\SYNvzo\189447\1011269NMU\1102817\172925nS", richFieldValue = "\EOTN\DC4vk\28629_\92677\1075193\ENQf\166049\40488P"},RichField {richFieldType = "e\1007023lLg\SIc\NAK\14743s}\RSp\1100002\&6\SUB", richFieldValue = "\1102850N+\1060312\156835j2\1086047u\NUL\ETBt?\CAN x=\1080553%2\26499\1053111*\169491\984475{"},RichField {richFieldType = "!\v\183363\FS\1097880n", richFieldValue = "$qA\13434\DEL^w#D&g\1082595\v\1101015\136426G"},RichField {richFieldType = "\GS\997787\1010065\185640", richFieldValue = "\138946PDldOume;\137305l\SO\FS \131930(\SYNg>x`O"},RichField {richFieldType = "n\SUB\SI\160135\29482\SUB0}J\1036010~eCkOHgXBQ-\43046m0", richFieldValue = "\1008123CP\171381v?\SOn2i\1028505<l F\FSU\1094170\1035041"},RichField {richFieldType = "\178089,\13149", richFieldValue = "^'\SI\172005o3\SOH<\SOH\DEL"},RichField {richFieldType = "\CAN", richFieldValue = "|*\NUL\45652\1071305\DC1oH\"j\FSjF<9g\189095\42072W"},RichField {richFieldType = "\30360\61557sg\SO", richFieldValue = "\1031561m\n;t}\1011571\996115\9375A\1049433KDm\48003K\"J\34981?S:K\EM\RS\r1\ACK}"},RichField {richFieldType = "\US.;yJ", richFieldValue = "\179191F\1023339\CAN\78271\70425_\ACK\SOH\CAN3_=\SYN\1007295\1068340\1034646\DC1]'g\SOps\1012249m\998246\GSa["},RichField {richFieldType = "\DC4\NAKp\EOT\24002]\DLEd;\ETBf\1039039\128122\38603\168429\145614lxeMf\EOT\1046426+", richFieldValue = "C[U\v\1012855wmz3\1085219MOSvy\1053461I@"}]}}
testObject_RichInfo_user_4 :: RichInfo
testObject_RichInfo_user_4 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "", richFieldValue = "T1"},RichField {richFieldType = "/\DEL\97566q\128805B\DC28c\1101939vp6qd&\1005166\NUL`\1031819f", richFieldValue = "_\1099037\SO\"\txW"},RichField {richFieldType = "\137085\1016756\92166\1054003\aN\49580y>Kf\SI\1004732jo\\X\138816\120303", richFieldValue = " {"},RichField {richFieldType = "\ETB}62\49051dRj\SOHBk\v\STX\1091296{=", richFieldValue = "\1019045\96938WOq ltT\38968\vU3po5\DC4\EMp\ACK;X\1108733\&4\1014987\&2\DELG"},RichField {richFieldType = "%m", richFieldValue = "\b\ETX\999637\DEL\SO"},RichField {richFieldType = "J\190674\1071678\&9\SI\RS\1108824\ENQp-\SUB", richFieldValue = "\DC4\173775\SYN6sN\RS>\134079s\169352<\GS\US=\986135\69240i\1108984j\FS\14443\"5Gp"},RichField {richFieldType = "\DC3A\SOH\1093100\n\51706\v^\ETB", richFieldValue = "\1060410\&2]\r6y/T6m:$\ETXU\138248\DEL*"},RichField {richFieldType = "\39107r\1101010EO\37387h\fc/\ESCd\1081099'\\\aH\175790>X;\63802\DC1DB?\30749\NAK\175640I", richFieldValue = "\1016029UP\1017991P\1057849r\RSw@k"}]}}
testObject_RichInfo_user_5 :: RichInfo
testObject_RichInfo_user_5 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "w$\CAN\39219\20101\14822C)", richFieldValue = "mA\1063629f6\99894s.}g\CANN\1111572\ACKl \993700.\ETB\134590\61944\SYNcu,_\DC3Uc"},RichField {richFieldType = "\19672d \DC4H$2\15210j.X\1034533\1014957jly\ACK\1042824", richFieldValue = "\DC2Q\ETX9\60427\1034859\1092146\SO>l%Y\NAK\EOT\RS\158132\23162\1005585q\188301\183646|"},RichField {richFieldType = "\r\ETX$\185952E~\172950\vx\147151\CANv\1039401\31318I\ETXr", richFieldValue = "^\1019610(=\STXbJ\180087V\1049476l/\22328\&8Z"}]}}
