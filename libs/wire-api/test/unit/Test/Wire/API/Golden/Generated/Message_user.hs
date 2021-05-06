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
testObject_Message_user_1 = Message {messageText = "\ESC\ESC,'7\1087315>#7r&\1018595\1006728\57811.V[e:\1097245\SOH\SI\EOT}\29518j\US\10871\&9CX\1135#C\67401l`84\1078975j\30172\7351r`\DLE,F\DC4:ug\135833\NUL\994880\1059377:\CANL\1032207A3j\ACK^\ETX\119983\EM\EOT\1054117\168510\CAN\RS'Ct\ESC\1023717\&9\SYN<(\v)\r\v\f7/}\tSw\GS\1075644\180455L\b\43675Lpa\FS4\SOL\163385&xDSz;2\162112\1050634"}
testObject_Message_user_2 :: Message
testObject_Message_user_2 = Message {messageText = "/4+\187797\1049215\1048973\DC1\SOHPm\36457W{30rX\NAK\1041784\&6\DC2C#WJ\1054689}\SYN! \917863+\99856c-(D|G@M>W,\SYNe\rn%$2Z\1018459w&9:~Yl4~\DC2\1030924+\152220toF\r;s,I\bA\1100736!vf=yl?\"\STX0\1022057\FSG^<\1051586zY\".\69450Iiv<T5OcH4$\1021409\22227^\1012019d%\1069371|\988192m[5\165113mr\1056747n8K&\51316\n=;\1102184}5\137955|\1036367\f`;wL\1076059-\1069411\1077355\DC4"}
testObject_Message_user_3 :: Message
testObject_Message_user_3 = Message {messageText = "\US\n&~q>\1107963NW\EM\1032000wN\46902\1079335.i\SI\NAK\US-\1112458:J\162691Y\131251a\CAN\1007420\37399\ESCQ\DC4-\EM2f"}
testObject_Message_user_4 :: Message
testObject_Message_user_4 = Message {messageText = "\DC4Z\aiq.H\no+[\986455K\ENQHW\61774\&0+?\182931\"V;\1098941}\34453'\n\SYN+g\58255K\992954\142715\1081032\3848o\ETB\28319\SI\RS_~=\180459:9s\r\td\EM\1095822\US\1089262u6\1052556m\1021858x;\v\DC28\162716\RSg\ESC-qq \12990\DEL)\ETX\DC1c}&\v{\123164\EM\ENQU\1042124\&3'6\1032815?E\1003172\EOT\1001714\DC4\t\150426I.&\r {=7\DC2\1016583\&6bX{3\98228a\SUB\162661\&7\1069937\&5\1009164`{{:\1040461pd\v,gq\1015339\33443n\60043/\50104e\990512\1027205<\NUL\63412*md`F<\GSe\1112700DtMx\vT\DC2\1019474\RS\"\DEL\ESC\DC4F\DC1=!-\188117\GS7\1093767W\988294\144644\160206'f$=\1102220\1113802\DEL$=\DC4}<(\94443\v\SYNr\RST tyf+\1050407w\1071134\CAN"}
testObject_Message_user_5 :: Message
testObject_Message_user_5 = Message {messageText = "J6,P7LV\175841Pe\GS\\s)\vr\DC3\987124?\137227\917590az_!\175141\&4<\1014326\&5*$\NUL\SYNf\72276<\1086668P\SI\71110\173013UWe\4590-\DEL+}(\9320Q\66626;lV0\FS\ENQ\18976\13683\NAK-\175405\SOH\"\156354\19657\46838\DC2\STX9\SUBWL\187338]\182714\1047793\1030228\ETB\1053703b\1034638\33369r!\SOH\US.Nv\1005403\1010428{;9\DC4)\DC4E\1092393\163587t<TC\CAN\1074604&J\RS\66026\1084281\22198M\SOi\95051\1026682\173981_\144700\DC2}\179513+~)E\1104325;v^[K\SOH0g#I\1101452x'n\NAK;<FJ\61113gm0\DC4;n \n\SUB@8##iCZ\1108741qb\29084\t\1001406\STX\FS\1014419c'\38371%.f\DC3\USrdG\STXf]l\ETXV\ESC:^ZF4~E;\1047169\STX\1096144\DLEa*\CAN4\1110227w"}
