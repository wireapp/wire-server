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
testObject_Name_1 :: Name
testObject_Name_1 = Name {fromName = "\179250Y\127098\128628Q;A\128091Ep\991633\ACK\1075681`KS\1066610\1028400\EOT3\157155\&3\STX\125220\&14c\US\1059491\ETX1c\44626):\159471\1007234\STX\EMoi\rV=\ru*TIeB\1050162\1006947\1036645\SOH&\164210zF\\L(C\t.e\22938=)\SYNsU\SI\43602\DC2y\58236Jm\1101385\162845%\33755\174342\SI:\22435\1068505"}
testObject_Name_2 :: Name
testObject_Name_2 = Name {fromName = "\18194v@8\r!\SYNU\GS\181965M#\ETX=\151796j\n\1059055t\92208{5,(f\43121\FS7\\\142106'\171356KR\1077170(_\49316\41820\ESCr.g\1092172A\1067389\SI \DLE\SYN]L\1026778\171022X\58828\SOH]6D0e/=\"]\177137\EOTA\1100469g`8\1112677\1017313\nA5Jzi\"\RS\1029041+kA%x\1098387x\1064961>~'9\13522;2\STX\DC1g\37674\DC1K\US"}
testObject_Name_3 :: Name
testObject_Name_3 = Name {fromName = "\\\ao\US\DLE\DC1|B\ETX\NAK3\ETB5\1067248\55051\129438\2667z\147735\984001\66563\&1\11429\n\1089355\1051243\NAKX\SI\10302:bA<\",\b\144856\NAKvrRJDTOWC\72100X+\1030977\DC4W\154346yhZJ]n\DEL\\q\ESC\DLEq\NAK\1091966#T\1097AeO"}
testObject_Name_4 :: Name
testObject_Name_4 = Name {fromName = "9\SYN6t\ENQ\1024216,cn\SOHi\1045234@!\SI\31245w\162703\&3\EOT=j\24823\58857\&0(\1145WDm\EM\160436d`\178861\&1E\1104584\1054657}\GS \r*c\NUL.~\NUL\DC4(rS\t\GS#h8\EM4\1000143\47899\ENQwT\99646Zxn\DLE\SI\t\"\ETX4=\1056305\1106373\DC2N\30639\1097593HD\160941We\ESC\179281T!\148945\147405lo\GS\1085875\1003267\986610\&9\1086442"}
testObject_Name_5 :: Name
testObject_Name_5 = Name {fromName = "\1039315j~YS\186556\"vX\SYN>t\CAN)d<\1034473\\\4902\1093466X\FS\CANi:I4NK\SUB^;\23088\&9a$\ENQ\1071896nLI\39074\DELu\185263\1016341\&4\CAN'o*Vd1I\1026334\SOH\EOTsg@1$\ETB3E#\17659\&9-`\b\\DCu|<"}
