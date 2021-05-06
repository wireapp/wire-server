{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Email_user where

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
testObject_Email_user_1 :: Email
testObject_Email_user_1 = Email {emailLocal = "${\34284\SI\ENQ\1097126G\SOHLR\1113095'\b\1006769L\f,\135815\STXF\74111\997628", emailDomain = ":X5\9187U\1047708\DC1\1083827szI-\US"}
testObject_Email_user_2 :: Email
testObject_Email_user_2 = Email {emailLocal = "\v\a>\NUL8\1050106", emailDomain = "?\FS~:ji\8211\ENQ\45619\1112351\&8XY\ENQhu\167893E"}
testObject_Email_user_3 :: Email
testObject_Email_user_3 = Email {emailLocal = "\24340|\983633\ETBy\ENQr\FS\118830/\r", emailDomain = "p\1113290g7\179265$5}\54574QM\175857\&2Z?\16907"}
testObject_Email_user_4 :: Email
testObject_Email_user_4 = Email {emailLocal = "\138242_\71055\1039637=\DELV<6\SOHcS|Dr\1062787`~\1090470gGvv?\20532r", emailDomain = "={/g\135923SS\1035845h]\1030004\SOH"}
testObject_Email_user_5 :: Email
testObject_Email_user_5 = Email {emailLocal = "p,\DELi>$\NAK\1060320\NUL\ENQD\DLE\b\1085547\&5", emailDomain = "\1112539C\34982p\1046495y\\\1098102\ETB-;\\\ETBX\DC1rCMp\78091\ETX\66588"}
testObject_Email_user_6 :: Email
testObject_Email_user_6 = Email {emailLocal = "E\45571L^3:)H\SYN\FS8}\EM|\bpo\f8\\#\1085849<s\134493g-\3976", emailDomain = "m\1412\161192j\53340X\DLEu\SUB"}
testObject_Email_user_7 :: Email
testObject_Email_user_7 = Email {emailLocal = "\STX\DC1'e_\1027787t\1068380\DC2IN+\SYN&~(\1045672\a\ETX\1012095t\"W#\GSsO_6\SYN", emailDomain = "\158452"}
testObject_Email_user_8 :: Email
testObject_Email_user_8 = Email {emailLocal = "\1026782\US\62131\1003376", emailDomain = "a\131648\194907;\DC1vZKV{R\120473#\990659"}
testObject_Email_user_9 :: Email
testObject_Email_user_9 = Email {emailLocal = "\1009037\11539C\SI3x\42568P", emailDomain = "O\1061641NT|\1053778$S1\993323Y\DC40/m\19752"}
testObject_Email_user_10 :: Email
testObject_Email_user_10 = Email {emailLocal = "\1003764\al~C", emailDomain = "9d"}
testObject_Email_user_11 :: Email
testObject_Email_user_11 = Email {emailLocal = ";CA\GS;o'\NAKIQ\ENQ", emailDomain = "\1112417\1002932yu\1084272\ESC\tJ\1076496\190451\DC4\GSc-X\v\30372\&8a#\153276w\STX\EM\DC4?\ENQ\181295"}
testObject_Email_user_12 :: Email
testObject_Email_user_12 = Email {emailLocal = "\SOH\SOJ\142697>\14605u^\ACK\1027768VYo\ESCc\ACK\34689\41279\1042280\59060QI\DEL\18737'\55150\n;\19887", emailDomain = "\147302"}
testObject_Email_user_13 :: Email
testObject_Email_user_13 = Email {emailLocal = "", emailDomain = "D>\CAN\ESCrGuZWc\DC3M\CANl\43411&KO\SOH\1056185mbTJ\GS\1039940(\136708\1056803\97470"}
testObject_Email_user_14 :: Email
testObject_Email_user_14 = Email {emailLocal = "\36213\SO\1010181#\1098606q\NAK", emailDomain = ""}
testObject_Email_user_15 :: Email
testObject_Email_user_15 = Email {emailLocal = "eP/", emailDomain = "\ETX\27826Rj\GS!e"}
testObject_Email_user_16 :: Email
testObject_Email_user_16 = Email {emailLocal = "\1099897\1042401~\1090663\1044505\24806Hs\157578o1", emailDomain = "\CAN\STX\1088710&(\36548v/jM\1001026\&7;\182563:"}
testObject_Email_user_17 :: Email
testObject_Email_user_17 = Email {emailLocal = "\57458\&5", emailDomain = "&\1086130\ESCos\184654S7G^"}
testObject_Email_user_18 :: Email
testObject_Email_user_18 = Email {emailLocal = "\DC3\1056999\30818u,U3R\36187#2", emailDomain = ""}
testObject_Email_user_19 :: Email
testObject_Email_user_19 = Email {emailLocal = "\156158A\r\158942%\RS6\STXK:\70062l\163454\NAK\14540\&2i'Benb\1091579", emailDomain = "3 "}
testObject_Email_user_20 :: Email
testObject_Email_user_20 = Email {emailLocal = "5\1038908\1090563\1089482\131170O\64129\GSQ/", emailDomain = "\ESCZ\CAN\20317\151090\EOT\DC1\172429=|\1101898\185968\a"}
